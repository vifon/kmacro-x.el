;;; kmacro-x-mc.el --- Multiple cursors implemented with kmacros  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Wojciech Siewierski

;; Author: Wojciech Siewierski
;; URL: https://github.com/vifon/kmacro-x.el
;; Keywords: convenience
;; Version: 0.9

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Efficient multiple cursors implemented using keyboard macros.

;; Loosely inspired by https://github.com/corytertel/macrursors and
;; obviously https://github.com/magnars/multiple-cursors.el

;;; Code:

(require 'kmacro-x)

(defgroup kmacro-x-mc nil
  "Multiple cursors implemented with keyboard macros."
  :group 'kmacro-x)

(defvar kmacro-x-mc-mode)

(defface kmacro-x-mc-cursor-face
  '((t (:inverse-video t)))
  "The face used for the fake cursors created in `kmacro-x-mc-mode'.")

(defface kmacro-x-mc-main-cursor-face
  '((t (:underline t)))
  "The face used for the original cursor when using `kmacro-x-mc-mode'.")

(defcustom kmacro-x-mc-mark-whole-symbol nil
  "When invoking `kmacro-x-mc-mark-next' with no region, mark the whole symbol.

If nil, keep point in place and put the mark at the end of the symbol.

If t, move the point to the end of the symbol and put the mark at its beginning.

Example, with | being the point and ^ being the mark:

    exam|ple-symbol
                   ^

    example-symbol|
    ^"
  :type 'boolean)

(defcustom kmacro-x-mc-preserve-last-macro t
  "If non-nil, preserve the previous value of `last-kbd-macro'.

If non-nil, `kmacro-x-mc-mode' saves the user's own `last-kbd-macro' to
restore it later.  The macro recorded by `kmacro-x-mc-mode' is then
discarded.  This allows the user to ignore the inner workings of
`kmacro-x-mc-mode' as the plain keyboard macros are not disturbed.

If nil, `kmacro-x-mc-mode' behaves like a regular keyboard macro and
keeps the recorded macro in `last-kbd-macro'.  The previous value is
pushed on `kmacro-ring'.  The macro recorded by `kmacro-x-mc-mode' can
then be reused."
  :type 'boolean)

(defcustom kmacro-x-mc-pre-apply-hook nil
  "Functions to run before applying the recorded keyboard macro to cursors."
  :type 'hook)

(defcustom kmacro-x-mc-post-apply-hook nil
  "Functions to run after applying the recorded keyboard macro to cursors."
  :type 'hook)

(defcustom kmacro-x-mc-live-preview nil
  "Experimental live previews of the edits performed within the cursors.

Changes performed outside of the cursors are not displayed, but
will be replicated as usual.

Turning this on makes it no longer possible to move the cursor
onto the fake cursors / selections.  While technically a bug,
this scenario is so rare and so difficult to get right (from the
user's perspective), this is just acknowledged here."
  :type 'boolean)

(defvar-local kmacro-x-mc-regexp nil
  "A regexp matching the intended cursor positions.")

(defvar-local kmacro-x-mc-cursors nil
  "The overlays for displaying and keeping the cursor positions.

`kmacro-x-mc-mode' initializes it with the original cursor's
overlay which can be recognized by it having its `face' property
set to `kmacro-x-mc-main-cursor-face'.  The other (\"fake\")
cursors use `kmacro-x-mc-cursor-face' instead.")

(defvar-local kmacro-x-mc-main-cursor nil
  "The overlay for the original cursor.

Duplicated from `kmacro-x-mc-cursors' so it can be accessed at
all times without searching through this list.")


(defun kmacro-x-mc--modification-hook-live-preview
    (ov after-p _beg _end &optional _length)
  "Overlay modification-hook updating all the other overlays.

OV and AFTER-P (and other ignored arguments) are passed according
to the modification-hook overlay API."
  (when after-p
    (dolist (cursor kmacro-x-mc-cursors)
      (unless (kmacro-x-mc--main-cursor-p cursor)
        (overlay-put cursor 'display
                     (buffer-substring (overlay-start ov)
                                       (overlay-end ov)))))))

(defun kmacro-x-mc--make-main-cursor (beg end)
  "Create an overlay representing the main cursor from BEG to END."

  ;; With `kmacro-x-mc-live-preview' inverting the *-advance arguments
  ;; of `make-overlay' it's possible to use the overlay
  ;; modification-hooks to capture the edits at the cursor boundaries.
  ;; With `kmacro-x-mc-live-preview' being nil, the difference is only
  ;; visual and this setting here is subjectively the most visually
  ;; pleasing one (according to the author).
  (let* ((front-advance (not kmacro-x-mc-live-preview))
         (rear-advance kmacro-x-mc-live-preview)
         (ov (make-overlay beg end nil front-advance rear-advance)))
    (overlay-put ov 'face 'kmacro-x-mc-main-cursor-face)
    (overlay-put ov 'offsets (cons (- (point) beg)
                                   (- (mark) beg)))
    (when kmacro-x-mc-live-preview
      (let ((hooks (list #'kmacro-x-mc--modification-hook-live-preview)))
        (overlay-put ov 'modification-hooks hooks)
        (overlay-put ov 'insert-in-front-hooks hooks)
        (overlay-put ov 'insert-behind-hooks hooks)))
    ov))

(defun kmacro-x-mc--make-cursor (beg end)
  "Create an overlay representing a fake cursor from BEG to END."
  (let ((ov (make-overlay beg end nil t nil)))
    (overlay-put ov 'face 'kmacro-x-mc-cursor-face)

    ;; Store the offsets per cursor as mouse-created cursors
    ;; have different offsets.
    (overlay-put ov 'offsets
                 (overlay-get kmacro-x-mc-main-cursor 'offsets))

    ;; Store whether the region should be active for this
    ;; cursor.  This allows to properly replicate the
    ;; region-sensitive commands such as using `DEL' to delete
    ;; the whole region without affecting the `kill-ring' (like
    ;; `C-w' would).
    (overlay-put ov 'region-active (region-active-p))

    ov))

(defun kmacro-x-mc--mark (prefix &optional backwards)
  "Create a new fake cursor for `kmacro-x-mc-mode'.

Used internally by `kmacro-x-mc-mark-next' and
`kmacro-x-mc-mark-previous'.

With the PREFIX argument, replace the last cursor and prefix it
with the new one, essentially skipping one occurence of a match.

If `kmacro-x-mc-mode' isn't enabled yet, PREFIX instead inverts
the `kmacro-x-mc-mark-whole-symbol' setting.

Enables `kmacro-x-mc-mode' if not enabled yet, for the necessary
initialization such as setting `kmacro-x-mc-regexp',
`kmacro-x-mc-cursors' and `kmacro-x-mc-main-cursor'.

If BACKWARDS is non-nil, searches backwards.
Otherwise searches forward."

  ;; Sanity check to prevent defining cursors with inconsistent
  ;; starting conditions.
  (when (and kmacro-x-mc-mode
             (not (memq last-command '(kmacro-x-mc-mark-next
                                       kmacro-x-mc-mark-previous))))
    (user-error "This command can only be used when starting a bulk edit"))

  (unless kmacro-x-mc-mode
    (let ((kmacro-x-mc-mark-whole-symbol
           ;; On the first call, use `prefix' to invert the
           ;; `kmacro-x-mc-mark-whole-symbol' setting and then set it
           ;; to nil to disable any behavior it would enable on the
           ;; subsequent calls instead.
           (if prefix
               (prog1
                   (not kmacro-x-mc-mark-whole-symbol)
                 (setq prefix nil))
             kmacro-x-mc-mark-whole-symbol)))
      (kmacro-x-mc-mode 1)))

  ;; This function is always being called while a macro is being
  ;; recorded, but we do not want it to become a part of the macro.
  (cancel-kbd-macro-events)

  (save-excursion
    ;; Start the search right after/behind the last cursor.
    ;; The original cursor is the "zeroth" cursor, so the list is
    ;; never empty and both appending and prepending to it makes sense
    ;; at all times once `kmacro-x-mc-mode' is active (it initializes
    ;; it with the original cursor).
    (let ((ov (car (if backwards
                       kmacro-x-mc-cursors
                     (last kmacro-x-mc-cursors)))))
      (goto-char (if backwards
                     (overlay-start ov)
                   (overlay-end ov))))

    ;; Create a new cursor.
    (if (let ((case-fold-search nil))
          (if backwards
              (search-backward-regexp kmacro-x-mc-regexp nil 'noerror)
            (search-forward-regexp kmacro-x-mc-regexp nil 'noerror)))
        (let ((ov (kmacro-x-mc--make-cursor (match-beginning 0)
                                            (match-end 0))))
          (if prefix
              ;; Replace the first or last cursor with the new one.
              (let ((old-cursor (if backwards
                                    kmacro-x-mc-cursors
                                  (last kmacro-x-mc-cursors))))
                (delete-overlay (car old-cursor))
                (setcar old-cursor ov))
            ;; Either append or prepend the new cursor.
            (if backwards
                (push ov kmacro-x-mc-cursors)
              (setq kmacro-x-mc-cursors
                    (append kmacro-x-mc-cursors (list ov))))))
      (message "No further matches"))))

;;;###autoload
(defun kmacro-x-mc-mark-next (&optional prefix)
  "Create a new fake cursor forward.

When region is active, create the cursor at the next occurrence
of the selection.  When region is inactive, look for the symbol
at point instead.

With the PREFIX argument, replace the last cursor and prefix it
with the new one, essentially skipping one occurence of a match.

If `kmacro-x-mc-mode' isn't enabled yet, PREFIX instead inverts
the `kmacro-x-mc-mark-whole-symbol' setting.

Activates `kmacro-x-mc-mode' with its keymap being used to either
apply or abort the bulk edit.

See also: `kmacro-x-mc-mark-previous'"
  (interactive "P")
  (kmacro-x-mc--mark prefix))

;;;###autoload
(defun kmacro-x-mc-mark-previous (&optional prefix)
  "Create a new fake cursor backwards.

See `kmacro-x-mc-mark-next' for the details and the PREFIX
argument behavior."
  (interactive "P")
  (kmacro-x-mc--mark prefix 'backwards))

(defun kmacro-x-mc--main-cursor-p (cursor)
  "Check whether CURSOR is the main cursor and not a fake one."
  (eq (overlay-get cursor 'face)
      'kmacro-x-mc-main-cursor-face))

(defun kmacro-x-mc--apply-cursor (cursor)
  "Apply the recorded macro to CURSOR (from `kmacro-x-mc-cursors').

CURSOR is internally an overlay."
  (overlay-put cursor 'display nil)
  (goto-char (+ (overlay-start cursor)
                (car (overlay-get cursor 'offsets))))
  (push-mark (+ (overlay-start cursor)
                (cdr (overlay-get cursor 'offsets))))
  (when (overlay-get cursor 'region-active)
    (activate-mark))
  (call-last-kbd-macro))

(defun kmacro-x-mc-apply ()
  "Apply the recoded macro for each cursor."
  (interactive)
  (when defining-kbd-macro
    (end-kbd-macro))
  (run-hooks 'kmacro-x-mc-pre-apply-hook)
  (let ((marker (point-marker)))
    (dolist (ov kmacro-x-mc-cursors)
      (unless (kmacro-x-mc--main-cursor-p ov)
        (kmacro-x-mc--apply-cursor ov)))
    ;; Push the original point to the `mark-ring', so it's easy to
    ;; return there if needed.
    (push-mark marker)
    (set-marker marker nil))
  (run-hooks 'kmacro-x-mc-post-apply-hook)
  (kmacro-x-mc-mode 0))

(defun kmacro-x-mc-apply-one ()
  "Apply the recoded macro to one fake cursor.

Keeps `kmacro-x-mc-mode' active to allow calling it more times,
but stops the macro recording.

Use `kmacro-x-mc-apply' to apply the macro to all the
remaining cursors."
  (interactive)
  (when defining-kbd-macro
    (end-kbd-macro))
  (catch 'cursor
    (dolist (ov kmacro-x-mc-cursors)
      (unless (kmacro-x-mc--main-cursor-p ov)
        (kmacro-x-mc--apply-cursor ov)
        (delete-overlay ov)
        (setq kmacro-x-mc-cursors
              (delete ov kmacro-x-mc-cursors))
        (throw 'cursor ov)))))

(defun kmacro-x-mc-quit ()
  "Cancel the macro recording, disable `kmacro-x-mc-mode'.

If region is active, merely deactivate it instead."
  (interactive)
  (if (region-active-p)
      (deactivate-mark)
    (kmacro-x-mc-mode 0)

    ;; Make sure the macro recording is cancelled.  Since this command
    ;; is bound to `C-g', it might be a good idea to call the original
    ;; `C-g' command anyway.
    (keyboard-quit)))


(defvar kmacro-x-mc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") #'kmacro-x-mc-quit)
    (define-key map (kbd "RET") #'kmacro-x-mc-apply)
    (define-key map (kbd "M-RET") #'kmacro-x-mc-apply-one)
    map))

(defvar kmacro-x-mc--bounds-override nil
  "An alternate boundaries of the edited text element.

Unless it's non-nil, the boundaries of the symbol at point are
used.  This variable can override this behavior in commands using
different semantics, for instance mouse-placed cursors.")

(define-minor-mode kmacro-x-mc-mode
  "Record a keyboard macro to apply with multiple cursors.

Usually invoked from `kmacro-x-mc-mark-next', not by the
user directory.

\\{kmacro-x-mc-mode-map}"
  :lighter " kmacro-mc"
  (if kmacro-x-mc-mode
      (let ((bounds (if (use-region-p)
                        (cons (region-beginning) (region-end))
                      (or kmacro-x-mc--bounds-override
                          (bounds-of-thing-at-point 'symbol)))))

        (let ((regexp (regexp-quote
                       (buffer-substring-no-properties
                        (car bounds)
                        (cdr bounds)))))
          (setq-local kmacro-x-mc-regexp
                      (if (use-region-p)
                          regexp
                        (concat "\\_<" regexp "\\_>"))))

        (unless (use-region-p)
          (if kmacro-x-mc-mark-whole-symbol
              (progn
                (push-mark (car bounds))
                (goto-char (cdr bounds)))
            ;; The end of the symbol is as good of a place as any for
            ;; the mark.  Definitely better than whatever random
            ;; position it was before.
            (push-mark (cdr bounds))))

        (let ((ov (kmacro-x-mc--make-main-cursor (car bounds)
                                                 (cdr bounds))))
          (setq-local kmacro-x-mc-main-cursor ov)
          (setq-local kmacro-x-mc-cursors (list ov)))

        (push `(kmacro-x-mc-mode . ,kmacro-x-mc-mode-map)
              minor-mode-overriding-map-alist)

        (when kmacro-x-mc-preserve-last-macro
          ;; Save the user's own `last-kbd-macro' and the associated
          ;; counter to restore them later.  When `last-kbd-macro' is
          ;; nil, `kmacro-ring' isn't modified, no extra handling
          ;; is needed.
          (setq-local kmacro-x-mc-stored-last-macro (kmacro-ring-head))
          (setq last-kbd-macro nil))

        (start-kbd-macro nil))

    (setq minor-mode-overriding-map-alist
          (assq-delete-all #'kmacro-x-mc-mode
                           minor-mode-overriding-map-alist))

    (mapc #'delete-overlay kmacro-x-mc-cursors)
    (kill-local-variable 'kmacro-x-mc-regexp)
    (kill-local-variable 'kmacro-x-mc-main-cursor)
    (kill-local-variable 'kmacro-x-mc-cursors)

    (when (bound-and-true-p kmacro-x-mc-stored-last-macro)
      ;; Restore the previous value of `last-kbd-macro' and its counter.
      (kmacro-split-ring-element kmacro-x-mc-stored-last-macro)
      (kill-local-variable 'kmacro-x-mc-stored-last-macro))))


(defvar-local kmacro-x-mc-change-group nil)

(defun kmacro-x-mc-undo-amalgamate-advice (&rest _)
  "Amalgamate all the edits created with `kmacro-x-mc-mode' active."
  (if kmacro-x-mc-mode
      (progn
        (undo-boundary)
        (setq-local kmacro-x-mc-change-group (prepare-change-group))
        (activate-change-group kmacro-x-mc-change-group))
    (accept-change-group kmacro-x-mc-change-group)
    (undo-amalgamate-change-group kmacro-x-mc-change-group)
    (kill-local-variable 'kmacro-x-mc-change-group)))

;;;###autoload
(define-minor-mode kmacro-x-mc-atomic-undo-mode
  "Undo the whole `kmacro-x-mc-mode' bulk operation at once."
  :global t
  :require 'kmacro-x-mc
  (if kmacro-x-mc-atomic-undo-mode
      (advice-add #'kmacro-x-mc-mode :after
                  #'kmacro-x-mc-undo-amalgamate-advice)
    (advice-remove #'kmacro-x-mc-apply
                   #'kmacro-x-mc-undo-amalgamate-advice)))

(provide 'kmacro-x-mc)
;;; kmacro-x-mc.el ends here
