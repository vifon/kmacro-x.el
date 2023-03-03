;;; kmacro-x-mc.el --- Multiple cursors implemented with kmacros  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Wojciech Siewierski

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
    ^
"
  :type 'boolean)

(defcustom kmacro-x-mc-pre-apply-hook nil
  "Functions to run before applying the recorded keyboard macro to cursors."
  :type 'hook)

(defcustom kmacro-x-mc-post-apply-hook nil
  "Functions to run after applying the recorded keyboard macro to cursors."
  :type 'hook)

(defvar-local kmacro-x-mc-regexp nil
  "A regexp matching the intended cursor positions.")

(defvar-local kmacro-x-mc-offsets '(nil . nil)
  "The cons with offsets of the point and the mark within the selection.")

(defvar-local kmacro-x-mc-cursors nil
  "The overlays for displaying and keeping the cursor positions.")

(defun kmacro-x-mc--mark (prefix &optional backwards)
  "Create a new fake cursor for `kmacro-x-mc-mode'.

Used internally by `kmacro-x-mc-mark-next' and
`kmacro-x-mc-mark-previous'.

With the PREFIX argument, replace the last cursor and prefix it
with the new one, essentially skipping one occurence of a match.

If `kmacro-x-mc-mode' isn't enabled yet, PREFIX instead inverts
the `kmacro-x-mc-mark-whole-symbol' setting.

Enables `kmacro-x-mc-mode' if not enabled yet, for the
necessary initialization."

  ;; Sanity check to prevent defining cursors with inconsistent
  ;; starting conditions.
  (when (and kmacro-x-mc-mode
             (not (memq last-command '(kmacro-x-mc-mark-next
                                       kmacro-x-mc-mark-previous))))
    (user-error "This command can only be used when starting a bulk edit"))

  (unless kmacro-x-mc-mode
    (let ((kmacro-x-mc-mark-whole-symbol
           ;; On the first call, use `prefix' to invert the
           ;; `kmacro-x-mc-mark-whole-symbol' setting.
           (if prefix
               (not kmacro-x-mc-mark-whole-symbol)
             kmacro-x-mc-mark-whole-symbol)))
      (kmacro-x-mc-mode 1)))

  ;; This function is always being called while a macro is being
  ;; recorded, but we do not want it to become a part of the macro.
  (cancel-kbd-macro-events)

  (save-excursion
    ;; Start the search right after/behind the last cursor.
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
        (let ((ov (make-overlay (match-beginning 0)
                                (match-end 0))))
          (overlay-put ov 'face 'kmacro-x-mc-cursor-face)

          ;; Store the offsets per cursor as mouse-created cursors
          ;; have different offsets.
          (overlay-put ov 'offsets kmacro-x-mc-offsets)

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
              (setq kmacro-x-mc-cursors (append kmacro-x-mc-cursors (list ov))))))
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

See `kmacro-x-mc-mark-next' for the details."
  (interactive "P")
  (kmacro-x-mc--mark prefix 'backwards))

(defun kmacro-x-mc-apply ()
  "Apply the recoded macro for each cursor."
  (interactive)
  (end-kbd-macro)
  (run-hooks 'kmacro-x-mc-pre-apply-hook)
  (dolist (ov kmacro-x-mc-cursors)
    (unless (eq (overlay-get ov 'face) 'kmacro-x-mc-main-cursor-face)
      (goto-char (+ (overlay-start ov)
                    (car (overlay-get ov 'offsets))))
      (push-mark (+ (overlay-start ov)
                    (cdr (overlay-get ov 'offsets))))
      (call-last-kbd-macro)))
  (run-hooks 'kmacro-x-mc-post-apply-hook)
  (kmacro-x-mc-mode 0))

(defun kmacro-x-mc-quit ()
  "Cancel the macro recording, disable `kmacro-x-mc-mode'.

If region is active, merely deactivate it instead.  This event is
omitted from the recorded macro to prevent premature termination."
  (interactive)
  (cancel-kbd-macro-events)
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
    map))

(define-minor-mode kmacro-x-mc-mode
  "Record a keyboard macro to apply with multiple cursors.

Usually invoked from `kmacro-x-mc-mark-next', not by the
user directory.

\\{kmacro-x-mc-mode-map}"
  :lighter " kmacro-mc"
  (if kmacro-x-mc-mode
      (let ((bounds (if (use-region-p)
                        (cons (region-beginning) (region-end))
                      (or (bounds-of-thing-at-point 'symbol)
                          ;; A technically correct fallback.
                          ;; Relevant mostly for the alternative ways to
                          ;; create cursors such as mouse clicks.
                          (cons (point) (1+ (point)))))))

        (let ((regexp (regexp-quote
                       (buffer-substring-no-properties
                        (car bounds)
                        (cdr bounds)))))
          (setq-local kmacro-x-mc-regexp
                      (if (use-region-p)
                          regexp
                        (concat "\\_<" regexp "\\_>"))))

        (let ((ov (make-overlay (car bounds) (cdr bounds))))
          (overlay-put ov 'face 'kmacro-x-mc-main-cursor-face)
          (setq-local kmacro-x-mc-cursors (list ov)))

        (unless (use-region-p)
          (if kmacro-x-mc-mark-whole-symbol
              (progn
                (push-mark (car bounds))
                (goto-char (cdr bounds)))
            ;; The end of the symbol is as good of a place as any for
            ;; the mark.  Definitely better than whatever random
            ;; position it was before.
            (push-mark (cdr bounds))))

        (setq-local kmacro-x-mc-offsets
                    (cons (- (point)
                             (car bounds))
                          (- (mark)
                             (car bounds))))

        (push `(kmacro-x-mc-mode . ,kmacro-x-mc-mode-map)
              minor-mode-overriding-map-alist)

        (start-kbd-macro nil))

    (setq minor-mode-overriding-map-alist
          (assq-delete-all #'kmacro-x-mc-mode
                           minor-mode-overriding-map-alist))

    (mapc #'delete-overlay kmacro-x-mc-cursors)
    (kill-local-variable 'kmacro-x-mc-regexp)
    (kill-local-variable 'kmacro-x-mc-offsets)
    (kill-local-variable 'kmacro-x-mc-cursors)))

;;;###autoload
(define-minor-mode kmacro-x-mc-atomic-undo-mode
  "Undo the whole `kmacro-x-mc-mode' bulk operation at once.

Note: The original actions used for the macro recording are not
undone, only the the macro executions (called from
`kmacro-x-mc-apply')."
  :global t
  :require 'kmacro-x-mc
  (if kmacro-x-mc-atomic-undo-mode
      (advice-add #'kmacro-x-mc-apply :around
                  #'kmacro-x-undo-amalgamate-advice)
    (advice-remove #'kmacro-x-mc-apply
                   #'kmacro-x-undo-amalgamate-advice)))

(provide 'kmacro-x-mc)
;;; kmacro-x-mc.el ends here
