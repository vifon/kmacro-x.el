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

;;; Code:

(require 'kmacro-x)

(defgroup kmacro-x-mc nil
  "Multiple cursors implemented with keyboard macros."
  :group 'kmacro-x)

(defface kmacro-x-mc-cursor-face
  '((t (:inverse-video t)))
  "The face used for the fake cursors created in `kmacro-x-mc-mode'.")

(defvar-local kmacro-x-mc-regexp nil
  "A regexp matching the intended cursor positions.")

(defvar-local kmacro-x-mc-offsets '(nil . nil)
  "The cons with offsets of the point and the mark within the selection.")

(defvar-local kmacro-x-mc-cursors nil
  "The overlays for displaying and keeping the cursor positions.")

(defun kmacro-x--mc-mark (&optional backwards)
  "Create a new fake cursor for `kmacro-x-mc-mode'.

Enables `kmacro-x-mc-mode' if not enabled yet, for the
necessary initialization."

  ;; Sanity check to prevent defining cursors with inconsistent
  ;; starting conditions.
  (when (and kmacro-x-mc-mode
             (not (member last-command '(kmacro-x-mc-mark-next
                                         kmacro-x-mc-mark-previous))))
    (user-error "This command can only be used when starting a bulk edit."))

  (unless kmacro-x-mc-mode
    (kmacro-x-mc-mode 1))

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

          ;; Store the offsets per cursor for future development.
          (overlay-put ov 'offsets kmacro-x-mc-offsets)

          ;; Either append or prepend the new cursor.
          (if backwards
              (push ov kmacro-x-mc-cursors)
            (setq kmacro-x-mc-cursors (append kmacro-x-mc-cursors (list ov)))))
      (message "No further matches"))))

;;;###autoload
(defun kmacro-x-mc-mark-next ()
  "Create a new fake cursor forward."
  (interactive)
  (kmacro-x--mc-mark))

;;;###autoload
(defun kmacro-x-mc-mark-previous ()
  "Create a new fake cursor backwards."
  (interactive)
  (kmacro-x--mc-mark 'backwards))

(defun kmacro-x-mc-apply ()
  "Apply the recoded macro for each cursor."
  (interactive)
  (end-kbd-macro)
  (dolist (ov kmacro-x-mc-cursors)
    (unless (overlay-get ov 'main-cursor)
      (goto-char (+ (overlay-start ov)
                    (car (overlay-get ov 'offsets))))
      (push-mark (+ (overlay-start ov)
                    (cdr (overlay-get ov 'offsets))))
      (call-last-kbd-macro)))
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

    ;; Make sure the macro recording is cancelled.  Since it's bound
    ;; to `C-g', it might be a good idea to call it anyway.
    (keyboard-quit)))


(defvar kmacro-x-mc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") #'kmacro-x-mc-quit)
    (define-key map (kbd "RET") #'kmacro-x-mc-apply)
    map))

(define-minor-mode kmacro-x-mc-mode
  "Record a keyboard macro to apply with multiple cursors.

`RET' applies, `C-g' aborts."
  :require 'macro-x
  :lighter " kmacro-mc"
  (if kmacro-x-mc-mode
      (let ((bounds (if (use-region-p)
                        (cons (region-beginning) (region-end))
                      (bounds-of-thing-at-point 'symbol))))

        (let ((regexp (regexp-quote
                       (buffer-substring-no-properties
                        (car bounds)
                        (cdr bounds)))))
          (setq-local kmacro-x-mc-regexp
                      (if (use-region-p)
                          regexp
                        (concat "\\_<" regexp "\\_>"))))

        (let ((ov (make-overlay (car bounds) (cdr bounds))))
          (overlay-put ov 'main-cursor t)
          (setq-local kmacro-x-mc-cursors (list ov)))

        (unless (use-region-p)
          ;; The end of the symbol is as good of a place as any for
          ;; the mark.  Definitely better than whatever random
          ;; position it was before.
          (push-mark (cdr bounds)))

        (setq-local kmacro-x-mc-offsets
                    (cons (- (point)
                             (car bounds))
                          (- (mark)
                             (car bounds))))

        (start-kbd-macro nil))

    (mapc #'delete-overlay kmacro-x-mc-cursors)
    (kill-local-variable 'kmacro-x-mc-regexp)
    (kill-local-variable 'kmacro-x-mc-offsets)
    (kill-local-variable 'kmacro-x-mc-cursors)))

(provide 'kmacro-x-mc)
