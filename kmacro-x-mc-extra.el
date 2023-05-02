;;; kmacro-x-mc-extra.el --- Additional functionality for the multiple cursors  -*- lexical-binding: t; -*-

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

;; Non-critical and/or experimental features for the multiple cursors
;; based on keyboard macros.

;;; Code:

(require 'kmacro-x-mc)

(require 'rect)

(defgroup kmacro-x-mc-extra nil
  "Additional functionality for the multiple cursors."
  :group 'kmacro-x-mc)

;;;###autoload
(defun kmacro-x-mc-mark-at-click (event)
  "Toggle the fake cursor at the mouse position.

EVENT is the mouse click event.

Some code borrowed from `mc/toggle-cursor-on-click'."
  (interactive "e")
  (mouse-minibuffer-check event)
  (let ((position (event-end event)))
    (unless (windowp (posn-window position))
      (user-error "Position not in text area of window"))
    (select-window (posn-window position))
    (let ((point (posn-point position)))
      (unless (catch 'cursor
                (dolist (ov (overlays-at point))
                  (unless (kmacro-x-mc--main-cursor-p ov)
                    (delete-overlay ov)
                    (setq kmacro-x-mc-cursors
                          (delete ov kmacro-x-mc-cursors))
                    (throw 'cursor ov))))
        (let ((ov (make-overlay point (1+ point) nil t)))
          (unless kmacro-x-mc-mode
            (let ((kmacro-x-mc-mark-whole-symbol nil))
              (kmacro-x-mc-mode 1)))
          (overlay-put ov 'face 'kmacro-x-mc-cursor-face)
          (overlay-put ov 'offsets '(0 . 0))
          (push ov kmacro-x-mc-cursors))))))

;;;###autoload
(defun kmacro-x-mc-mark-next-or-rect ()
  "A smart version of `kmacro-x-mc-mark-next' with a fallback.

Acts just like `kmacro-x-mc-mark-next' but falls back to
`rectangle-mark-mode' when there is no selection."
  (interactive)
  (require 'rect)
  (if (and (not rectangle-mark-mode)
           (use-region-p))
      (progn
        (setq this-command 'kmacro-x-mc-mark-next)
        (kmacro-x-mc-mark-next))
    (unless rectangle-mark-mode
      (rectangle-mark-mode 1))
    (rectangle-next-line)))

;;;###autoload
(defun kmacro-x-mc-pause ()
  "Pause the macro recording to allow free movement.

Intended to be bound to `C-v' in `kmacro-x-mc-mode-map', calls
`scroll-up-command' when the macro is already paused."
  (interactive)
  (if defining-kbd-macro
      (progn
        (cancel-kbd-macro-events)
        (save-mark-and-excursion
          (end-kbd-macro)
          (message "%s" (substitute-command-keys
                         "Macro recording paused, press \
`\\[exit-recursive-edit]' to exit recursive edit"))
          (recursive-edit)
          (start-kbd-macro 'append 'no-exec)))
    (call-interactively #'scroll-up-command)))

(provide 'kmacro-x-mc-extra)
;;; kmacro-x-mc-extra.el ends here
