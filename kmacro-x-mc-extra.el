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

(defgroup kmacro-x-mc-extra nil
  "Additional functionality for the multiple cursors"
  :group 'kmacro-x-mc)

;;;###autoload
(defun kmacro-x-mc-mark-at-click (event)
  "Toggle the fake cursor at the mouse position.

Some code borrowed from `mc/fake-cursor-at-point'."
  (interactive "e")
  (mouse-minibuffer-check event)
  (let ((position (event-end event)))
    (unless (windowp (posn-window position))
      (user-error "Position not in text area of window"))
    (select-window (posn-window position))
    (let* ((point (posn-point position))
           (overlays-at-point (overlays-at point)))
      (if overlays-at-point
          (dolist (ov overlays-at-point)
            (when (eq (overlay-get ov 'face) 'kmacro-x-mc-cursor-face)
              (delete-overlay ov)
              (setq kmacro-x-mc-cursors
                    (delete ov kmacro-x-mc-cursors))))
        (let ((ov (make-overlay point (1+ point) nil t)))
          (unless kmacro-x-mc-mode
            (kmacro-x-mc-mode 1))
          (overlay-put ov 'face 'kmacro-x-mc-cursor-face)
          (overlay-put ov 'offsets '(0 . 0))
          (push ov kmacro-x-mc-cursors))))))

;;;###autoload
(defun kmacro-x-mc-mark-next-or-rect ()
  "Like `kmacro-x-mc-mark-next' but fall back to
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

(provide 'kmacro-x-mc-extra)
;;; kmacro-x-mc-extra.el ends here