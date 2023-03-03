;;; kmacro-x-mc-legacy.el --- The old implementation of the multiple cursors  -*- lexical-binding: t; -*-

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

;; The obsolete version of the multiple cursors based of keyboard
;; macros.  Less robust but with different semantics.  No longer
;; maintained but might be useful.

;;; Code:

(require 'kmacro-x-mc)

(defgroup kmacro-x-mc-legacy nil
  "Multiple cursors implemented with keyboard macros (legacy)."
  :group 'kmacro-x-mc)

(defface kmacro-x-mc-legacy-highlight-face
  '((t (:inherit hi-yellow)))  ;`highlight-regexp' hardcodes `hi-yellow'
  "The face used by `kmacro-x-mc-legacy-region'.")

(defcustom kmacro-x-mc-legacy-region-sequence-fmt "C-s %s C-r RET 2*C-s RET"
  "The sequence of keys used by `kmacro-x-mc-legacy-region'.

This sequence should search for the next occurrence of the query
and leave the region in a predictable state for the user to use.

The search query will be spliced into it using `format', and so
it should contain exactly one `%s' placeholder."
  :type 'string)


;;;###autoload
(defun kmacro-x-mc-legacy-region (start end &optional highlight)
  "Record a keyboard macro emulating multiple cursors.

The kmacro will first search for current region and then execute
the rest of the recorded kmacro.  During the kmacro execution,
mark is always at the beginning of the match and point is at
the end.

START and END mark the region.

If HIGHLIGHT is non-nil (or with the prefix argument when using
interactively) highlight the query using `highlight-regexp'.
Use `\\[unhighlight-regexp]' to remove the highlight later."
  (interactive "r\nP")
  (unless (and (= start (region-beginning))
               (= end (region-end)))
    ;; Ensure a consistent behavior when called interactively and when
    ;; called from Elisp with arguments not being the current region.
    ;; Considering the interactive nature of the kmacros, tampering
    ;; with the user's region seems very much justified.
    (push-mark start)
    (goto-char end))
  (let ((query (buffer-substring-no-properties start end)))
    (when (< (point) (mark))
      (exchange-point-and-mark))
    (deactivate-mark)
    (kmacro-push-ring)
    (setq last-kbd-macro
          (read-kbd-macro (format kmacro-x-mc-legacy-region-sequence-fmt
                                  (format-kbd-macro query))))
    (when highlight
      (highlight-regexp (regexp-quote query)
                        'kmacro-x-mc-legacy-highlight-face))
    (start-kbd-macro 'append 'no-exec)))

;;;###autoload
(defun kmacro-x-mc-legacy (&optional highlight)
  "A more user friendly version of `kmacro-x-mc-legacy-region'.

If selection is active, it does the same thing as
`kmacro-x-mc-legacy-region'.  If there is no active selection, it uses
the symbol at point instead.

See `kmacro-x-mc-legacy-region' for the HIGHLIGHT argument."
  (interactive "P")
  (let ((bounds (if (use-region-p)
                    (cons (region-beginning) (region-end))
                  (bounds-of-thing-at-point 'symbol))))
    (unless bounds
      (error "%s" "No region or symbol to act on"))
    (let ((start (car bounds))
          (end (cdr bounds)))
      (kmacro-x-mc-legacy-region start end highlight))))


(provide 'kmacro-x-mc-legacy)
;;; kmacro-x-mc-legacy.el ends here
