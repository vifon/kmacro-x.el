;;; kmacro-x.el --- Keyboard macro helpers and extensions  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Wojciech Siewierski

;; Author: Wojciech Siewierski
;; URL: https://github.com/vifon/kmacro-x.el
;; Keywords: convenience
;; Version: 0.9
;; Package-Requires: ((emacs "29.1"))

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

;; A collection of commands, modes and functions building on top of
;; the keyboard macros (kmacros) system.

;;; Code:

(require 'kmacro)


(defgroup kmacro-x nil
  "Keyboard macro helpers and extensions."
  :group 'kmacro)

;;;###autoload
(define-minor-mode kmacro-x-atomic-undo-mode
  "Undo the kmacro executions atomically."
  :global t
  :require 'kmacro-x
  :lighter " atomic-kmacro"
  (if kmacro-x-atomic-undo-mode
      (progn
        ;; Seemingly advising `execute-kbd-macro' should suffice, but
        ;; that's not true.  It might get called either from Elisp or
        ;; from `call-last-kbd-macro' which is a C function and so the
        ;; advice wouldn't apply for this call.  So we advice both to
        ;; cover hopefully all the possible entry points.
        (advice-add #'execute-kbd-macro :around
                    #'kmacro-x-undo-amalgamate-advice)
        (advice-add #'call-last-kbd-macro :around
                    #'kmacro-x-undo-amalgamate-advice))
    (advice-remove #'execute-kbd-macro
                   #'kmacro-x-undo-amalgamate-advice)
    (advice-remove #'call-last-kbd-macro
                   #'kmacro-x-undo-amalgamate-advice)))

(defun kmacro-x-undo-amalgamate-advice (orig &rest args)
  "Advice the ORIG function to amalgamate all the edits into one undo.

ARGS are passed verbatim to ORIG."
  ;; Place an `undo-boundary' to ensure the consecutive calls won't
  ;; get auto-amalgamated.
  (undo-boundary)
  (with-undo-amalgamate
    (apply orig args)))


(provide 'kmacro-x)
;;; kmacro-x.el ends here
