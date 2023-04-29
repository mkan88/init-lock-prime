;;; init-lock.el --- Lock init.el behind simple arithmetic  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Grant Rosson, Matthew Kan

;; Original Author: Grant Rosson <grantrosson@gmail.com>
;; Author: Matthew Kan
;; Keywords:

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

;; Inhibit the opening of your init.el file to prevent tinkering when you
;; should be working.

;; TODO Lock all files in a given directory with list-dirs-recursively

;;; Code:

(defgroup init-lock nil
  "Lock files behind prime factorisation."
  :group 'convenience
  :prefix "init-lock")

(defcustom init-lock-files nil
  "Files to lock behind prime factorisation."
  :type '(repeat file))

(defun init-lock-loop ()
  (let ((N 1)
        (p 2)
        (q 3)
        (p-guess)
        (q-guess))
    (while (not (or
                  (and (eq p p-guess) (eq q q-guess))
                  (and (eq p q-guess) (eq q p-guess))))
      (setq p (string-to-number (shell-command-to-string "openssl prime -generate -bits 16")))
      (setq q (string-to-number (shell-command-to-string "openssl prime -generate -bits 16")))
      (setq N (* p q))
      (setq p-guess (read-number (format "N = %s, p = ?" N)))
      (setq q-guess (read-number (format "N = %s, q = ?" N))))
    nil))

;;;###autoload
(defun init-lock (orig-fun &rest args)
  (when (member (car args) init-lock-files)
    (init-lock-loop))
  (apply orig-fun args))

;;;###autoload
(defun init-lock-enable ()
  (interactive)
  (advice-add 'find-file :around 'init-lock))

(defun init-lock-disable ()
  (interactive)
  (init-lock-loop)
  (advice-remove 'find-file 'file-lock))

(provide 'init-lock-prime)
;;; init-lock.el ends here

