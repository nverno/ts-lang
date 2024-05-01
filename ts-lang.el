;;; ts-lang.el --- Get tree-sitter language info -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/ts-lang
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Created: 30 April 2024
;; Keywords:

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;; Code:

(eval-when-compile (require 'cl-lib))

(declare-function ts-lang--language-info "ts-lang-module")
(declare-function ts-lang-load "ts-lang-module")


(unless module-file-suffix
  (error "ts-lang requires module support (Emacs compiled with --with-modules)"))

(defun ts-lang-compile (&optional cmake)
  (interactive
   (list (if current-prefix-arg
             (read-file-name "Cmake: ")
           (executable-find "cmake"))))
  (or cmake (setq cmake (executable-find "cmake")))
  (unless (file-exists-p cmake)
    (error "CMake not found: %s" cmake))
  (let ((dir (shell-quote-argument
              (file-name-directory
               (locate-library "ts-lang.el" t)))))
    (compilation-start
     (format "cd \"%s\"; cmake -B build && make -C build" dir))))

(unless (require 'ts-lang-module nil t)
  (ts-lang-compile (executable-find "cmake"))
  (require 'ts-lang-module))


(cl-defstruct (ts-lang--info (:constructor ts-lang-make-info))
  "Tree-sitter language info."
  lang named anon fields)


(defun ts-lang--read-parser (&optional directory)
  "Read parser from DIRECTORY or default tree-sitter directory."
  (let* ((parsers
          (mapcar (lambda (f)
                    (cons (replace-regexp-in-string
                           "libtree-sitter-" "" (file-name-base f))
                          f))
                  (directory-files
                   (or directory
                       (expand-file-name "tree-sitter" user-emacs-directory))
                   t "^[^.]")))
         (choice (completing-read "Language: " parsers nil t)))
    (assoc-default choice parsers)))

;;;###autoload
(defun ts-lang-parser-info (lang &optional interactive)
  "Get information about parser for LANG.

Returns a list of lists, where the elements are:
 - named nodes
 - anonymous nodes
 - fields

Theses are lists of strings containing the names of the grammar components."
  (interactive (list (ts-lang--read-parser) t))
  (let* ((lang (ts-lang-load lang))
         (info (ts-lang--parser-info lang)))
    (when interactive
      (message "%S" info))
    (ts-lang-make-info
     :lang lang
     :named (car info)
     :anon (cadr info)
     :fields (caddr info))))

  
(provide 'ts-lang)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; ts-lang.el ends here
