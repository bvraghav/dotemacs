;;; c-headers.el --- a simple package                     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  B.V. Raghav

;; Author: B.V. Raghav <r@bvraghav.com>
;; Keywords: c-c++ headers
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Recipe for constructing c-headers using native completing-read
;; extensions

;; This package provides a set of completion candidates for c-headers.

;; --- Example
;; #include <|>
;; ---
;;`|' represents the cursor here

;; Invoke the command with `M-x insert-c-header' or assign a shortcut
;; like `C-c C-f C-i' and this library will search the filesystem and
;; provide completion candidates. In case the first level completion
;; is only a directory, then the library enters another interactive
;; function with `read-file-name' and provides completion candidates
;; for the files inside of the directory.

;; Finally, once the header filename is selected, an appropriate
;; header file is inserted after stripping off, the include folder
;; prefixes from the path.

;; This approach works for me, however, there are many more
;; customizations possible in this library. I leave it to time and
;; response, to implement those.

;; Any critique shall be highly appreciated.

;; For those interested, this library has the following mail thread as
;; the background.
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2016-07/msg00153.html

;; Additionally, the function `c-header' in this package can be used
;; in conjunction with yasnippets to fully automate the completion, as
;; follows:

;; # -*- mode: snippet -*-
;; # name : #include <...>
;; # key  : inc
;; # binding: C-c C-c C-i
;; # --
;; #include <`(c-header yas/selected-text)`>

;; The shotcut key-binding in the above snippet, whether to use it or
;; not at all, is fully the discretion of the user, of course.

;;; Code:

;; C-Header
;; ----------------------------------------------------------------------

(require 'cl-lib)
(defun c-include-path()
  (cl-remove-if (lambda (x) (eq nil x))
		(cl-reduce #'append
			   (mapcar #'parse-colon-path
				   (list (c-std-libs)
					 (format "%s/usr/include" (getenv "HOME"))
					 ;; (format "%s/.local/include" (getenv "HOME"))
					 (getenv "CPATH")
					 "/usr/include:/usr/local/include:/usr/include/x86_64-linux-gnu")))))
;; (c-include-path)

(defun c-std-libs()
  (let ((base-dir "/usr/include/c++"))
    (cl-reduce (lambda (sum item) (format "%s:%s" sum item))
	       (mapcar (lambda (x) (format "%s/%s" base-dir x))
		       (cl-remove-if (lambda(x)
				       (string-suffix-p "." x))
				     (directory-files base-dir))))))

(defun c-include-libs-raw(path)
  (cl-remove-if (lambda (x) (eq nil x))
		(cl-reduce #'append
			   (mapcar (lambda (x)
				     (mapcar (lambda (y)
					       (let ((long-y (format "%s%s" x y)))
						 (if (file-accessible-directory-p long-y)
						     long-y
						   y)))
					     (when (or (file-accessible-directory-p x)
						       (and (file-symlink-p x)
							    (file-accessible-directory-p
							     (file-symlink-p x))))
					       (directory-files x))))
				   path))))

(defun sanitize-dir-read (path-list)
  (let* ((path-list (cl-remove-if (lambda(x)
				    (or (eq nil x)
					(string-suffix-p "." x)
					(string-suffix-p "~" x)
					(string-prefix-p "_" x)))
				  path-list))
	 (path-list (cl-remove-duplicates path-list :test #'string=)))
    path-list))

(defun c-headers-current-project()
  (when (projectile-project-p)
    (append
     (mapcar (lambda (x) (expand-file-name x (projectile-project-root)))
	     (cl-remove-if-not (lambda (x) (string-match-p "\\.h\\(pp\\)\?$" x))
			       (projectile-current-project-files)))
     `(,(substring (projectile-project-root) 0 -1)))))

(defun c-headers-primary ()
  (append (c-headers-current-project)
	  (sanitize-dir-read (c-include-libs-raw (c-include-path)))))

(defun guess-include-dir (file-name path-list)
  (let* ((ll (string-match "inc" file-name))
	 (ll (when ll (string-match  "/" file-name ll)))
	 (include-dir (when ll
			(substring file-name 0 (+ 1 ll)))))
    (when (and include-dir
	       (file-accessible-directory-p include-dir))
      include-dir)))

(defun find-include-dir(file-name path-list)
  (cond ((null path-list) nil)
	((string-prefix-p (car path-list) file-name) (car path-list))
	(t (find-include-dir file-name (cdr path-list)))))

(defun include-dir (file paths)
  (cl-labels ((inter-fun (file-name path-list func-list)
			 (if (null func-list)
			     nil
			   (let ((val (funcall (car func-list) file-name path-list)))
			     (if (null val)
				 (inter-fun file-name path-list (cdr func-list))
			       val)))))
    (inter-fun file paths
	       (list #'find-include-dir
		     #'guess-include-dir))))

(defun c-header(&optional initial-match)
  (let* ((selected-path (completing-read "C/C++ Header: " (c-headers-primary)
					 nil nil initial-match))
	 (final-path (if (file-accessible-directory-p selected-path)
			 (read-file-name "C/C++ Header: " (format "%s/" selected-path))
		       selected-path))
	 (inclusive-path (include-dir final-path (c-include-path)))
	 (include-file-name (file-relative-name final-path inclusive-path)))
    include-file-name))

(defun insert-c-header()
  (interactive)
  (insert (c-header)))

;; ----------------------------------------------------------------------


(provide 'c-headers)
;;; c-headers.el ends here
