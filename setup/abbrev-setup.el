;;; abbrev-setup.el --- Setup Buffer Local Abbrevs. -*- lexical-binding: t -*-

;; Author: B.V. Raghav
;; Maintainer: B.V. Raghav
;; Version: 1.0.0
;; Package-Requires: ()
;; Homepage: https://github.com/bvraghav
;; Keywords: abbrev


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

;; Setup Buffer Local Abbrevs.

;; Adapted from here:
;; https://stackoverflow.com/a/49216125

;;; Code:


(defun set-local-abbrevs (abbrevs)
    "Add ABBREVS to `local-abbrev-table' and make it buffer local.

     ABBREVS should be a list of abbrevs as passed to `define-abbrev-table'.
     The `local-abbrev-table' will be replaced by a copy with the new 
     abbrevs added, so that it is not the same as the abbrev table used
     in other buffers with the same `major-mode'.

     Adapted from here:
     https://stackoverflow.com/a/49216125"
    (let* ((bufname (buffer-name))
           (prefix (substring (md5 bufname) 0 (length bufname)))
           (tblsym (intern (concat prefix "-abbrev-table"))))
      (set tblsym (copy-abbrev-table local-abbrev-table))
      (dolist (abbrev abbrevs)
          (define-abbrev (eval tblsym)
            (car abbrev)
            (cadr abbrev)
            (caddr abbrev)))
      (setq-local local-abbrev-table (eval tblsym))))

(defun create-local-abbrev (b e)
  "Create Buffer Local Abbrev."
  (interactive "r")

  (let* ((abbr (read-string "Abbrev: "))
         (expn (completing-read "Expansion: " kill-ring nil nil (buffer-substring b e))))
    (set-local-abbrevs `((,abbr ,expn)))))

(defalias 'abbrev-add-local 'create-local-abbrev)

(provide 'abbrev-setup)

;;; abbrev-setup.el ends here
