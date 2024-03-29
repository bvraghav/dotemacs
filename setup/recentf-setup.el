;;; recentf-setup.el --- Setup Recentf -*- lexical-binding: t -*-

;; Author: B.V. Raghav
;; Maintainer: B.V. Raghav
;; Version: 0.1
;; Package-Requires: (recentf)
;; Homepage: https://github.com/bvraghav/dotemacs
;; Keywords: recentf,dotemacs


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

;; Setup Recentf

;;; Code:

(require 'recentf)
;; Activate recentf mode
;; ====================================================
(recentf-mode t)

;; Count items in menu and saved list
;; ----------------------------------------------------
(setq recentf-max-menu-items 8
      recentf-max-saved-items 1000)

;; ====================================================
;; Periodically saving the list of files
;; ----------------------------------------------------
;; https://www.emacswiki.org/emacs/RecentFiles#h5o-1
;; ----------------------------------------------------

(run-at-time nil (* 5 60) 'recentf-save-list)

;; ====================================================




;; ====================================================
;; Updating recentf-list when renaming files in dired
;; ----------------------------------------------------
;; https://www.emacswiki.org/emacs/RecentFiles#h5o-22
;; ----------------------------------------------------

;; Magic advice to rename entries in recentf when
;; moving files in dired.
(defun rjs/recentf-rename-notify
    (oldname newname &rest args)
  (if (file-directory-p newname)
      (rjs/recentf-rename-directory oldname newname)
    (rjs/recentf-rename-file oldname newname)))

(defun rjs/recentf-rename-file (oldname newname)
  (setq recentf-list
        (mapcar (lambda (name)
                  (if (string-equal name oldname)
                      newname
                    name))
                recentf-list)))

(defun rjs/recentf-rename-directory (oldname newname)
  ;; oldname, newname and all entries of recentf-list
  ;; should already be absolute and normalised so I
  ;; think this can just test whether oldname is a
  ;; prefix of the element.
  (setq recentf-list
        (mapcar (lambda (name)
                  (if (string-prefix-p oldname name)
                      (concat
                       newname
                       (substring name
                                  (length oldname)))
                    name))
                recentf-list)))

(advice-add 'dired-rename-file
            :after #'rjs/recentf-rename-notify)

;; ====================================================




(provide 'recentf-setup)

;;; recentf-setup.el ends here
;;; ---------------------------------------------------
