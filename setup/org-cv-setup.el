;;; org-cv-setup.el --- Setup Org-CV -*- lexical-binding: t -*-

;; Author: B.V. Raghav
;; Maintainer: B.V. Raghav
;; Version: version
;; Package-Requires: (org-cv)
;; Homepage: https://github.com/bvraghav/dotemacs
;; Keywords: org-cv


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

;; Adaptation of installation instrutions on this
;; project page: https://titan-c.gitlab.io/org-cv/

;;; Code:

(require 'org)
(require 'ox-latex)

(message "Initialising Org CV!")

(defcustom bvr/org-cv-dir "~/code/org-cv/"
  "Path to ORG-CV codebase.")

(use-package ox-moderncv
  :load-path bvr/org-cv-dir
  :init (require 'ox-moderncv)
  :bind (("C-c C-# C-r" . bvr/org-cv-publish-resume)
         ("C-c # r"     . bvr/org-cv-publish-resume)))

(defun bvr/org-cv-publish-resume (&optional fname)
  "Publish resume.

Publish FNAME as a resume using org-cv moderncv export.
Use (BUFFER-FILE-NAME) for FNAME by default."
  (interactive)
  (let* ((fname (or fname (buffer-file-name)))
         (out-tex (file-name-with-extension fname ".tex")))
    (org-export-to-file 'moderncv out-tex)
    (org-latex-compile out-tex)))

(message "DONE: Initialising Org CV!")

(provide 'org-cv-setup)

;;; org-cv-setup.el ends here
