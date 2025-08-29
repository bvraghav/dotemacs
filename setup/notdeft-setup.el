;;; notdeft-setup.el --- Setup for Notdeft. -*- lexical-binding: t -*-

;; Author: B.V. Raghav
;; Maintainer: B.V. Raghav
;; Version: 0.0.1
;; Package-Requires: ()
;; Homepage: https://github.com/bvraghav/dotemacs.git
;; Keywords: notdeft,notes


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

;; Setup for Notdeft.
;;
;; https://tero.hasu.is/notdeft/
;;
;; Deft like full text search for Org-roam.
;;
;; Installation:
;;
;; Shell Commands:
;;     cd ${SRC_ROOT}
;;     git clone https://github.com/hasu/notdeft.git
;;     cd notdeft
;;     make
;;     cd xapian
;;     pacman -Ss pkgconf tclap xapian-core
;;     make
;;
;; Emacs Setup:
;;
;;     (use-package notdeft-setup
;;       :custom
;;       (bvr/notdeft-setup/repo  "~/src/notdeft"))
;;     

;;; Code:

(require 'f)

(defgroup bvr/notdeft-setup nil
  "Customise local NotDeft setup.")

(defcustom bvr/notdeft-setup/repo
  "~/src/notdeft"
  "Path to the repo of NotDeft.

The location where https://github.com/hasu/notdeft.git has been cloned."
  :type 'directory
  :group 'bvr/notdeft-setup)

(add-to-list 'load-path bvr/notdeft-setup/repo)
(require 'notdeft-autoloads)

(use-package notdeft
  :bind (("C-z C-M-f" . notdeft)
         ("C-x C-M-f" . notdeft-lucky-find-file))
  :config
  (setq notdeft-directories `(,org-roam-directory ,org-directory))
  :custom
  (notdeft-extension "org")
  (notdeft-secondary-extensions '("md" "txt")))

;; (custom-set-variables
;;  `(notdeft-xapian-program ,(f-join bvr/notdeft-setup/repo "xapian" "notdeft-xapian")))

(provide 'notdeft-setup)

;;; notdeft-setup.el ends here
