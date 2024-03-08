;;; arduino-setup.el --- Setup for Arduino -*- lexical-binding: t -*-

;; Author: B.V. Raghav
;; Maintainer: B.V. Raghav
;; Version: 1.0.0
;; Package-Requires: (arduino-mode use-package)
;; Homepage: https://github.com/bvraghav/dotemacs.git
;; Keywords: dotemacs, arduino


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

;; Setup for arduino development

;;; Code:

(use-package arduino-mode
  :ensure t
  :after org
  :hook (arduino-mode . flycheck-arduino-setup)
  :init
  (require 'flycheck-arduino)
  (require 'ob-arduino)
  (add-to-list 'org-babel-load-languages '(arduino . t))
  (org-babel-do-load-languages
   'org-babel-load-languages
   org-babel-load-languages))

(provide 'arduino-setup)

;;; arduino-setup.el ends here
