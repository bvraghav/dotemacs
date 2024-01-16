;;; livedown-setup.el --- Setup Livedown -*- lexical-binding: t -*-

;; Author: B.V. Raghav
;; Maintainer: B.V. Raghav
;; Version: 1.0.0
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


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

;; Setup Livedown for Emacs.

;;; Code:

(defcustom bvr/livedown-dir "~/code/emacs-livedown/"
  "Livedown Package Load Path.")

(use-package livedown
  :load-path bvr/livedown-dir

  :init
  (custom-set-variables
   '(livedown-autostart nil) ; automatically open
                             ; preview when opening
                             ; markdown files

   '(livedown-open t)        ; automatically open the
                             ; browser window

   '(livedown-port 1337)     ; port for livedown server
   '(livedown-browser nil))  ; browser to use

  :bind
  (("C-c C-q C-l" . livedown-preview))
  )

(provide 'livedown-setup)

;;; livedown-setup.el ends here
