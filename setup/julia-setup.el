;;; julia-setup.el --- Setup Julia Programming Language -*- lexical-binding: t -*-

;; Author: B.V. Raghav
;; Maintainer: B.V. Raghav
;; Version: 1.0.0
;; Package-Requires: ()
;; Homepage: https://github.com/bvraghav/dotemacs.git
;; Keywords: Julia,Programming Language


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

;; Setup Julia Programming Lanugage

;;; Code:

(use-package julia-mode
  :ensure t
  :config
  (setq julia-indent-offset 2))

(provide 'julia-setup)

;;; julia-setup.el ends here
