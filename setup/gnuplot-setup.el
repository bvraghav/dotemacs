;;; gnuplot-setup.el --- Config setup for package gnuplot -*- lexical-binding: t -*-

;; Author: B.V. Raghav
;; Maintainer: B.V. Raghav
;; Version: 0.1
;; Package-Requires: (gnuplot)
;; Homepage: https://github.com/bvraghav/dotemacs
;; Keywords: dotemacs,gnuplot


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

;; Config setup for Emacs package `gnuplot'.

;;; Code:


(use-package gnuplot
  :ensure t
  :mode ("\\.gnuplot\\'" . gnuplot-mode)
  :bind (:map gnuplot-mode-map
              ("C-c C-c" . gnuplot-send-buffer-to-gnuplot))
  )                                     ; The END


(provide 'gnuplot-setup)

;;; gnuplot-setup.el ends here

