;;; docker-setup.el --- Config options for dockerfile mode. -*- lexical-binding: t -*-

;; Author: B.V. Raghav
;; Maintainer: B.V. Raghav
;; Version: 0.1
;; Package-Requires: (dockerfile-mode)
;; Homepage: https://github.com/bvraghav/dotemacs
;; Keywords: dotemacs


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

;; Config options for Dockerfile Emacs Mode.

;;; Code:

(use-package dockerfile-mode :ensure t)

(message "Completed Docker Setup!")

(provide 'docker-setup)

;;; docker-setup.el ends here
