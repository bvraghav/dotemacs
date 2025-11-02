;;; rust-setup.el --- Setup for Rust -*- lexical-binding: t -*-

;; Author: B.V. Raghav
;; Maintainer: B.V. Raghav
;; Version: 1.0.0
;; Package-Requires: (rust-mode use-package)
;; Homepage: https://github.com/bvraghav/dotemacs.git
;; Keywords: dotemacs, rust


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

;; Setup for rust development

;;; Code:

(use-package ob-rust
  :ensure t)

(use-package cargo-mode
  :ensure t
  :custom
  (cargo-mode-use-comint nil))

(use-package flycheck-rust
  :ensure t)

(use-package rust-mode
  :ensure t
  :after (org cargo-mode)
  :hook ((rust-mode . flycheck-rust-setup)
         (rust-mode . cargo-minor-mode))
  :init
  (require 'flycheck-rust)
  (require 'ob-rust)
  (add-to-list 'org-babel-load-languages '(rust . t))
  (org-babel-do-load-languages
   'org-babel-load-languages
   org-babel-load-languages))

(provide 'rust-setup)

;;; rust-setup.el ends here
