;;; hs-setup.el --- Setup HS Minor Mode -*- lexical-binding: t -*-

;; Author: B.V. Raghav
;; Maintainer: B.V. Raghav
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: https://github.com/bvraghav/dotemacs.git
;; Keywords: code-folding,hideshow,hs-minor-mode


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

;; Setup HS Minor mode for keybindings with prefix `C-c
;; C-#' instead of `C-c @'.

;;; Code:


(use-package hideshow
  :bind (:map hs-minor-mode-map
              ("C-c C-# C-h"    . hs-hide-block)
              ("C-c C-# C-s"    . hs-show-block)
              ("C-c C-# C-M-h"  . hs-hide-all)
              ("C-c C-# C-M-s"  . hs-show-all)
              ("C-c C-# C-l"    . hs-hide-level)
              ("C-c C-# C-c"    . hs-toggle-hiding)
              ("C-c C-# C-a"    . hs-show-all)
              ("C-c C-# C-t"    . hs-hide-all)
              ("C-c C-# C-d"    . hs-hide-block)
              ("C-c C-# C-b"    . hs-hide-block)
              ("C-c C-# C-e"    . hs-toggle-hiding)

              ;; Remove them from minor mode map so
              ;; that helm show the previous key
              ;; bindings.
              ;; --------------------------------------
              ("C-c @ C-h"    . nil)
              ("C-c @ C-s"    . nil)
              ("C-c @ C-M-h"  . nil)
              ("C-c @ C-M-s"  . nil)
              ("C-c @ C-l"    . nil)
              ("C-c @ C-c"    . nil)
              ("C-c @ C-a"    . nil)
              ("C-c @ C-t"    . nil)
              ("C-c @ C-d"    . nil)
              ("C-c @ C-b"    . nil)
              ("C-c @ C-e"    . nil)))


;; Hideshow defintions for reference
;; ----------------------------------------------------

'(                                      ; Commands
  hs-show-all
  hs-minor-mode
  hs-hide-block
  hs-hide-level
  hs-show-block
  hs-toggle-hiding
  hs-minor-mode-menu
  hs-mouse-toggle-hiding
  hs-hide-initial-comment-block
  )


'(                                      ; Functions
  hs-overlay-at
  hs-life-goes-on
  hs-isearch-show
  hs-make-overlay
  hs-forward-sexp
  hs-grok-mode-type
  hs-already-hidden-p
  hs-discard-overlays
  hs-inside-comment-p
  hs-hide-comment-region
  hs-hide-block-at-point
  hs-find-block-beginning
  hs-hide-level-recursive
  hs-isearch-show-temporary
  hs-looking-at-block-start-p
  hs-c-like-adjust-block-beginning
  )

'(                                      ;Variables
  hs-headline
  hs-show-hook
  hs-hide-hook
  hs-minor-mode
  hs-isearch-open
  hs-allow-nesting
  hs-minor-mode-map
  hs-set-up-overlay
  hs-c-start-regexp
  hs-minor-mode-menu
  hs-minor-mode-hook
  hs-block-end-regexp
  hs-forward-sexp-func
  hs-block-start-regexp
  hs-special-modes-alist
  hs-adjust-block-beginning
  hs-block-start-mdata-select
  hs-hide-comments-when-hiding-all
  hs-hide-all-non-comment-function
  )
;; ----------------------------------------------------



(provide 'hs-setup)

;;; hs-setup.el ends here
