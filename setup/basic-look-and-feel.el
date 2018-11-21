(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Basic look and feel
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode t)
(global-reveal-mode t)
(outline-minor-mode t)
(column-number-mode t)
(display-time-mode t)
(setq display-time-24hr-format t)
(display-battery-mode t)
(electric-pair-mode t)
(show-paren-mode t)
(setq show-paren-delay 0)
(toggle-word-wrap t)
(global-subword-mode t)

;; Ediff Split Window Sensibly
(setq ediff-split-window-function 'split-window-sensibly)
(setq ediff-merge-split-window-function 'split-window-sensibly)

;; Pairing
(setq skeleton-pair t)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)

;; No confirm new file creation
(setq confirm-nonexistent-file-or-buffer nil)

;; Wc Mode
(require 'wc-mode)
(wc-mode t)

;; Fundamental mode
(add-to-list 'auto-mode-alist '("\\.ply\\'" . fundamental-mode))

;; Expand Region
(require 'expand-region)

(provide 'basic-look-and-feel)
