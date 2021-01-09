;; define toolbar reset
(defun graphic-p ()
  (if (< emacs-major-version 23)
      (or window-system (getenv "DISPLAY"))
    (display-graphic-p)))


;; Basic reset
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Basic look and feel
(when (graphic-p)
  (progn
    (tool-bar-mode -1)
    (scroll-bar-mode -1)))    
(menu-bar-mode -1)
(scroll-bar-mode -1)
;; ;; Inhibit Linum mode for non-text modes
;; (global-linum-mode t)
(global-reveal-mode t)
(outline-minor-mode t)
(column-number-mode t)
(setq display-time-24hr-format t)
(display-time-mode t)
(display-battery-mode t)
(electric-pair-mode t)
(show-paren-mode t)
(setq show-paren-delay 0)
(toggle-word-wrap t)
(global-subword-mode t)

;; Inhibit Linum mode for non-text modes
(defun inhibit-linum-mode ()
  "Inhibit global linum mode"
  (add-hook 'after-change-major-mode-hook
	    (lambda () (linum-mode 0))
	    :append :local))
(setq non-text-modes
      '(doc-view-mode-hook
	doc-view-minor-mode-hook
	image-mode-hook
	image-dired-display-image-mode-hook
	image-dired-minor-mode-hook
	image-dired-thumbnail-mode-hook
	image-minor-mode-hook))
(dolist (mode-hook non-text-modes)
  (add-hook 'mode-hook #'inhibit-linum-mode))

;; Enable linum mode for text modes
(setq text-mode-hook-list
      '(text-mode-hook
	prog-mode-hook
	fundamental-mode-hook))
(dolist (mode-hook text-mode-hook-list)
  (add-hook mode-hook #'linum-mode))


;; Other basic customizations
;; Info
(setq Info-additional-directory-list
      '("/usr/share/info" "/usr/local/share/info")
      Info-default-directory-list
      '("/usr/share/info/" "~/.local/share/info/")

      ;; Man
      Man-notify-method 'pushy
      Man-width 65

      ;; Auth Sources
      auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc")

      ;; Bookmarks
      bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks"
      bookmark-save-flag 1
      bookmark-version-control t

      ;; Theme
      custom-enabled-themes '(zenburn)

      ;; Desktop Saving
      desktop-path '("~/.emacs.d/" "~" "~/.emacs.d/dtp")
      desktop-save-mode nil

      ;; Warning
      echo-bell-mode t
      visible-bell t

      ;; Ediff
      ediff-split-window-function 'split-window-right
      ediff-merge-split-window-function 'split-window-right

      ;; External apps
      image-dired-external-viewer "/usr/bin/feh"

      ;; Indents
      lua-indent-level 2
      sh-basic-offset 2
      sh-indentation 2

      ;; Whitespaces
      whitespace-action nil)

;; Pairing
(setq skeleton-pair t)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)

;; No confirm new file creation
(setq confirm-nonexistent-file-or-buffer nil)

;; Wc Mode
(use-package wc-mode :ensure t)

;; Fundamental mode
(add-to-list 'auto-mode-alist '("\\.ply\\'" . fundamental-mode))

;; Wavefront Obj mode
(add-to-list 'auto-mode-alist '("\\.obj\\'" . wavefront-obj-mode))

;; Expand Region
(use-package expand-region :ensure t)

;; Smartparens
(use-package smartparens
  :ensure t

  :config
  (require 'smartparens-config))

;; Pinentry
(use-package pinentry
  :ensure t

  :init
  (pinentry-start))

;; Fill Column
(setq-default fill-column 55)

;;; Drew Adams' enhancements
;;; ===================================================
;; Crosshairs
(require 'crosshairs)
(crosshairs-mode t)

(require 'icomplete+)
(icompletep-cycling-mode t)

;; Dabbrev
(require 'dabbrev)

;; Dot Mode
(use-package dot-mode :ensure t)
(dot-mode t)

;; ThingsAtPt+
(eval-after-load "thingatpt"
  '(when (require 'thingatpt+)
     (tap-redefine-std-fns)))

;; Bookmark+
(require 'bookmark+)

;; Dired+
(require 'dired-x)
(require 'dired+)
;;; ===================================================

(provide 'basic-look-and-feel)
