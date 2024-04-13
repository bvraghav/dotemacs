
;; Basic reset
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Basic look and feel
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
(setq-default word-wrap t)
(global-subword-mode t)
(setq ring-bell-function 'ignore)

;; Inhibit Linum mode for non-text modes
(defun safe-linum-mode (&rest args)
  (if (version-list-<=
       (version-to-list emacs-version)
       '(29 0))
      (apply #'linum-mode args)
    (apply #'display-line-numbers-mode args)))
(defun inhibit-linum-mode ()
  "Inhibit global linum mode"
  (add-hook 'after-change-major-mode-hook
	    (lambda () (safe-linum-mode 0))
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
  (add-hook mode-hook #'safe-linum-mode))


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
      ;; ----------------------------------------------
      ;; https://magit.vc/manual/ghub.html#Storing-a-Token
      ;; Make sure only one exists and store everything
      ;; there.
      ;; auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc")
      auth-sources '("~/.authinfo.gpg")

      ;; Bookmarks
      bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks"
      bookmark-save-flag 1
      bookmark-version-control t

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
      css-indent-offset 2
      indent-tabs-mode nil     ; Untabify (by default).

      ;; Set dictionary for Aspell/ Ispell/ Hunspell/ Flyspell.
      ispell-program-name "/usr/bin/hunspell"
      ispell-dictionary "en_GB-large"

      ;; Whitespaces
      whitespace-action nil)

;; Theme
(defun bvr/set-theme-colors ()
  "BVR Customisations over the base theme"
  (set-face-background 'default "#2c353f")
  (set-face-foreground 'font-lock-string-face "#6ca0f0")
  (set-face-foreground 'font-lock-comment-face "gray50")
  (set-face-foreground 'font-lock-comment-delimiter-face "gray50")
  (set-face-foreground 'font-lock-doc-face "#987878"))
(use-package zenburn-theme
  :ensure t
  :demand

  :init
  (setq custom-enabled-themes '(zenburn))

  :config
  (bvr/set-theme-colors))

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

;; Yaml Mode
(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

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

;; Unfill
(use-package unfill
  :ensure t
  :bind ("M-Q" . unfill-toggle))

;; Fill Column
(setq-default fill-column 55)

;; No tabs on indentation
(setq-default indent-tabs-mode nil)

;; Debug on Error
(setq debug-on-error t)

;; Typographical editing. (eg. smart quotes)
(use-package typo :ensure t)

;; GNU Global (gtags)
(use-package ggtags :ensure t)

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

;; ;; Bookmark+
;; (require 'bookmark+)

;; Dired+
(require 'dired-setup)
;; (require 'dired+)
;;; ===================================================

;; Activate winner mode
;; ====================================================
(winner-mode t)

;; Require uuidgen
;; ----------------------------------------------------
(use-package uuidgen :ensure t)
(use-package bvr-uuid :after uuidgen)

;; Install glsl-mode maybe
;; ----------------------------------------------------
(use-package glsl-mode :ensure t)

;; UTF-8 by default
;; ----------------------------------------------------
;; http://xahlee.info/emacs/emacs/emacs_file_encoding.html

;; UTF-8 as default encoding
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)

;; Add this especially on Windows, else python output
;; problem
(set-terminal-coding-system 'utf-8-unix)
;; ----------------------------------------------------

;; Enable Flyspell by default
;; ----------------------------------------------------
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Set text as default major mode
;; ----------------------------------------------------
(setq-default major-mode 'text-mode)
(provide 'basic-look-and-feel)
