;;; helm-setup.el --- Setup Helm -*- lexical-binding: t -*-

;; Author: B.V. Raghav
;; Maintainer: B.V. Raghav
;; Version: 0.1
;; Package-Requires: nil
;; Homepage: https://github.com/bvraghav/dotemacs
;; Keywords: helm dotemacs


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

;; Setup helm.  Taken from other sources, but
;; attributed accordingly.

;;; Code:


;;; init-helm.el --- My startup file for helm. -*- lexical-binding: t -*-
;;; Source URI: https://github.com/thierryvolpiatto/emacs-tv-config/blob/master/init-helm.el
;;; Code:

;;; Set up helm first (will load helm-autoloads.el)

(require 'iso-transl)

(use-package dash :ensure t :demand)
(use-package f :ensure t :demand)
(use-package s :ensure t :demand)

(use-package helm
  :ensure t
  :after (dash f s)
  :config
  ;; (require 'helm-config)
  (setq helm-input-idle-delay                     0.01
        helm-reuse-last-window-split-state        t
        helm-always-two-windows                   t
        helm-split-window-inside-p                nil
        helm-commands-using-frame                 '(completion-at-point
                                                    helm-apropos
                                                    helm-eshell-prompts helm-imenu
                                                    helm-imenu-in-all-buffers)
        helm-actions-inherit-frame-settings       t
        helm-use-frame-when-more-than-two-windows t
        helm-use-frame-when-dedicated-window      t
        helm-frame-background-color               "DarkSlateGray"
        helm-show-action-window-other-window      'left
        helm-allow-mouse                          t
        helm-move-to-line-cycle-in-source         t
        helm-autoresize-max-height                80 ; it is %.
        helm-autoresize-min-height                20 ; it is %.
        helm-debug-root-directory                 "~/tmp/helm-debug"
        helm-follow-mode-persistent               t
        helm-candidate-number-limit               500)
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-info-bash)
  (helm-define-key-with-subkeys global-map (kbd "C-c n") ?n 'helm-cycle-resume))

;;; Load all autoloads for helm extensions
;;
;;
;; (load "/home/bvr/.elisp/helm-extensions/helm-extensions-autoloads.el")


(defun helm/debug-toggle ()
  (interactive)
  (setq helm-debug (not helm-debug))
  (message "Helm Debug is now %s"
           (if helm-debug "Enabled" "Disabled")))

(defun helm/eselect-grep ()
  (interactive)
  (when (y-or-n-p (format "Current grep program is %s, switching? "
                          (helm-grep-command)))
    (if (helm-grep-use-ack-p)
        (setq helm-grep-default-command
              "grep --color=always -d skip %e -n%cH -e %p %f"
              helm-grep-default-recurse-command
              "grep --color=always -d recurse %e -n%cH -e %p %f")
      (setq helm-grep-default-command
            "ack-grep -Hn --color --smart-case --no-group %e %p %f"
            helm-grep-default-recurse-command
            "ack-grep -H --color --smart-case --no-group %e %p %f"))
    (message "Switched to %s" (helm-grep-command))))

(defun helm/turn-on-header-line ()
  (interactive)
  (setq helm-echo-input-in-header-line t)
  (setq helm-split-window-in-side-p t)
  (helm-autoresize-mode -1)
  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
  )

(defun helm/turn-off-header-line ()
  (interactive)
  (setq helm-echo-input-in-header-line nil)
  ;;(helm-autoresize-mode 1)
  (setq helm-split-window-in-side-p nil)
  (remove-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
  )

(defun helm/occur-which-func ()
  (interactive)
  (with-current-buffer
      (or (helm-aif (with-helm-buffer
                      (window-buffer helm-persistent-action-display-window))
              (and (null (minibufferp it)) it))
          helm-current-buffer)
    (when (eq major-mode 'emacs-lisp-mode)
      (message "[%s]" (which-function)))))

(defun helm-find-files-in-frame ()
  (interactive)
  (with-helm-in-frame
    (call-interactively #'helm-find-files)))

(defun helm-M-x-in-frame ()
  (interactive)
  (with-helm-in-frame
    (call-interactively #'helm-M-x)))

(defun helm-occur-in-frame ()
  (interactive)
  (with-helm-in-frame
    (call-interactively #'helm-occur)))

(defun helm-mini-in-frame ()
  (interactive)
  (with-helm-in-frame
    (call-interactively #'helm-mini)))

(defun helm-do-grep-ag-in-frame ()
  (interactive)
  (with-helm-in-frame
    (call-interactively #'helm-do-grep-ag)))

(defun helm-do-git-grep-in-frame ()
  (interactive)
  (with-helm-in-frame
    (call-interactively #'helm-grep-do-git-grep)))

(defun helm-imenu-in-frame ()
  (interactive)
  (with-helm-in-frame
    (call-interactively #'helm-imenu)))

(defun helm-top-in-frame ()
  (interactive)
  (with-helm-in-frame
    (call-interactively #'helm-top)))


;;; Use-package declarations.
;;
;; Helm-mode is loading nearly everything.
(use-package helm-mode
  :init
  (add-hook 'helm-mode-hook
            (lambda ()
              (setq completion-styles
                    (cond ((assq 'helm-flex completion-styles-alist)
                           '(helm-flex)) ;; emacs-26.
                          ((assq 'flex completion-styles-alist)
                           '(flex)))))) ;; emacs-27+.
  :diminish (helm-mode " ⎈")
  :config
  (helm-mode 1)
  (setq helm-completing-read-handlers-alist
        '((xref-find-references . helm-completing-read-default-find-tag)
          (write-file . helm-read-file-name-handler-1)
          (basic-save-buffer . helm-read-file-name-handler-1)
          (find-tag . helm-completing-read-default-find-tag)
          (xref-find-definitions . helm-completing-read-default-find-tag)
          (xref-find-references . helm-completing-read-default-find-tag)
          (tmm-menubar)
          (mu4e-view-save-attachment-multi . helm-read-file-name-handler-1)
          (mu4e-view-save-attachment-single . helm-read-file-name-handler-1)
          (cancel-debug-on-entry)
          (org-capture . helm-org-completing-read-tags)
          (org-set-tags . helm-org-completing-read-tags)
          (dired-do-rename . helm-read-file-name-handler-1)
          (dired-do-copy . helm-read-file-name-handler-1)
          (dired-do-symlink . helm-read-file-name-handler-1)
          (dired-do-relsymlink . helm-read-file-name-handler-1)
          (dired-do-hardlink . helm-read-file-name-handler-1)
          (basic-save-buffer . helm-read-file-name-handler-1)
          (write-file . helm-read-file-name-handler-1)
          (write-region . helm-read-file-name-handler-1))))

(use-package helm-adaptive
  :after (helm)
  :config
  (setq helm-adaptive-history-file nil)
  (helm-adaptive-mode 1))

(use-package helm-utils
  :after (helm)
  :config
  ;; Popup buffer-name or filename in grep/moccur/imenu-all etc...
                                        ;(helm-popup-tip-mode 1)
  (setq helm-highlight-matches-around-point-max-lines   '(30 . 30)
        helm-window-show-buffers-function #'helm-window-mosaic-fn)
  (add-hook 'find-file-hook 'helm-save-current-pos-to-mark-ring))

(use-package helm-sys
  :after (helm)
  :commands (helm-top)
  :config (helm-top-poll-mode 1))

(use-package helm-info :after (helm))
;; :bind ("C-h r" . helm-info-emacs)

;; (use-package helm-ipython
;;              :config
;;              (use-package python
;;                           :config
;;                           (defun tv/bind-tab-in-inf-python ()
;;                             (define-key inferior-python-mode-map (kbd "C-i") 'helm-ipython-complete))
;;                           (define-key python-mode-map (kbd "<M-tab>") 'helm-ipython-complete)
;;                           (add-hook 'inferior-python-mode-hook 'tv/bind-tab-in-inf-python)))

(use-package helm-ring
  :after (helm)
  :config
  (setq helm-kill-ring-threshold 1)
  ;; Action for helm kill-ring
  (defun helm/emamux:copy-from-kill-ring (candidate)
    (require 'emamux)
    (emamux:check-tmux-running)
    (when (null kill-ring)
      (error "kill-ring is nil!!"))
    (emamux:set-buffer candidate 0))
  (add-to-list 'helm-kill-ring-actions '("Emamux copy" . helm/emamux:copy-from-kill-ring) t)
  :bind (:map helm-kill-ring-map
              ("C-d" . helm-kill-ring-run-persistent-delete)))

(use-package helm-recoll
  :after (helm)
  :disabled t
  :commands helm-recoll
  ;; Use the HFF actions to setup directories, then run
  ;; "recoll index -c DIR" on each directory. 
  :init (customize-set-variable 'helm-recoll-directories
                                '(("work" . "~/.recoll-work"))))

(use-package helm-ls-git
  :after (helm)
  :ensure t
  :config
  ;; Use `magit-status-setup-buffer' instead of
  ;; `magit-status-internal' with recent magit.
  (setq helm-ls-git-status-command 'magit-status-internal)
  (cl-defmethod helm-setup-user-source ((source helm-ls-git-source))
    (helm-source-add-action-to-source-if
     "Magit find file"
     (lambda (candidate)
       (magit-find-file (magit-branch-or-commit-at-point) candidate))
     source
     (lambda (_candidate)
       ;; For `magit-branch-or-commit-at-point'.
       (require 'magit-git)
       (with-helm-current-buffer (magit-branch-or-commit-at-point)))
     1)))

(use-package helm-buffers
  :after (helm)
  :config
  (setq helm-buffers-favorite-modes
        (append helm-buffers-favorite-modes '(picture-mode artist-mode))
        helm-buffers-fuzzy-matching       t
        helm-buffer-skip-remote-checking  t
        helm-buffer-max-length            22
        helm-buffers-end-truncated-string "…"
        helm-buffers-maybe-switch-to-tab  t
        helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-buffer-not-found)
        helm-boring-buffer-regexp-list
        '("\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf"
          "\\`\\*Messages" "\\`\\*Magit" "\\`\\*git-gutter" "\\`\\*Help"))

  (define-key helm-buffer-map (kbd "C-d") 'helm-buffer-run-kill-persistent)
  
  (cl-defmethod helm-setup-user-source ((source helm-source-buffers))
    "Adds additional actions to `helm-source-buffers-list'.
- Magit status."
    (setf (slot-value source 'candidate-number-limit) 300)
    (helm-aif (slot-value source 'action)
        (setf (slot-value source 'action)
              (helm-append-at-nth
               (if (symbolp it)
                   (symbol-value it)
                 it)
               '(("Diff buffers" . helm-buffers-diff-buffers)) 4)))
    (helm-source-add-action-to-source-if
     "Magit status"
     (lambda (candidate)
       (funcall helm-ls-git-status-command
                (with-current-buffer candidate default-directory)))
     source
     (lambda (candidate)
       (locate-dominating-file (with-current-buffer candidate default-directory)
                               ".git"))
     1)))

(use-package helm-files
  :after (helm)
  :config
  (setq helm-ff-auto-update-initial-value        t
        helm-ff-allow-non-existing-file-at-point t
        helm-trash-remote-files                  t
        helm-dwim-target                         'next-window
        helm-ff-cache-mode-post-delay            0.3
        helm-ff-refresh-cache-delay              0.3
        ;; helm-ff-cache-mode-lighter-sleep         " 💡"
        ;; helm-ff-cache-mode-lighter-updating      " 💀"
        )
  (customize-set-variable 'helm-ff-lynx-style-map t)
  (define-key helm-read-file-map (kbd "RET") 'helm-ff-RET)
  (define-key helm-find-files-map (kbd "C-i") nil)
  (define-key helm-find-files-map (kbd "C-/") 'helm-ff-run-find-sh-command)
  (define-key helm-find-files-map (kbd "C-d") 'helm-ff-persistent-delete)

  (defun helm/insert-date-in-minibuffer ()
    (interactive)
    (with-selected-window (or (active-minibuffer-window)
                              (minibuffer-window))
      (unless (or (helm-follow-mode-p)
                  helm--temp-follow-flag)
        (goto-char (point-max))
        (insert (format-time-string "%Y-%m-%d-%H:%M")))))
  (define-key helm-find-files-map (kbd "C-c y") 'helm/insert-date-in-minibuffer)
  (define-key helm-read-file-map (kbd "C-c y") 'helm/insert-date-in-minibuffer)

  (defun helm/ff-candidates-lisp-p (candidate)
    (cl-loop for cand in (helm-marked-candidates)
             always (string-match "\\.el$" cand)))

  (defun helm-ff-recoll-index-directory (directory)
    "Create a recoll index directory from DIRECTORY.
Add the new created directory to `helm-recoll-directories' using the
basename of DIRECTORY as name.
By using `customize-set-variable', a new source is created for this
new directory."
    (cl-assert (boundp 'helm-recoll-directories) nil
               "Package helm-recoll not installed or configured")
    (let* ((bn (helm-basename (expand-file-name directory)))
           (index-dir (format "~/.recoll-%s" bn))
           (conf-file (expand-file-name "recoll.conf" index-dir)))
      (mkdir index-dir)
      (with-current-buffer (find-file-noselect conf-file)
        (insert (format "topdirs = %s" (expand-file-name directory)))
        (save-buffer)
        (kill-buffer))
      (customize-set-variable 'helm-recoll-directories
                              (append `((,bn . ,index-dir)) helm-recoll-directories))
      (message "Don't forget to index config directory with 'recollindex -c %s'" index-dir)))

  (defun helm-ff-recoll-index-directories (_candidate)
    (let ((dirs (helm-marked-candidates)))
      (cl-loop for dir in dirs
               when (file-directory-p dir)
               do (helm-ff-recoll-index-directory dir))))

  ;; Add actions to `helm-source-find-files' IF:
  (cl-defmethod helm-setup-user-source ((source helm-source-ffiles))
    "Adds additional actions to `helm-find-files'.
    - Byte compile file(s) async
    - Byte recompile directory
    - Magit status
    - Github issues
    - Patch region on directory
    - Open in emms
    - Update directory autoloads
    - Recoll directory creation"
    (helm-source-add-action-to-source-if
     "Byte compile file(s) async"
     (lambda (_candidate)
       (cl-loop for file in (helm-marked-candidates)
                do (async-byte-compile-file file)))
     source
     'helm/ff-candidates-lisp-p)
    (helm-source-add-action-to-source-if
     "Recover file"
     (lambda (candidate)
       (recover-file candidate))
     source
     (lambda (candidate)
       (file-exists-p (expand-file-name
                       (format "#%s#" (helm-basename candidate))
                       (helm-basedir candidate)))))
    (helm-source-add-action-to-source-if
     "Byte recompile directory (async)"
     'async-byte-recompile-directory
     source
     'file-directory-p)
    (helm-source-add-action-to-source-if
     "Magit status"
     (lambda (_candidate)
       (funcall helm-ls-git-status-command
                helm-ff-default-directory))
     source
     (lambda (candidate)
       (and (not (string-match-p ffap-url-regexp candidate))
            helm-ff-default-directory
            (locate-dominating-file helm-ff-default-directory ".git")))
     1)
    (helm-source-add-action-to-source-if
     "Patch region on directory"
     (lambda (_candidate)
       (with-helm-current-buffer
         (shell-command-on-region (region-beginning) (region-end)
                                  (format "patch -d %s -p1"
                                          helm-ff-default-directory))))
     source
     (lambda (_candidate)
       (with-helm-current-buffer
         (and (or (eq major-mode 'mu4e-view-mode)
                  (eq major-mode 'diff-mode))
              (region-active-p))))
     1)
    (helm-source-add-action-to-source-if
     "Open in emms"
     (lambda (candidate)
       (if (file-directory-p candidate)
           (emms-play-directory candidate)
         (emms-play-file candidate)))
     source
     (lambda (candidate)
       (or (and (file-directory-p candidate)
                (directory-files
                 candidate
                 nil ".*\\.\\(mp3\\|ogg\\|flac\\)$" t))
           (string-match-p ".*\\.\\(mp3\\|ogg\\|flac\\)$" candidate)))
     1)
    (helm-source-add-action-to-source-if
     "Update directory autoloads"
     (lambda (candidate)
       (require 'autoload)
       (let ((default-directory helm-ff-default-directory)
             (generated-autoload-file
              (read-file-name "Write autoload definitions to file: "
                              helm-ff-default-directory
                              nil nil nil
                              (lambda (f)
                                (string-match "autoloads\\|loaddefs" f)))))
         (cl-letf (((symbol-function 'autoload-generated-file)
                    (lambda ()
                      (expand-file-name generated-autoload-file default-directory))))
           (update-directory-autoloads (expand-file-name candidate)))))
     source
     (lambda (candidate)
       (and (file-directory-p candidate)
            (string= (helm-basename candidate) ".")))
     1)
    (helm-source-add-action-to-source-if
     "Recoll index directory"
     'helm-ff-recoll-index-directories
     source
     'file-directory-p
     3)
    (helm-source-add-action-to-source-if
     "Epa encrypt file"
     (lambda (candidate)
       (require 'epg) (require 'epa)
       (epa-encrypt-file candidate
                         (helm :sources (helm-build-sync-source
                                            "Select recipient for encryption: "
                                          :persistent-action 'ignore
                                          :candidates 'helm-epa-get-key-list))))
     source
     'file-exists-p
     3)))

(use-package helm-descbinds
  :after (helm)
  :ensure t
  :config
  ;; C-h b, C-x C-h etc...
  (helm-descbinds-mode 1))

(use-package helm-lib
  :after (helm)
  :config
  (setq helm-scroll-amount 4)
  (helm-help-define-key "C-x" 'exchange-point-and-mark)
  (helm-help-define-key "C-l" 'recenter-top-bottom))

(use-package helm-net
  :after (helm)
  :config
  (setq helm-net-prefer-curl           t
        helm-surfraw-duckduckgo-url    "https://duckduckgo.com/?q=%s&ke=-1&kf=fw&kl=fr-fr&kr=b&k1=-1&k4=-1"
        helm-google-suggest-search-url helm-surfraw-duckduckgo-url))

(use-package helm-external
  :after (helm)
  :config
  (setq helm-raise-command                 "wmctrl -xa %s"
        helm-default-external-file-browser "thunar"))

(use-package helm-grep
  :after (helm)
  :bind  (("C-z C-x" . bvr/helm-grep)
	  :map helm-grep-map
          ("C-M-a" . 'helm/occur-which-func))

  :hook (helm-grep-mode . hl-line-mode)

  :config
  (setq helm-pdfgrep-default-read-command
        "evince --page-label=%p '%f'"
        helm-grep-default-command
        "ack-grep -Hn --color --smart-case --no-group %e %p %f"
        helm-grep-default-recurse-command
        "ack-grep -H --color --smart-case --no-group %e %p %f"
        helm-grep-ag-command
        "rg --color=always --colors 'match:bg:yellow' --colors 'match:fg:black' --smart-case --no-heading --line-number %s %s %s"
        helm-grep-ag-pipe-cmd-switches
        '("--colors 'match:bg:yellow' --colors 'match:fg:black'")
        helm-grep-git-grep-command
        "git --no-pager grep -n%cH --color=always --exclude-standard --no-index --full-name -e %p -- %f")

  (defun bvr/helm-grep (arg)
    (interactive "P")
    (call-interactively
     (if arg #'helm-grep-do-git-grep
       #'helm-do-grep-ag))))

(use-package helm-occur
  :after (helm)
  :config
  (add-hook 'helm-occur-mode-hook 'hl-line-mode)
  (define-key helm-occur-map (kbd "C-M-a") 'helm/occur-which-func))

(use-package helm-elisp
  :after (helm)
  :config
  (setq helm-show-completion-display-function #'helm-display-buffer-in-own-frame
        helm-apropos-fuzzy-match              t
        helm-lisp-fuzzy-completion            t)
  (helm-multi-key-defun helm-multi-lisp-complete-at-point
      "Multi key function for completion in emacs lisp buffers.
First call indent, second complete symbol, third complete fname."
    '(helm-lisp-indent
      helm-lisp-completion-at-point
      helm-complete-file-name-at-point)
    0.3)
  (define-key emacs-lisp-mode-map (kbd "TAB") 'helm-multi-lisp-complete-at-point)
  (define-key lisp-interaction-mode-map (kbd "TAB") 'helm-multi-lisp-complete-at-point))

(use-package helm-locate
  :after (helm)
  :config
  (setq helm-locate-fuzzy-match t))

(use-package helm-org
  :after (helm org)
  :ensure t
  :config
  (setq helm-org-headings-fontify t))

(use-package helm-emms
  :after (helm)
  :after 'emms
  :config
  (setq helm-emms-use-track-description-function nil))

(use-package helm-find
  :after (helm)
  :config
  (setq helm-find-noerrors t))

;; Obsoleted because of new helm-packages manager
;; ----------------------------------------------------
;; https://github.com/emacs-helm/helm/blob/b4fe77174e09537a58077c24054f14fa99cad42e/NEWS.org#new-helm-packages-manager
;; ----------------------------------------------------
;; (use-package helm-elisp-package
;;   :after (helm)
;;   :config
;;   (setq helm-el-package-autoremove-on-start t
;;         helm-el-package-upgrade-on-start t))
;; ----------------------------------------------------
(use-package helm-packages
  :after (helm))

(use-package helm-imenu
  :after (helm)
  :config
  (add-to-list 'helm-imenu-type-faces '("^Use package$" . font-lock-keyword-face))
  (customize-set-variable 'helm-imenu-lynx-style-map t))

;; (use-package helm-apt
;;              :config
;;              ;; (defun helm-apt/show-mode-hook-fn ()
;;              ;;   (font-lock-add-keywords nil '(("^\\(.*: \\).*" 1 '((:foreground "Darkslategray1")))))
;;              ;;   (font-lock-add-keywords nil '(("^\\(.*: \\)\\(.*\\)" 2 '((:foreground "DarkOrange")))))
;;              ;;   (font-lock-add-keywords nil '(("\\(https?://\\)\\(.*\\)" 0 '((:foreground "#73d216" :weight bold :underline t)))))
;;              ;;   (goto-char (point-min))
;;              ;;   (let ((map (make-sparse-keymap))
;;              ;;         (inhibit-read-only t))
;;              ;;     (define-key map (kbd "RET") 'browse-url-at-point)
;;              ;;     (while (re-search-forward "https?://.*" nil t)
;;              ;;       (add-text-properties
;;              ;;        (match-beginning 0) (match-end 0)
;;              ;;        `(keymap ,map help-echo "Browse url")))))
;;              ;; (add-hook 'helm-apt-show-mode-hook 'helm-apt/show-mode-hook-fn)
;;              )

(use-package helm-misc
  :after (helm)
  :config
  ;; Minibuffer history (Rebind to M-s).
  (customize-set-variable 'helm-minibuffer-history-key [remap next-matching-history-element])
  (helm-epa-mode 1))



;; ----------------------------------------------------
;; Helm Colors
;; ----------------------------------------------------
;; Insert Helm Color
(defun bvr/helm/insert-color-at-point-or-region (arg)
  "Insert Color Name with interactive selector
by helm-colors. With prefix ARG insert RGB instead."
  (interactive "P")
  (let* ((beg (use-region-beginning))
         (end (use-region-end))
         (color-name (helm-colors))
         (color (if (not arg) color-name
                  (bvr/named-color-as-rgb color-name))))
    (and beg (delete-region beg end))
    (insert color)))

(defun bvr/insert-named-color-as-rgb (arg)
  "Insert COLOR-NAME as hex RGB value.  Prompt for color name if
not marked with active region, or numeric prefix ARG."
  (interactive "*P")
  (pcase-let*
      ((color-name (or (and (use-region-p)
                            (not arg)
                            (delete-and-extract-region
                             (use-region-beginning)
                             (use-region-end)))
                       (read-string "Color Name: "))))
    (insert (bvr/named-color-as-rgb color-name))))

(defun bvr/named-color-as-rgb (color-name &optional prefix)
  "Convert COLOR-NAME to hex RGB; Optionally prefixed with #. Use
empty string to inhibit prefix"
  (pcase-let* ((`(,r ,g ,b)
                (mapcar (lambda (x) (ash x -8))
                        (color-values color-name)))
               (p (or prefix "#")))
    (format "%s%02x%02x%02x" p r g b)))
;; ----------------------------------------------------

;; ----------------------------------------------------
;; Insert org entity
;;
;; PS: Manually install-helm-insert-org-entity from
;; dotelisp: https://github.com/bvraghav/dotelisp.git.
;; ----------------------------------------------------
(use-package bvr-helm-insert-org-entity
  :bind (:map iso-transl-ctl-x-8-map
              ("TAB" . bvr/helm-insert-org-entity)))
;; ----------------------------------------------------

;;; Ctl-x-5 map
;;
(define-key ctl-x-5-map (kbd "C-x c t") 'helm-top-in-frame)
(define-key ctl-x-5-map (kbd "C-x c i") 'helm-imenu-in-frame)
(define-key ctl-x-5-map (kbd "C-x C-f") 'helm-find-files-in-frame)
(define-key ctl-x-5-map (kbd "M-x")     'helm-M-x-in-frame)
(define-key ctl-x-5-map (kbd "C-s")     'helm-occur-in-frame)
(define-key ctl-x-5-map (kbd "C-x C-b") 'helm-mini-in-frame)
(define-key ctl-x-5-map (kbd "M-g a")   'helm-do-grep-ag-in-frame)
(define-key ctl-x-5-map (kbd "M-g g")   'helm-do-git-grep-in-frame)


;;; Helm-command-map
;;
;;
(define-key helm-command-map (kbd "g") 'helm-apt)
(define-key helm-command-map (kbd "z") 'helm-complex-command-history)
;; (define-key helm-command-map (kbd "w") 'helm-w3m-bookmarks)
;; (define-key helm-command-map (kbd "x") 'helm-firefox-bookmarks)
;; (define-key helm-command-map (kbd "#") 'helm-emms)
(define-key helm-command-map (kbd "I") 'helm-imenu-in-all-buffers)
;; (define-key helm-command-map (kbd "@") 'helm-list-elisp-packages-no-fetch)

;;; Global-map
;;
;;
(global-set-key (kbd "C-M-j")                        'helm-filtered-bookmarks)
(global-set-key (kbd "M-x")                          'undefined)
(global-set-key (kbd "M-x")                          'helm-M-x)
(global-set-key (kbd "M-y")                          'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f")                      'helm-find-files)
(global-set-key (kbd "C-z C-f")                      'helm-for-files)
(global-set-key (kbd "C-c <SPC>")                    'helm-all-mark-rings)
(global-set-key [remap bookmark-jump]                'helm-filtered-bookmarks)
(global-set-key (kbd "C-:")                          'helm-eval-expression-with-eldoc)
(global-set-key (kbd "C-,")                          'helm-calcul-expression)
;; (global-set-key (kbd "C-h d")                        'helm-info-at-point)
;; (global-set-key (kbd "C-h i")                        'helm-info)
(global-set-key (kbd "C-x C-d")                      'helm-browse-project)
(global-set-key (kbd "<f1>")                         'helm-resume)
(global-set-key (kbd "C-h C-f")                      'helm-apropos)
(global-set-key (kbd "C-h a")                        'helm-apropos)
(global-set-key (kbd "C-h C-d")                      'helm-debug-open-last-log)
(global-set-key (kbd "<f5> s")                       'helm-find)
(global-set-key (kbd "S-<f2>")                       'helm-execute-kmacro)
(global-set-key (kbd "C-c i")                        'helm-imenu-in-all-buffers)
(global-set-key (kbd "C-c C-i")                      'helm-imenu)
(global-set-key (kbd "<f11>")                        nil)
(global-set-key (kbd "<f11> o")                      'helm-org-agenda-files-headings)
(global-set-key (kbd "C-s")                          'helm-occur-visible-buffers)
;; (global-set-key (kbd "<f6> h")                       'helm-emms)
(define-key global-map [remap jump-to-register]      'helm-register)
(define-key global-map [remap list-buffers]          'helm-mini)
(define-key global-map [remap dabbrev-expand]        'helm-dabbrev)
;; (define-key global-map [remap find-tag]              'helm-etags-select)
;; (define-key global-map [remap xref-find-definitions] 'helm-etags-select)
(define-key global-map (kbd "M-g a")                 'helm-do-grep-ag)
(define-key global-map (kbd "M-g g")                 'helm-grep-do-git-grep)
;; (define-key global-map (kbd "M-g i")                 'helm-gid) ; Code browsing utility. Use GNU Global instead
(define-key global-map (kbd "C-x r p")               'helm-projects-history)
;; (define-key global-map (kbd "C-x r c")               'helm-addressbook-bookmarks)
(define-key global-map (kbd "C-c t r")               'helm-dictionary)
(define-key global-map (kbd "C-x c c")               #'bvr/helm/insert-color-at-point-or-region)
(define-key global-map (kbd "C-#")                   #'helm-command-prefix)
(define-key global-map (kbd "C-# SPC")               #'helm-multi-files)

;; Indent or complete with completion-at-point
;; (setq tab-always-indent 'complete)

;; (define-key global-map (kbd "<backtab>") 'completion-at-point)

;; Avoid hitting forbidden directories when using find.
(add-to-list 'completion-ignored-extensions ".gvfs/")
(add-to-list 'completion-ignored-extensions ".dbus/")
(add-to-list 'completion-ignored-extensions "dconf/")

;;; init-helm.el ends here

(provide 'helm-setup)

;;; helm-setup.el ends here

