;;; js-setup.el --- Setting up JS handling.

;;; Commentary:
;;; ---------------------------------------------------
;;; Setting up JS handling.
;;;
;;; Use flycheck; setup the executable.
;;;
;;; Disable flymake.

;;; Code:

(use-package js2-mode
  :ensure t

  :mode "\\.[cm]?js\\'"

  :bind (:map js2-mode-map
	      ("C-x C-e"	. js-send-last-sexp)
	      ("C-M-x"		. js-send-last-sexp-and-go)
	      ("C-c b"		. js-send-buffer)
	      ("C-x C-b"	. js-send-buffer-and-go)
	      ("C-x l"		. js-load-file-and-go))

  :hook ((js2-mode . auto-revert-mode)
         (js2-mode . bvr/set-local-flycheck-eslint-maybe)
         ;; bvr/disable-flymake
         (js2-mode . bvr/js2/code-fold-setup))

  ;; js2-mode provides 4 level of syntax
  ;; highlighting. They are
  ;; --------------------------------------------------
  ;; 0 or a negative value means none.
  ;; 1 adds basic syntax highlighting.
  ;; 2 adds highlighting of some Ecma built-in properties.
  ;; 3 adds highlighting of many Ecma built-in functions.
  :config
  (setq js-indent-level 2
        js2-highlight-level 3
	js2-missing-semi-one-line-override t
	js2-strict-missing-semi-warning nil

	;; Org Babel for javascript
	org-babel-js-cmd "NODE_PATH=/home/bvr/.local/lib/node_modules node"

        ;; Node setup
        js-comint-program-arguments
        '("--experimental-json-modules" "--experimental-repl-await")))

(defun bvr/js2/code-fold-setup ()
  "Setup code folding for js2-mode."
  (hs-minor-mode)
  (hs-hide-all))

(defun bvr/disable-flymake ()
  "Disable Flymake minor mode."
  (setq flymake-mode nil))

(defun bvr/set-local-flycheck-eslint-maybe ()
  "Set local eslint may be as executable."
  (interactive)
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root (expand-file-name
                            "./node_modules/.bin/eslint"
                            root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable
                  eslint)
      (setq-local flycheck-bvr-javascript-eslint-executable
                  eslint))))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :config
  (setq js-indent-level 2))

;; Magit-like for NPM
(use-package npm
    :ensure t)

;; ----------------------------------------------------
;; Setting up NVM Convenience functions
;; ----------------------------------------------------

;; Example defcustom
;; -----------------------------------
;; (defcustom frobnicate-automatically nil
;;        "Non-nil means automatically frobnicate all buffers."
;;        :type 'boolean
;;        :require 'frobnicate-mode
;;        :group 'frobnicate)

(defgroup bvr/js-setup nil "For BVR javascript setup." :version "v1.0")
(defcustom bvr/shell-source-command "source"
  "/bin/sh uses `\.' /bin/zsh uses `source'.   Define based on source."
  :type '(string)
  :group 'bvr/js-setup)
(defcustom bvr/nvm-search-dirs-list
  '("~/.nvm" "~/.config/nvm")
  "List of dirs to search for nvm installation."
  :type '(list string)
  :group 'bvr/js-setup)
(defcustom bvr/default-nvm-dir nil "NVM_DIR."
  :type '(string)
  :group 'bvr/js-setup)
(defcustom bvr/default-nodejs-repl-command nil
  "Default NodeJS REPL command."
  :type '(string)
  :group 'bvr/js-setup)
(defcustom bvr/nvm-node-args nil "Node REPL args."
  :type '(string)
  :group 'bvr/js-setup)

(defun bvr/has-nvm-sh-p (dir)
  "Search if `nvm.sh' is a top level member of DIR in fs."
  (file-exists-p (file-name-concat dir "nvm.sh")))
(defun bvr/not-has-nvm-sh-p (dir)
  "Negate #'bvr/has-nvm-sh-p for DIR."
  (not (bvr/has-nvm-sh-p dir)))
(defun bvr/find-nvm-dir (&optional path-list)
  "Search for nvm installation in the given PATH-LIST.
PATH_LIST is taken from 'bvr/nvm-search-dirs-list by default."
  (let* ((path-list (or path-list bvr/nvm-search-dirs-list))
         (found (-drop-while #'bvr/not-has-nvm-sh-p path-list)))
    (and found (first found))))
(defun bvr/nvm-dir ()
  "Get or find NVM_DIR."
  (expand-file-name
   (or bvr/default-nvm-dir (bvr/find-nvm-dir))))

(defun bvr/nvm-shell-cmd ()
  "Get nvm shell command."
  (or (getenv "NVM_DIR")
      (setenv "NVM_DIR"))
  (format "%s ${NVM_DIR}/nvm.sh ; nvm "
          bvr/shell-source-command))

(defun bvr/nvm-list-versions ()
  "List acceptable versions for nvm."
  (let* ((nvm-versions-cmd
          (format "%s list --no-colors"
                  (bvr/nvm-shell-cmd)))
         (vraw (string-split (shell-command-to-string
                              nvm-versions-cmd)
                             "\n"))
         (vtrim (mapcar #'string-trim vraw))
         (vsplit (mapcar #'string-split vtrim))
         (vdrop (mapcar (lambda (splits)
                          (-drop-while (lambda (s) (string-prefix-p "-" s))
                                       splits))
                        vsplit))
         (versions (mapcar #'first vdrop)))
    versions))
;; ----------------------------------------------------


;; ----------------------------------------------------
;; Indium Setup
;; ----------------------------------------------------
;; Indium does not work. Never add this.
;;(use-package indium :ensure t)


;; (defun bvr/ts-setup ()
;;   (auto-revert-mode 1)
;;   (ggtags-mode 1))
;; (add-hook 'typescript-mode-hook #'bvr/ts-setup)

(provide 'js-setup)
;;; js-setup.el ends here
