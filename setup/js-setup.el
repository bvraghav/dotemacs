(use-package js2-mode
  :ensure t

  :mode ("\\.[cm]?js\\'" "\\.json\\'")

  :bind (:map js2-mode-map
	      ("C-x C-e"	. js-send-last-sexp)
	      ("C-M-x"		. js-send-last-sexp-and-go)
	      ("C-c b"		. js-send-buffer)
	      ("C-x C-b"	. js-send-buffer-and-go)
	      ("C-x l"		. js-load-file-and-go))

  :hook ((js2-mode . auto-revert-mode))

  :config
  (setq js-indent-level 2
	js2-missing-semi-one-line-override t
	js2-strict-missing-semi-warning nil

	;; Org Babel for javascript
	org-babel-js-cmd "NODE_PATH=/home/bvr/.local/lib/node_modules node"

        ;; Node setup
        js-comint-program-arguments
        '("--experimental-json-modules" "--experimental-repl-await")))


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

(defcustom bvr/shell-source-command "source"
  "/bin/sh uses `\.' /bin/zsh uses `source'. Define based on
source")
(defcustom bvr/nvm-search-dirs-list
  '("~/.nvm" "~/.config/nvm")
  "List of dirs to search for nvm installation")
(defcustom bvr/default-nvm-dir nil "NVM_DIR")
(defcustom bvr/default-nodejs-repl-command nil "")
(defcustom bvr/nvm-node-args nil "Node REPL args")

(defun bvr/has-nvm-sh-p (dir)
  "Search if `nvm.sh' is a top level member of dir in FS"
  (file-exists-p (file-name-concat dir "nvm.sh")))
(defun bvr/not-has-nvm-sh-p (dir)
  "Negage #'bvr/has-nvm-sh-p"
  (not (bvr/has-nvm-sh-p dir)))
(defun bvr/find-nvm-dir (&optional path-list)
  "Search for nvm installation in the given PATH-LIST, which is
taken from 'bvr/nvm-search-dirs-list by default."
  (let* ((path-list (or path-list bvr/nvm-search-dirs-list))
         (found (-drop-while #'bvr/not-has-nvm-sh-p path-list)))
    (and found (first found))))
(defun bvr/nvm-dir ()
  "Get of find NVM_DIR"
  (expand-file-name
   (or bvr/default-nvm-dir (bvr/find-nvm-dir))))

(defun bvr/nvm-shell-cmd ()
  "Get nvm shell command"
  (or (getenv "NVM_DIR")
      (setenv "NVM_DIR"))
  (format "%s ${NVM_DIR}/nvm.sh ; nvm "
          bvr/shell-source-command))

(defun bvr/nvm-list-versions ()
  "List acceptable versions for nvm"
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
