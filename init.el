(package-initialize)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	;; ("org" . "https://orgmode.org/elpa/")
        ))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq confidential-file "~/.emacs.d/confidential-setup.el")
(when (file-exists-p confidential-file)
  (load confidential-file))

;; Elisp Path
(add-to-list 'load-path "~/.elisp")
(add-to-list 'load-path "~/.emacs.d/setup")
(add-to-list 'load-path "~/.emacs.d/myelpa")

;; Start using use-package
(unless (package-installed-p 'use-package)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'use-package))
(require 'use-package)

(require 'myelpa-setup)

(require 'basic-look-and-feel)
(require 'global-key-bindings)

(require 'gui-setup)

(require 'dired-setup)
(require 'dired-file-associations)
(require 'functions)
(require 'speedbar)
(require 'slug)
(require 'caffe-mode-setup)
(require 'c-headers)
(require 'rectangle-replace)
(require 'auto-insert-setup)
(require 'w3m-ext)
(require 'latex-setup)
(require 'plumb)
(require 'c-setup)
(require 'python-setup)
(require 'js-setup)
(require 'web-setup)
(require 'yas-setup)
(require 'magit-setup)

(require 'helm-setup)
(require 'projectile-setup)

(require 'org-setup)
(require 'gnus-setup)
(require 'project-explorer-setup)

;; (require 'icicles-setup)
;; (require 'slime-setup)
(require 'scheme-setup)

;; (require 'proxy-setup)
;; (require 'openwith-setup)

;; (require 'lsp-setup)

;; From .elisp
(require 'trivial-functions)
(require 'vue-mode-setup)

;; The last (Indic Setup)
(require 'indic-setup)

;; Org Roam Setup
(require 'org-roam-setup)
