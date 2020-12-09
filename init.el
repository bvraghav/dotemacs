(package-initialize)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq confidential-file "~/.emacs.d/confidential-setup.el")
(when (file-exists-p confidential-file)
  (load confidential-file))

;; Elisp Path
(add-to-list 'load-path "~/.elisp")
(add-to-list 'load-path "~/.emacs.d/setup")
(add-to-list 'load-path "~/.emacs.d/myelpa")

(require 'myelpa-setup)

(require 'basic-look-and-feel)
(require 'indic-setup)
(require 'org-setup)
(require 'plumb)
(require 'yas-setup)
(require 'c-setup)
(require 'python-setup)
(require 'js-setup)
(require 'web-setup)
(require 'projectile-setup)
(require 'gnus-setup)
(require 'latex-setup)
(require 'project-explorer-setup)

;; (require 'icicles-setup)
(require 'helm-setup)
(require 'dired-setup)
(require 'functions)
(require 'c-headers)
(require 'rectangle-replace)
(require 'auto-insert-setup)
(require 'w3m-ext)
(require 'slime-setup)
(require 'speedbar)
(require 'slug)
(require 'caffe-mode-setup)
(require 'scheme-setup)

(require 'proxy-setup)
;; (require 'openwith-setup)

;; (require 'lsp-setup)

(require 'global-key-bindings)

;; From .elisp
(require 'trivial-functions)
(require 'vue-mode-setup)

(pinentry-start)
