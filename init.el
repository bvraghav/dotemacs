(package-initialize)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Elisp Path
(add-to-list 'load-path "~/.elisp")
(add-to-list 'load-path "~/.emacs.d/setup")

;; Desktop Path
(setq desktop-dirname "~/.emacs.d/dtp")

(require 'basic-look-and-feel)
(require 'org-setup)
(require 'plumb)
(require 'yas-setup)
(require 'c-setup)
(require 'web-setup)
(require 'projectile-setup)
(require 'gnus-setup)
(require 'latex-setup)

(require 'icicles-setup)
(require 'dired-setup)
(require 'functions)
(require 'c-headers)
(require 'rectangle-replace)
(require 'auto-insert-setup)
(require 'w3m-ext)
;; (require 'slime-setup)
(require 'speedbar)
(require 'slug)
(require 'caffe-mode-setup)

(require 'global-key-bindings)

(pinentry-start)
