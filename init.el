(package-initialize)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
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

;; Bootstrap
;; ----------------------------------------------------
(use-package sqlite3 :ensure t)

(require 'myelpa-setup)

(require 'basic-look-and-feel)
(require 'global-key-bindings)
(require 'abbrev-setup)

(require 'gui-setup)

(require 'docker-setup)
(require 'dired-setup)
(require 'dired-file-associations)
(require 'functions)
(require 'recentf-setup)
(require 'speedbar)
(require 'slug)
(require 'caffe-mode-setup)
(require 'c-headers)
(require 'rectangle-replace)
(require 'auto-insert-setup)
;; Obsoleted.
;; (require 'w3m-ext)
(require 'hs-setup)
(require 'vcard-setup)

(require 'plumb)
(require 'gnuplot-setup)
(require 'c-setup)
(require 'python-setup)
(require 'js-setup)
(require 'typescript-setup)
(require 'web-setup)
(require 'yas-setup)
(require 'magit-setup)

(require 'org-setup)

(require 'helm-setup)
(require 'projectile-setup)

(require 'latex-setup)                  ;depends upon helm

(require 'gnus-setup)
(require 'project-explorer-setup)

;; (require 'slime-setup)
(require 'scheme-setup)

(require 'eglot-setup)

;; From .elisp
(require 'trivial-functions)
(require 'vue-mode-setup)

;; Unicode setup including CJK
(require 'unicode-setup)

;; Org Roam Setup
(use-package org-roam-setup)

;; Org CV Setup
(use-package org-cv-setup)

;; Markdown Mode Setup
(use-package markdown-setup)

;; Arduino Setup
(use-package arduino-setup)

;; Julia Setup
(use-package julia-setup)

;; Notdeft Setup
(use-package notdeft-setup
  :after org-roam-setup)

(provide 'init)
;;; init.el ends here
