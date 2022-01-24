(setq lsp-keymap-prefix "C-S-l")

(use-package lsp-mode
  :ensure t
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)

;; Python Language Server
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package lsp-ui :ensure t :commands lsp-ui-mode)
(use-package helm-lsp :ensure t :commands lsp-ui-mode)
(use-package lsp-treemacs :ensure t :commands lsp-treemacs-errors-list)
(use-package which-key :ensure t :config (which-key-mode))

(provide 'lsp-setup)
