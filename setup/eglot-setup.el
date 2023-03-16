;;; eglot-setup --- Setting up eglot Language server

;;; Commentary:
;;; ---------------------------------------------------
;;; Setting up eglot Language server, along
;;; with flycheck

;;; Code:

(use-package eglot
  :ensure t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package bvr-flycheck-eslint)

(provide 'eglot-setup)
;;; eglot-setup.el ends here
