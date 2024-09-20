;; Run ggtags if projectile root folder available
;; ====================================================
(defun bvr-python-mode-hook ()
  (customize-set-variable 'indent-tabs-mode nil)
  ;; (ggtags-mode 1)
  (projectile-mode 1)
  (hs-minor-mode)
  (outline-minor-mode))

(use-package python
  :ensure t
  :hook (python-base-mode . bvr-python-mode-hook)
  :init
  (setq python-indent-offset 2)
  :bind
  (("C-M-t" . treemacs-select-window)
   ;; ("M-." . lsp-goto-type-definition)
   ;; ("M-]" . lsp-find-references)
   ))

(use-package conda
  :ensure t
  :defer t
  :init
  (setq conda-anaconda-home (expand-file-name "~/miniconda3")
        conda-env-home-directory (expand-file-name "~/miniconda3"))
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell))

(use-package jupyter
  :ensure t
  :defer t
  :init
  (setq org-babel-default-header-args:jupyter-python
        '((:async . "yes")
          (:session . "py")))
  :config
  ;; Push this to org-setup
  ;; (org-babel-jupyter-override-src-block "python")
  )

(use-package python-docstring
  :ensure t)

(provide 'python-setup)
