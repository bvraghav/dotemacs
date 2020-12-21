;; Run ggtags if projectile root folder available
;; ====================================================
(defun bvr-run-ggtags-may-be ()
  (interactive)
  nil)

(defun bvr-python-mode-hook ()
  (customize-set-variable 'indent-tabs-mode nil)
  (ggtags-mode 1)
  (projectile-mode 1))

(add-hook 'python-mode-hook #'bvr-python-mode-hook)

(use-package conda
  :defer t
  :init
  (setq conda-anaconda-home (expand-file-name "~/miniconda3")
        conda-env-home-directory (expand-file-name "~/miniconda3"))
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell))

(use-package jupyter
  :defer t
  :init
  (setq org-babel-default-header-args:jupyter-python
        '((:async . "yes")
          (:session . "py")))
  :config
  (org-babel-jupyter-override-src-block "python"))

(provide 'python-setup)
