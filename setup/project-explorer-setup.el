(use-package project-explorer
  :ensure t
  :hook (project-explorer-mode . bvr-disable-linum)
  :config
  (defun bvr-disable-linum ()
    (linum-mode -1)))

(provide 'project-explorer-setup)
