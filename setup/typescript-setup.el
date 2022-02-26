
(use-package tide
  :ensure t
  :hook (before-save . tide-format-before-save))

(use-package typescript-mode
  :ensure t
  :after tide

  :hook (typescript-mode . setup-tide-mode)

  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1))

  (setq typescript-indent-level 2))

(provide 'typescript-setup)
