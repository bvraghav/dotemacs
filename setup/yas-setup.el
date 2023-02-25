(use-package yasnippet
  :ensure t

  :demand

  :hook (prog-mode . yas-minor-mode)

  :config
  (setq yas-prompt-functions
	'( yas-completing-prompt
           yas-ido-prompt
	   yas-x-prompt
	   yas-dropdown-prompt
	   yas-no-prompt))
  (yas-reload-all)
  (require 'create-python-command))

(use-package yasnippet-snippets
  :ensure t)

(provide 'yas-setup)
