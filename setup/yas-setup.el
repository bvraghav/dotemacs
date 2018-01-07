
;; Yasnippet Completion Prompts
(setq yas-prompt-functions
      '(yas-ido-prompt
	yas-completing-prompt
	yas-x-prompt
	yas-dropdown-prompt
	yas-no-prompt))

;; Programming modes
(add-hook 'prog-mode-hook 'yas-minor-mode)

(provide 'yas-setup)
