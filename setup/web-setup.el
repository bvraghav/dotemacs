;; Php mode
(require 'sgml-mode)

;; CSS class completion
(require 'css-completion-minor-mode)

(defun bvr-php-indentation()
  "Indent php to 2 spaces"
  (interactive)
  (setq tab-width 2
	c-basic-offset 2))
(add-hook 'php-mode-hook #'bvr-php-indentation)
(add-hook 'php-mode-hook #'css-completion-minor-mode)

;; Web mode
(defun bvr-web-mode-indentation ()
  "indent web mode with 2 spaces"
  (interactive)
  (setq tab-width 2
	c-basic-offset 2))
(add-hook 'web-mode-hook #'bvr-web-mode-indentation)
(add-hook 'web-mode-hook #'css-completion-minor-mode)

;; Indent Levels
(setq apache-indent-level 2
      nginx-indent-level 2
      css-indent-offset 2)

;; Use mark multiple to edit sgml tag
(define-key sgml-mode-map (kbd "C-c C-r") #'rename-sgml-tag)
(define-key sgml-mode-map (kbd "C-c r") #'rename-sgml-tag)
(add-hook 'sgml-mode-hook #'css-completion-minor-mode)

(provide 'web-setup)
