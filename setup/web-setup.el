;; Php mode
(defun bvr-php-indentation()
  "Indent php to 2 spaces"
  (interactive)
  (setq tab-width 2
	c-basic-offset 2))
(add-hook 'php-mode-hook #'bvr-php-indentation)

;; Web mode
(defun bvr-web-mode-indentation ()
  "indent web mode with 2 spaces"
  (interactive)
  (setq tab-width 2
	c-basic-offset 2))
(add-hook 'web-mode-hook #'bvr-web-mode-indentation)

;; Apache Indent Level
(setq apache-indent-level 2)

(provide 'web-setup)
