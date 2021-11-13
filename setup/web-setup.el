;; CSS class completion
;; https://github.com/bvraghav/dotelisp
(require 'css-completion-minor-mode)

;; Indentation setup
(defun bvr-web-indentation()
  "Indent php to 2 spaces"
  (interactive)
  (setq tab-width 2
	c-basic-offset 2))

;; Php Mode
(use-package php-mode
  :ensure t
  :mode "\\.php\\'"
  :config
  (add-hook 'php-mode-hook #'bvr-web-indentation)
  (add-hook 'php-mode-hook #'css-completion-minor-mode))

;; ;; ;; Web mode
;; ;; (defun bvr-web-mode-indentation ()
;; ;;   "indent web mode with 2 spaces"
;; ;;   (interactive)
;; ;;   (setq tab-width 2
;; ;; 	c-basic-offset 2))
;; (use-package web-mode
;;   :ensure t
;;   :hook (web-mode . (bvr-web-indentation css-completion-minor-mode))

;;   :config
;;   ;; Indent Levels
;;   (setq apache-indent-level 2
;;         nginx-indent-level 2
;;         css-indent-offset 2))

;; Use mark multiple to edit sgml tag
(use-package sgml-mode
  :ensure t
  :hook ((sgml-mode . css-completion-minor-mode)
	 (sgml-mode . auto-fill-mode))
  :bind (:map sgml-mode-map
              ("C-c C-r" . rename-sgml-tag)
              ("C-c r" . rename-sgml-tag)))

(use-package emmet-mode
  :ensure t
  :hook (sgml-mode html-mode css-mode))

;; (require 'sgml-mode)
;; (define-key sgml-mode-map (kbd "C-c C-r") #'rename-sgml-tag)
;; (define-key sgml-mode-map (kbd "C-c r") #'rename-sgml-tag)
;; (add-hook 'sgml-mode-hook #'css-completion-minor-mode)

(provide 'web-setup)
