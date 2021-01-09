(use-package js2-mode
  :ensure t

  :mode "\\.[cm]?js\\'"

  :bind (:map js2-mode-map
	      ("C-x C-e"	. js-send-last-sexp)
	      ("C-M-x"		. js-send-last-sexp-and-go)
	      ("C-c b"		. js-send-buffer)
	      ("C-x C-b"	. js-send-buffer-and-go)
	      ("C-x l"		. js-load-file-and-go))

  :hook ((js2-mode . auto-revert-mode))

  :config
  (setq js-indent-level 2
	js2-missing-semi-one-line-override t
	js2-strict-missing-semi-warning nil
	;; Org Babel for javascript
	org-babel-js-cmd "NODE_PATH=/home/bvr/.local/lib/node_modules node"))

(use-package typescript-mode
  :ensure t

  :hook ((typescript-mode . auto-revert-mode)
	 (typescript-mode . ggtags-mode)))


;; (require 'js2-mode)

;; ;; ES6 based javascript mode
;; ;; (add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (add-to-list 'auto-mode-alist '("\\.cjs\\'" . js2-mode))
;; (add-to-list 'auto-mode-alist '("\\.mjs\\'" . js2-mode))

;; (defun bvr/js2-key-bindings ()
;;   (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
;;   (local-set-key (kbd "C-M-x")   'js-send-last-sexp-and-go)
;;   (local-set-key (kbd "C-c b")   'js-send-buffer)
;;   (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)
;;   (local-set-key (kbd "C-c l")   'js-load-file-and-go)
;;   (message "Added bvr/js2-key-bindings"))

;; (add-hook 'js2-mode-hook #'bvr/js2-key-bindings)

;; (add-hook 'js2-mode-hook #'auto-revert-mode)

;; (defun bvr/ts-setup ()
;;   (auto-revert-mode 1)
;;   (ggtags-mode 1))
;; (add-hook 'typescript-mode-hook #'bvr/ts-setup)

(provide 'js-setup)
