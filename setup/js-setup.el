(require 'js2-mode)

;; ES6 based javascript mode
;; (add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.cjs\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js2-mode))

(defun bvr/js2-key-bindings ()
  (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
  (local-set-key (kbd "C-M-x")   'js-send-last-sexp-and-go)
  (local-set-key (kbd "C-c b")   'js-send-buffer)
  (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)
  (local-set-key (kbd "C-c l")   'js-load-file-and-go)
  (message "Added bvr/js2-key-bindings"))

(add-hook 'js2-mode-hook #'bvr/js2-key-bindings)

(add-hook 'js2-mode-hook #'auto-revert-mode)

(defun bvr/ts-setup ()
  (auto-revert-mode 1)
  (ggtags-mode 1))
(add-hook 'typescript-mode-hook #'bvr/ts-setup)

(provide 'js-setup)
