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

(provide 'python-setup)
