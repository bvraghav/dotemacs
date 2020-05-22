(defun bvr/scheme-mode-hook ()
  (paredit-mode t))

(add-hook 'scheme-mode-hook #'bvr/scheme-mode-hook)

(provide 'scheme-setup)
