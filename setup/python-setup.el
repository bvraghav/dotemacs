(defun bvr-python-mode-hook ()
  (customize-set-variable 'indent-tabs-mode nil))

(add-hook 'python-mode-hook #'bvr-python-mode-hook)

(provide 'python-setup)
