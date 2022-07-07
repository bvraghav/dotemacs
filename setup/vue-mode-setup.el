(use-package edit-indirect :ensure t)

(require 'vue-mode)

(add-hook 'vue-mode-hook #'yas-minor-mode)

(provide 'vue-mode-setup)
