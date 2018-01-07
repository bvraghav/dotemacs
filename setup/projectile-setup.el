;; Projectile Setup
(require 'projectile)
;; (setq
;;  projectile-enable-caching t
;;  projectile-completion-system 'default
;;  )
;;(projectile-global-mode t)
(define-key projectile-mode-map (kbd "C-'") 'projectile-command-map)

(provide 'projectile-setup)
