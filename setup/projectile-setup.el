;; Projectile Setup
(require 'projectile)

(custom-set-variables
 '(projectile-enable-caching t)
 '(projectile-completion-system 'default)
 '(projectile-project-search-path '("~/code"))
 '(projectile-switch-project-action 'projectile-dired)
 )

(define-key projectile-mode-map (kbd "C-'") 'projectile-command-map)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)

(define-key projectile-command-map (kbd "SPC") #'helm-projectile)

(projectile-mode t)

(provide 'projectile-setup)
