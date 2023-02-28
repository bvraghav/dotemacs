;; Projectile Setup
(use-package projectile
  :ensure t

  :demand

  :bind (:map projectile-mode-map
	      ("C-'" . projectile-command-map)
	      ("s-p" . projectile-command-map)
	      :map projectile-command-map
	      ("SPC" . helm-projectile))

  :init
  (setq projectile-enable-caching t
	projectile-completion-system 'default
	projectile-project-search-path '("~/code" "~/webr")
	projectile-switch-project-action 'projectile-dired
	projectile-project-root-files
	'("rebar.config" "project.clj" "build.boot"
	  "SConstruct" "pom.xml" "build.sbt" "gradlew"
	  "build.gradle" ".ensime" "Gemfile" "requirements.txt"
	  "setup.py" "tox.ini" "gulpfile.js" "Gruntfile.js"
	  "bower.json" "composer.json" "Cargo.toml" "mix.exs"
	  "stack.yaml" "info.rkt" "DESCRIPTION" "TAGS" "GTAGS")
	projectile-project-root-files-bottom-up
	'(".projectile" ".git" ".hg" ".fslckout" "_FOSSIL_"
	  ".bzr" "_darcs" ".svn"))
  (projectile-mode t))

(use-package helm-projectile
  :ensure t)

;; Required for projectile-search-ripgrep
(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings))

;; (require 'projectile)

;; (custom-set-variables
;;  '(projectile-enable-caching t)
;;  '(projectile-completion-system 'default)
;;  '(projectile-project-search-path '("~/code"))
;;  '(projectile-switch-project-action 'projectile-dired)
;;  )

;; (define-key projectile-mode-map (kbd "C-'") 'projectile-command-map)
;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)

;; (define-key projectile-command-map (kbd "SPC") #'helm-projectile)

;; (projectile-mode t)

(provide 'projectile-setup)
