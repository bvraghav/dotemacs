;; Magit and Forge
;; -----------------------------------
(use-package magit
  :ensure t
  
  :bind ("C-x g" . magit-status)

  :init
  (setq smerge-command-prefix ""))

(use-package gitignore-mode
  :ensure t)

(use-package forge
  :ensure t
  :after magit
  :config
  (setq forge-alist
	(quote
	 (("github.com" "api.github.com" "github.com" forge-github-repository)
	  ("gitlab" "gitlab.com/api/v4" "ssh:gitlab" forge-gitlab-repository)
	  ("gitlab.com" "gitlab.com/api/v4" "gitlab.com" forge-gitlab-repository)
	  ("salsa.debian.org" "salsa.debian.org/api/v4" "salsa.debian.org" forge-gitlab-repository)
	  ("codeberg.org" "codeberg.org/api/v1" "codeberg.org" forge-gitea-repository)
	  ("code.orgmode.org" "code.orgmode.org/api/v1" "code.orgmode.org" forge-gogs-repository)
	  ("bitbucket.org" "api.bitbucket.org/2.0" "bitbucket.org" forge-bitbucket-repository)
	  ("git.savannah.gnu.org" nil "git.savannah.gnu.org" forge-cgit*-repository)
	  ("git.kernel.org" nil "git.kernel.org" forge-cgit-repository)
	  ("repo.or.cz" nil "repo.or.cz" forge-repoorcz-repository)
	  ("git.suckless.org" nil "git.suckless.org" forge-stagit-repository)
	  ("git.sr.ht" nil "git.sr.ht" forge-srht-repository)))))

(provide 'magit-setup)
