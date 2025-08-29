;;; projectile-setup.el --- Setup Projectile -*- lexical-binding: t -*-

;; Author: B.V. Raghav
;; Maintainer: B.V. Raghav
;; Version: 0.1
;; Package-Requires: ()
;; Homepage: https://github.com/bvraghav/dotemacs
;; Keywords: dotemacs,emacs,projectile


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Setup Projectile

;;; Code:

;; Projectile Setup
(use-package projectile
  :ensure t

  :demand

  :bind (:map projectile-mode-map
	      ("C-'" . projectile-command-map)
	      ("s-p" . projectile-command-map)
	      :map projectile-command-map
	      ("SPC" . helm-projectile)
              ("C-d" . bvr/projectile-open-tmux-in-root))

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
  (defun bvr/projectile-open-tmux-in-root ()
    (interactive)
    (let* ((dir (projectile-project-root))
           (dir (and dir (directory-file-name dir)))
           (cmd-1 (format
                   "{ tmux ls -F '#S#,#{session_path}' 2>/dev/null } | grep '%s' | grep -o '^E_.*,' | tr -d ','"
                   dir))
           (sess (string-trim (shell-command-to-string cmd-1)))
           (cmd-attach (format "which xterm; xterm -e 'tmux attach -t \"%s\"' "
                               sess))
           (cmd-new (format "xterm -e 'tmux new-session -c \"%s\"  -s \"E_`uuidgen | head -c 6`\"'"
                            dir))
           (cmd (if (string-empty-p sess) cmd-new cmd-attach)))
      (message "bvr/projectile-open-tmux-in-root Async Shell CMD:")
      (message "  {%s}" cmd)
      (async-shell-command cmd)))

  ;; (defun bvr/projectile-refresh-py-cli-doc ()
  ;;   (interactive)
  ;;   (let* ((dir (projectile-project-root))
  ;;          (dir (and dir (directory-file-name dir)))
  ;;          )
  ;;     ))

  (projectile-mode t))

(use-package helm-projectile
  :ensure t)

;; Required for projectile-search-ripgrep
(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings))


(provide 'projectile-setup)

;;; projectile-setup.el ends here
