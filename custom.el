
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list (quote ("/usr/share/info" "/usr/local/share/info")))
 '(Info-default-directory-list (quote ("/usr/share/info/" "~/.local/share/info/")))
 '(Man-width 65)
 '(auth-sources (quote ("~/.authinfo.gpg" "~/.authinfo" "~/.netrc")))
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(bookmark-save-flag 1)
 '(bookmark-version-control t)
 '(css-indent-offset 2)
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" "67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" default)))
 '(desktop-path (quote ("~/.emacs.d/" "~" "~/.emacs.d/dtp")))
 '(desktop-save-mode t)
 '(dired-isearch-filenames t)
 '(dired-listing-switches "-alh")
 '(echo-bell-mode t)
 '(gnus-summary-line-format "%U%R%z%I%(%[%d: %-23,23f%]%) %s
")
 '(gnus-thread-indent-level 2)
 '(icicle-Completions-text-scale-decrease 0.0)
 '(icicle-mode t)
 '(icicle-top-level-key-bindings
   (quote
    (([pause]
      icicle-switch-to/from-minibuffer t)
     ("" icicle-search-generic t)
     ("$" icicle-search-word t)
     ("^" icicle-search-keywords t)
     ("o" icicle-occur t)
     ("=" icicle-imenu t)
     ("\"" icicle-search-text-property t)
     ("/" icicle-complete-thesaurus-entry
      (fboundp
       (quote icicle-complete-thesaurus-entry)))
     ([24 134217829]
      icicle-execute-named-keyboard-macro t)
     ([27 134217752]
      icicle-command-abbrev t)
     ("5o" icicle-select-frame t)
     ("" icicle-describe-option-of-type t)
     ([S-f4]
      icicle-kmacro t)
     (abort-recursive-edit icicle-abort-recursive-edit t)
     (apropos icicle-apropos t)
     (apropos-command icicle-apropos-command t)
     (apropos-value icicle-apropos-value t)
     (apropos-user-option icicle-apropos-option
			  (and
			   (fboundp
			    (quote icicle-apropos-option))
			   (fboundp
			    (quote apropos-user-option))))
     (apropos-variable icicle-apropos-option
		       (and
			(fboundp
			 (quote icicle-apropos-option))
			(not
			 (fboundp
			  (quote apropos-user-option)))))
     (apropos-variable icicle-apropos-variable
		       (not
			(fboundp
			 (quote icicle-apropos-option))))
     (apropos-zippy icicle-apropos-zippy t)
     (bookmark-jump icicle-bookmark t)
     (bookmark-jump-other-window icicle-bookmark-other-window t)
     (bmkp-bookmark-set-confirm-overwrite icicle-bookmark-cmd
					  (fboundp
					   (quote bmkp-bookmark-set-confirm-overwrite)))
     (bookmark-set icicle-bookmark-cmd t)
     (customize-apropos icicle-customize-apropos t)
     (customize-apropos-faces icicle-customize-apropos-faces t)
     (customize-apropos-groups icicle-customize-apropos-groups t)
     (customize-apropos-options icicle-customize-apropos-options t)
     (customize-face icicle-customize-face t)
     (customize-face-other-window icicle-customize-face-other-window t)
     (dabbrev-completion icicle-dabbrev-completion t)
     ([201326639]
      icicle-dispatch-C-M-/ t)
     (delete-window icicle-delete-window t)
     (delete-windows-for icicle-delete-window t)
     (describe-package icicle-describe-package
		       (fboundp
			(quote describe-package)))
     (dired icicle-dired
	    (not
	     (featurep
	      (quote dired+))))
     (dired-other-window icicle-dired-other-window
			 (not
			  (featurep
			   (quote dired+))))
     (exchange-point-and-mark icicle-exchange-point-and-mark t)
     (execute-extended-command icicle-execute-extended-command t)
     (find-file icicle-file t)
     (find-file-other-window icicle-file-other-window t)
     (find-file-read-only icicle-find-file-read-only t)
     (find-file-read-only-other-window icicle-find-file-read-only-other-window t)
     (insert-buffer icicle-insert-buffer t)
     (kill-buffer icicle-kill-buffer t)
     (kill-buffer-and-its-windows icicle-kill-buffer t)
     (load-library icicle-load-library
		   (> emacs-major-version 20))
     (minibuffer-keyboard-quit icicle-abort-recursive-edit
			       (fboundp
				(quote minibuffer-keyboard-quit)))
     (other-window icicle-other-window-or-frame t)
     (other-window-or-frame icicle-other-window-or-frame t)
     (pop-global-mark icicle-goto-global-marker-or-pop-global-mark t)
     (repeat-complex-command icicle-repeat-complex-command t)
     (set-mark-command icicle-goto-marker-or-set-mark-command t)
     (switch-to-buffer icicle-buffer t)
     (switch-to-buffer-other-window icicle-buffer-other-window t)
     (where-is icicle-where-is t)
     (yank icicle-yank-maybe-completing t)
     (yank-pop icicle-yank-pop-commands
	       (featurep
		(quote second-sel)))
     (yank-pop-commands icicle-yank-pop-commands
			(featurep
			 (quote second-sel)))
     (zap-to-char icicle-zap-to-char
		  (fboundp
		   (quote read-char-by-name)))
     (bmkp-autofile-set icicle-bookmark-a-file
			(fboundp
			 (quote bmkp-bookmark-a-file)))
     (bmkp-tag-a-file icicle-tag-a-file
		      (fboundp
		       (quote bmkp-tag-a-file)))
     (bmkp-untag-a-file icicle-untag-a-file
			(fboundp
			 (quote bmkp-untag-a-file)))
     (bmkp-find-file icicle-find-file-handle-bookmark
		     (fboundp
		      (quote bmkp-find-file)))
     (bmkp-find-file-other-window icicle-find-file-handle-bookmark-other-window
				  (fboundp
				   (quote bmkp-find-file-other-window)))
     (bmkp-autofile-jump icicle-bookmark-autofile
			 (fboundp
			  (quote bmkp-autofile-jump)))
     (bmkp-autofile-jump-other-window icicle-bookmark-autofile-other-window
				      (fboundp
				       (quote bmkp-autofile-jump)))
     (bmkp-autonamed-jump icicle-bookmark-autonamed
			  (fboundp
			   (quote bmkp-autonamed-jump)))
     (bmkp-autonamed-jump-other-window icicle-bookmark-autonamed-other-window
				       (fboundp
					(quote bmkp-autonamed-jump)))
     (bmkp-autonamed-this-buffer-jump icicle-bookmark-autonamed-this-buffer
				      (fboundp
				       (quote bmkp-autonamed-this-buffer-jump)))
     (bmkp-bookmark-file-jump icicle-bookmark-bookmark-file
			      (fboundp
			       (quote bmkp-bookmark-file-jump)))
     (bmkp-bookmark-list-jump icicle-bookmark-bookmark-list
			      (fboundp
			       (quote bmkp-bookmark-list-jump)))
     (bmkp-desktop-jump icicle-bookmark-desktop
			(fboundp
			 (quote bmkp-desktop-jump)))
     (bmkp-dired-jump icicle-bookmark-dired
		      (fboundp
		       (quote bmkp-dired-jump)))
     (bmkp-dired-jump-other-window icicle-bookmark-dired-other-window
				   (fboundp
				    (quote bmkp-dired-jump)))
     (bmkp-file-jump icicle-bookmark-file
		     (fboundp
		      (quote bmkp-file-jump)))
     (bmkp-file-jump-other-window icicle-bookmark-file-other-window
				  (fboundp
				   (quote bmkp-file-jump)))
     (bmkp-file-this-dir-jump icicle-bookmark-file-this-dir
			      (fboundp
			       (quote bmkp-file-this-dir-jump)))
     (bmkp-file-this-dir-jump-other-window icicle-bookmark-file-this-dir-other-window
					   (fboundp
					    (quote bmkp-file-this-dir-jump)))
     (bmkp-gnus-jump icicle-bookmark-gnus
		     (fboundp
		      (quote bmkp-gnus-jump)))
     (bmkp-gnus-jump-other-window icicle-bookmark-gnus-other-window
				  (fboundp
				   (quote bmkp-gnus-jump)))
     (bmkp-image-jump icicle-bookmark-image
		      (fboundp
		       (quote bmkp-image-jump)))
     (bmkp-image-jump-other-window icicle-bookmark-image-other-window
				   (fboundp
				    (quote bmkp-image-jump)))
     (bmkp-info-jump icicle-bookmark-info
		     (fboundp
		      (quote bmkp-info-jump)))
     (bmkp-info-jump-other-window icicle-bookmark-info-other-window
				  (fboundp
				   (quote bmkp-info-jump)))
     (bmkp-local-file-jump icicle-bookmark-local-file
			   (fboundp
			    (quote bmkp-local-file-jump)))
     (bmkp-local-file-jump-other-window icicle-bookmark-local-file-other-window
					(fboundp
					 (quote bmkp-local-file-jump)))
     (bmkp-man-jump icicle-bookmark-man
		    (fboundp
		     (quote bmkp-man-jump)))
     (bmkp-man-jump-other-window icicle-bookmark-man-other-window
				 (fboundp
				  (quote bmkp-man-jump)))
     (bmkp-non-file-jump icicle-bookmark-non-file
			 (fboundp
			  (quote bmkp-non-file-jump)))
     (bmkp-non-file-jump-other-window icicle-bookmark-non-file-other-window
				      (fboundp
				       (quote bmkp-non-file-jump)))
     (bmkp-region-jump icicle-bookmark-region
		       (fboundp
			(quote bmkp-region-jump)))
     (bmkp-region-jump-other-window icicle-bookmark-region-other-window
				    (fboundp
				     (quote bmkp-region-jump)))
     (bmkp-remote-file-jump icicle-bookmark-remote-file
			    (fboundp
			     (quote bmkp-remote-file-jump)))
     (bmkp-remote-file-jump-other-window icicle-bookmark-remote-file-other-window
					 (fboundp
					  (quote bmkp-remote-file-jump)))
     (bmkp-specific-buffers-jump icicle-bookmark-specific-buffers
				 (fboundp
				  (quote bmkp-specific-buffers-jump)))
     (bmkp-specific-buffers-jump-other-window icicle-bookmark-specific-buffers-other-window
					      (fboundp
					       (quote bmkp-specific-buffers-jump)))
     (bmkp-specific-files-jump icicle-bookmark-specific-files
			       (fboundp
				(quote bmkp-specific-files-jump)))
     (bmkp-specific-files-jump-other-window icicle-bookmark-specific-files-other-window
					    (fboundp
					     (quote bmkp-specific-files-jump)))
     (bmkp-temporary-jump icicle-bookmark-temporary
			  (fboundp
			   (quote bmkp-temporary-jump)))
     (bmkp-temporary-jump-other-window icicle-bookmark-temporary-other-window
				       (fboundp
					(quote bmkp-temporary-jump)))
     (bmkp-this-buffer-jump icicle-bookmark-this-buffer
			    (fboundp
			     (quote bmkp-this-buffer-jump)))
     (bmkp-this-buffer-jump-other-window icicle-bookmark-this-buffer-other-window
					 (fboundp
					  (quote bmkp-this-buffer-jump)))
     (bmkp-url-jump icicle-bookmark-url
		    (fboundp
		     (quote bmkp-url-jump)))
     (bmkp-url-jump-other-window icicle-bookmark-url-other-window
				 (fboundp
				  (quote bmkp-url-jump)))
     (bmkp-w3m-jump icicle-bookmark-w3m
		    (fboundp
		     (quote bmkp-w3m-jump)))
     (bmkp-w3m-jump-other-window icicle-bookmark-w3m-other-window
				 (fboundp
				  (quote bmkp-w3m-jump)))
     ("jtj" icicle-bookmark-tagged
      (featurep
       (quote bookmark+)))
     ("4jtj" icicle-bookmark-tagged-other-window
      (featurep
       (quote bookmark+)))
     ("jt" icicle-find-file-tagged
      (featurep
       (quote bookmark+)))
     ("4jt" icicle-find-file-tagged-other-window
      (featurep
       (quote bookmark+)))
     (bmkp-find-file-all-tags icicle-find-file-all-tags
			      (fboundp
			       (quote bmkp-find-file-all-tags)))
     (bmkp-find-file-all-tags-other-window icicle-find-file-all-tags-other-window
					   (fboundp
					    (quote bmkp-find-file-all-tags)))
     (bmkp-find-file-all-tags-regexp icicle-find-file-all-tags-regexp
				     (fboundp
				      (quote bmkp-find-file-all-tags-regexp)))
     (bmkp-find-file-all-tags-regexp-other-window icicle-find-file-all-tags-regexp-other-window
						  (fboundp
						   (quote bmkp-find-file-all-tags-regexp-other-window)))
     (bmkp-find-file-some-tags icicle-find-file-some-tags
			       (fboundp
				(quote bmkp-find-file-some-tags)))
     (bmkp-find-file-some-tags-other-window icicle-find-file-some-tags-other-window
					    (fboundp
					     (quote bmkp-find-file-some-tags-other-window)))
     (bmkp-find-file-some-tags-regexp icicle-find-file-some-tags-regexp
				      (fboundp
				       (quote bmkp-find-file-some-tags-regexp)))
     (bmkp-find-file-some-tags-regexp-other-window icicle-find-file-some-tags-regexp-other-window
						   (fboundp
						    (quote bmkp-find-file-some-tags-regexp-other-window)))
     (bmkp-autofile-all-tags-jump icicle-bookmark-autofile-all-tags
				  (fboundp
				   (quote bmkp-autofile-all-tags-jump)))
     (bmkp-autofile-all-tags-jump-other-window icicle-bookmark-autofile-all-tags-other-window
					       (fboundp
						(quote bmkp-autofile-all-tags-jump)))
     (bmkp-autofile-all-tags-regexp-jump icicle-bookmark-autofile-all-tags-regexp
					 (fboundp
					  (quote bmkp-autofile-all-tags-regexp-jump)))
     (bmkp-autofile-all-tags-regexp-jump-other-window icicle-bookmark-autofile-all-tags-regexp-other-window
						      (fboundp
						       (quote bmkp-autofile-all-tags-regexp-jump)))
     (bmkp-autofile-some-tags-jump icicle-bookmark-autofile-some-tags
				   (fboundp
				    (quote bmkp-autofile-some-tags-jump)))
     (bmkp-autofile-some-tags-jump-other-window icicle-bookmark-autofile-some-tags-other-window
						(fboundp
						 (quote bmkp-autofile-some-tags-jump)))
     (bmkp-autofile-some-tags-regexp-jump icicle-bookmark-autofile-some-tags-regexp
					  (fboundp
					   (quote bmkp-autofile-some-tags-regexp-jump)))
     (bmkp-autofile-some-tags-regexp-jump-other-window icicle-bookmark-autofile-some-tags-regexp-other-window
						       (fboundp
							(quote bmkp-autofile-some-tags-regexp-jump)))
     (bmkp-all-tags-jump icicle-bookmark-all-tags
			 (fboundp
			  (quote bmkp-all-tags-jump)))
     (bmkp-all-tags-jump-other-window icicle-bookmark-all-tags-other-window
				      (fboundp
				       (quote bmkp-all-tags-jump)))
     (bmkp-all-tags-regexp-jump icicle-bookmark-all-tags-regexp
				(fboundp
				 (quote bmkp-all-tags-regexp-jump)))
     (bmkp-all-tags-regexp-jump-other-window icicle-bookmark-all-tags-regexp-other-window
					     (fboundp
					      (quote bmkp-all-tags-regexp-jump)))
     (bmkp-some-tags-jump icicle-bookmark-some-tags
			  (fboundp
			   (quote bmkp-some-tags-jump)))
     (bmkp-some-tags-jump-other-window icicle-bookmark-some-tags-other-window
				       (fboundp
					(quote bmkp-some-tags-jump)))
     (bmkp-some-tags-regexp-jump icicle-bookmark-some-tags-regexp
				 (fboundp
				  (quote bmkp-some-tags-regexp-jump)))
     (bmkp-some-tags-regexp-jump-other-window icicle-bookmark-some-tags-regexp-other-window
					      (fboundp
					       (quote bmkp-some-tags-regexp-jump)))
     (bmkp-file-all-tags-jump icicle-bookmark-file-all-tags
			      (fboundp
			       (quote bmkp-file-all-tags-jump)))
     (bmkp-file-all-tags-jump-other-window icicle-bookmark-file-all-tags-other-window
					   (fboundp
					    (quote bmkp-file-all-tags-jump)))
     (bmkp-file-all-tags-regexp-jump icicle-bookmark-file-all-tags-regexp
				     (fboundp
				      (quote bmkp-file-all-tags-regexp-jump)))
     (bmkp-file-all-tags-regexp-jump-other-window icicle-bookmark-file-all-tags-regexp-other-window
						  (fboundp
						   (quote bmkp-file-all-tags-regexp-jump)))
     (bmkp-file-some-tags-jump icicle-bookmark-file-some-tags
			       (fboundp
				(quote bmkp-file-some-tags-jump)))
     (bmkp-file-some-tags-jump-other-window icicle-bookmark-file-some-tags-other-window
					    (fboundp
					     (quote bmkp-file-some-tags-jump)))
     (bmkp-file-some-tags-regexp-jump icicle-bookmark-file-some-tags-regexp
				      (fboundp
				       (quote bmkp-file-some-tags-regexp-jump)))
     (bmkp-file-some-tags-regexp-jump-other-window icicle-bookmark-file-some-tags-regexp-other-window
						   (fboundp
						    (quote bmkp-file-some-tags-regexp-jump)))
     (bmkp-file-this-dir-all-tags-jump icicle-bookmark-file-this-dir-all-tags
				       (fboundp
					(quote bmkp-file-this-dir-all-tags-jump)))
     (bmkp-file-this-dir-all-tags-jump-other-window icicle-bookmark-file-this-dir-all-tags-other-window
						    (fboundp
						     (quote bmkp-file-this-dir-all-tags-jump)))
     (bmkp-file-this-dir-all-tags-regexp-jump icicle-bookmark-file-this-dir-all-tags-regexp
					      (fboundp
					       (quote bmkp-file-this-dir-all-tags-regexp-jump)))
     (bmkp-file-this-dir-all-tags-regexp-jump-other-window icicle-bookmark-file-this-dir-all-tags-regexp-other-window
							   (fboundp
							    (quote bmkp-file-this-dir-all-tags-regexp-jump)))
     (bmkp-file-this-dir-some-tags-jump icicle-bookmark-file-this-dir-some-tags
					(fboundp
					 (quote bmkp-file-this-dir-some-tags-jump)))
     (bmkp-file-this-dir-some-tags-jump-other-window icicle-bookmark-file-this-dir-some-tags-other-window
						     (fboundp
						      (quote bmkp-file-this-dir-some-tags-jump)))
     (bmkp-file-this-dir-some-tags-regexp-jump icicle-bookmark-file-this-dir-some-tags-regexp
					       (fboundp
						(quote bmkp-file-this-dir-some-tags-regexp-jump)))
     (bmkp-file-this-dir-some-tags-regexp-jump-other-window icicle-bookmark-file-this-dir-some-tags-regexp-other-window
							    (fboundp
							     (quote bmkp-file-this-dir-some-tags-regexp-jump)))
     (find-tag icicle-find-tag
	       (fboundp
		(quote command-remapping)))
     (find-tag-other-window icicle-find-first-tag-other-window t)
     (pop-tag-mark icicle-pop-tag-mark
		   (fboundp
		    (quote command-remapping)))
     (eval-expression icicle-pp-eval-expression
		      (fboundp
		       (quote command-remapping)))
     (pp-eval-expression icicle-pp-eval-expression
			 (fboundp
			  (quote command-remapping)))
     ([S-f10]
      icicle-complete-menu-bar
      (fboundp
       (quote icicle-complete-menu-bar)))
     ([27 134217848]
      lacarte-execute-command
      (fboundp
       (quote lacarte-execute-command)))
     ([134217824]
      lacarte-execute-menu-command
      (fboundp
       (quote lacarte-execute-menu-command)))
     ([f10]
      lacarte-execute-menu-command
      (fboundp
       (quote lacarte-execute-menu-command))))))
 '(image-dired-external-viewer "/usr/bin/feh")
 '(js-indent-level 2)
 '(lua-indent-level 2)
 '(mm-discouraged-alternatives (quote ("text/html" "text/richtext")))
 '(nginx-indent-level 2)
 '(org-agenda-files (quote ("~/org")))
 '(org-babel-load-languages
   (quote
    ((js . t)
     (http . t)
     (emacs-lisp . t)
     (R . t)
     (shell . t)
     (python . t)
     (perl . t)
     (dot . t)
     (gnuplot . t)
     (sql . t)
     (lisp . t)
     (ipython . t))))
 '(org-default-notes-file "~/org/notes.org")
 '(org-export-backends
   (quote
    (ascii beamer html icalendar latex md odt koma-letter)))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "evince %s")
     ("\\.png\\'" . "feh %s")
     ("\\.jpg\\'" . "feh %s")
     ("\\.gif\\'" . "feh %s"))))
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("marmalade" . "https://marmalade-repo.org/packages/")
     ("org" . "http://orgmode.org/elpa/"))))
 '(package-selected-packages
   (quote
    (slime ox-mediawiki magit php-mode nginx-mode edit-indirect xah-css-mode pinentry org org-wc org-web-tools ob-http yatemplate yasnippet-snippets yasnippet ob-ipython elpy ess-smart-equals ess echo-bell zenburn-theme wc-mode thingatpt+ synonyms smtpmail-multi python-info pydoc-info projectile-speedbar projectile-sift pp+ php+-mode pcomplete-extension multi-term mc-extras mb-depth+ markdown-mode+ lua-mode lacarte jam-mode info+ icomplete+ icicles highlight graphviz-dot-mode gnuplot-mode glsl-mode fuzzy-match frame-cmds expand-region emmet-mode ein dot-mode doremi-mac doremi-frm doremi-cmd dired-narrow dired-filter dired+ crosshairs cmake-mode cmake-ide bookmark+ bbdb-csv-import aurel auctex-latexmk apropos-fn+var apache-mode)))
 '(projectile-project-root-files
   (quote
    ("rebar.config" "project.clj" "build.boot" "SConstruct" "pom.xml" "build.sbt" "gradlew" "build.gradle" ".ensime" "Gemfile" "requirements.txt" "setup.py" "tox.ini" "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml" "mix.exs" "stack.yaml" "info.rkt" "DESCRIPTION" "TAGS" "GTAGS")))
 '(projectile-project-root-files-bottom-up
   (quote
    (".projectile" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".svn")))
 '(python-indent-offset 2)
 '(sh-basic-offset 2)
 '(sh-indentation 2)
 '(slime-auto-start (quote ask))
 '(slime-fuzzy-completion-in-place nil)
 '(tramp-backup-directory-alist (quote ((".*" . "~/.emacs.d/backups"))) nil (tramp))
 '(visible-bell t)
 '(w3m-cookie-accept-bad-cookies (quote ask))
 '(w3m-enable-google-feeling-lucky t)
 '(w3m-fill-column 55)
 '(w3m-search-default-engine "glucky")
 '(w3m-search-engine-alist
   (quote
    (("numpy" "https://www.google.com/search?btnI=1&q=site:docs.scipy.org+%s" nil)
     ("python2-api" "https://www.google.com/search?btnI=1&q=site:docs.python.org%%2f2+%s" nil)
     ("tensorflow-api" "https://www.google.com/search?btnI=1&q=site:tensorflow.org+%s" nil)
     ("duckduckgo" "https://duckduckgo.com/lite/?q=%s" nil)
     ("worldclock" "http://www.timeanddate.com/worldclock/results.html?query=%s" nil)
     ("wikipedia-en" "http://en.wikipedia.org/wiki/Special:Search?search=%s" nil)
     ("weather" "http://www.weather.com/search/search?where=%s&what=WeatherLocalUndeclared" nil)
     ("syndic8" "http://www.syndic8.com/feedlist.php?ShowStatus=all&ShowMatch=%s" nil)
     ("google-groups" "http://www.google.com/groups?q=%s" nil)
     ("ebay" "http://search.ebay.com/search/search.dll?query=%s" nil)
     ("acronym" "http://www.acronymfinder.com/af-query.asp?acronym=%s&string=exact" nil)
     ("yahoo" "http://search.yahoo.com/bin/search?p=%s" nil)
     ("yahoo-ja" "http://search.yahoo.co.jp/bin/search?p=%s" euc-japan)
     ("alc" "http://eow.alc.co.jp/%s/UTF-8/" utf-8)
     ("blog" "http://blogsearch.google.com/blogsearch?q=%s&oe=utf-8&ie=utf-8" utf-8)
     ("blog-en" "http://blogsearch.google.com/blogsearch?q=%s&hl=en&oe=utf-8&ie=utf-8" utf-8)
     ("glucky" "http://www.google.com/search?btnI=1&q=%s" utf-8)
     ("google" "http://www.google.com/search?q=%s&ie=utf-8&oe=utf-8" utf-8)
     ("google-en" "http://www.google.com/search?q=%s&hl=en&ie=utf-8&oe=utf-8" utf-8)
     ("google news" "http://news.google.com/news?q=%s&ie=utf-8&oe=utf-8" utf-8)
     ("google news-en" "http://news.google.com/news?q=%s&hl=en&ie=utf-8&oe=utf-8" nil)
     ("google groups" "http://groups.google.com/groups?q=%s" nil)
     ("All the Web" "http://www.alltheweb.com/search?q=%s&web&_sb_lang=en" nil)
     ("All the Web-ja" "http://www.alltheweb.com/search?q=%s&web&_sb_lang=ja&cs=euc-jp" euc-japan)
     ("technorati" "http://www.technorati.com/search/%s" utf-8)
     ("technorati-ja" "http://www.technorati.jp/search/search.html?query=%s&language=ja" utf-8)
     ("technorati-tag" "http://www.technorati.com/tag/%s" utf-8)
     ("goo-ja" "http://search.goo.ne.jp/web.jsp?MT=%s" euc-japan)
     ("excite-ja" "http://www.excite.co.jp/search.gw?target=combined&look=excite_jp&lang=jp&tsug=-1&csug=-1&search=%s" shift_jis)
     ("altavista" "http://altavista.com/sites/search/web?q=\"%s\"&kl=ja&search=Search" nil)
     ("rpmfind" "http://rpmfind.net/linux/rpm2html/search.php?query=%s" nil)
     ("debian-pkg" "http://packages.debian.org/cgi-bin/search_contents.pl?directories=yes&arch=i386&version=unstable&case=insensitive&word=%s" nil)
     ("debian-bts" "http://bugs.debian.org/cgi-bin/pkgreport.cgi?archive=yes&pkg=%s" nil)
     ("freebsd-users-jp" "http://home.jp.FreeBSD.org/cgi-bin/namazu.cgi?key=\"%s\"&whence=0&max=50&format=long&sort=score&dbname=FreeBSD-users-jp" euc-japan)
     ("iij-archie" "http://www.iij.ad.jp/cgi-bin/archieplexform?query=%s&type=Case+Insensitive+Substring+Match&order=host&server=archie1.iij.ad.jp&hits=95&nice=Nice" nil)
     ("waei" "http://dictionary.goo.ne.jp/search.php?MT=%s&kind=je" euc-japan)
     ("eiwa" "http://dictionary.goo.ne.jp/search.php?MT=%s&kind=ej" nil)
     ("kokugo" "http://dictionary.goo.ne.jp/search.php?MT=%s&kind=jn" euc-japan)
     ("eiei" "http://www.dictionary.com/cgi-bin/dict.pl?term=%s&r=67" nil)
     ("amazon" "http://www.amazon.com/exec/obidos/search-handle-form/250-7496892-7797857" iso-8859-1 "url=index=blended&field-keywords=%s")
     ("amazon-ja" "http://www.amazon.co.jp/gp/search?__mk_ja_JP=%%83J%%83%%5E%%83J%%83i&url=search-alias%%3Daps&field-keywords=%s" shift_jis)
     ("emacswiki" "http://www.emacswiki.org/cgi-bin/wiki?search=%s" nil)
     ("en.wikipedia" "http://en.wikipedia.org/wiki/Special:Search?search=%s" nil)
     ("de.wikipedia" "http://de.wikipedia.org/wiki/Spezial:Search?search=%s" utf-8)
     ("ja.wikipedia" "http://ja.wikipedia.org/wiki/Special:Search?search=%s" utf-8)
     ("msdn" "http://search.msdn.microsoft.com/search/default.aspx?query=%s" nil)
     ("teld" "https://oag.iitk.ac.in/Tel/TelephoneDirectory1.php?Select0=S&Select=dept&FDept=%s" nil)
     ("freshmeat" "http://freshmeat.net/search/?q=%s&section=projects" nil))))
 '(w3m-uri-replace-alist
   (quote
    (("\\`np:" w3m-search-uri-replace "numpy")
     ("\\`py:" w3m-search-uri-replace "python2-api")
     ("\\`tf:" w3m-search-uri-replace "tensorflow-api")
     ("\\`du:" w3m-search-uri-replace "duckduckgo")
     ("\\`g:" w3m-search-uri-replace "glucky")
     ("\\`gg:" w3m-search-uri-replace "google")
     ("\\`ggg:" w3m-search-uri-replace "google groups")
     ("\\`ya:" w3m-search-uri-replace "yahoo")
     ("\\`al:" w3m-search-uri-replace "altavista")
     ("\\`bts:" w3m-search-uri-replace "debian-bts")
     ("\\`dpkg:" w3m-search-uri-replace "debian-pkg")
     ("\\`archie:" w3m-search-uri-replace "iij-archie")
     ("\\`alc:" w3m-search-uri-replace "alc")
     ("\\`teld:" w3m-search-uri-replace "teld")
     ("\\`urn:ietf:rfc:\\([0-9]+\\)" w3m-pattern-uri-replace "http://www.ietf.org/rfc/rfc\\1.txt")))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "xos4 Terminus" :foundry "xos4" :slant normal :weight normal :height 105 :width normal))))
 '(col-highlight ((t (:background "#383838"))))
 '(highlight ((t (:background "dim gray")))))
