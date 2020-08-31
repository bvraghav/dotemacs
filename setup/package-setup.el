;; ----------------------------------------------------

;; To refresh package archive once
;; ----------------------------------------------------

(setq refreshed nil)

(defun get-package-archive-contents ()
  (unless refreshed
    (package-refresh-contents)
    (setq refreshed t))
  package-archive-contents)

;; ----------------------------------------------------

;; To install the packages
;; ----------------------------------------------------

(setq
 bvr-packages-in-archive

 '(anaphora apache-mode async auctex auctex-latexmk aurel bbdb
            bbdb-csv-import biblio biblio-core bibtex-completion
            bui closql cmake-ide cmake-mode company dash
            dash-functional deferred dired-filter
            dired-hacks-utils dired-narrow dot-mode edit-indirect
            ein elpy emacsql emacsql-sqlite emmet-mode epl erlang
            es-lib es-windows ess ess-smart-equals esxml
            exec-path-from-shell expand-region f flycheck forge
            fuzzy-match geiser ggtags ghub git-commit
            gitattributes-mode gitconfig-mode gitignore-mode
            glsl-mode gnuplot-mode graphql graphviz-dot-mode helm
            helm-bibtex helm-core highlight highlight-indentation
            htmlize hydra ivy jam-mode jq-mode js-comint js2-mode
            json-mode json-reformat json-snatcher julia-mode
            key-chord kubernetes lacarte levenshtein lua-mode lv
            macrostep magit magit-popup mark-multiple
            markdown-mode markdown-mode+ mc-extras multi-term
            multiple-cursors nginx-mode npm-mode ob-http
            ob-ipython ob-restclient openwith org
            org-plus-contrib org-ref org-sync org-wc
            org-web-tools ox-mediawiki paredit parsebib
            pcomplete-extension pcsv pdf-tools php+-mode php-mode
            pinentry pkg-info polymode popup project-explorer
            projectile projectile-sift projectile-speedbar
            python-info pyvenv request request-deferred
            restclient restclient-test s scad-mode sift
            simple-httpd skewer-mode slime smtpmail-multi
            sr-speedbar stylus-mode sws-mode synonyms tablist
            transient treepy w3m wavefront-obj-mode wc-mode
            websocket with-editor xah-css-mode yaml-mode
            yasnippet yasnippet-snippets yatemplate
            zenburn-theme))

(defun install-selected-packages ()
  (interactive)
  (get-package-archive-contents)
  (dolist (pkg bvr-packages-in-archive)
    (unless (package-installed-p pkg)
      (package-install pkg))))

(package-initialize)
(install-selected-packages)

(provide 'package-setup)

;; ----------------------------------------------------

;; ;; How was the packages list computed
;; ;; ----------------------------------------------------
;; (setq bvr-package-list '(project-explorer js2-mode
;;       org-sync forge biblio biblio-core ghub helm
;;       helm-bibtex helm-core htmlize hydra key-chord
;;       org-plus-contrib org-ref parsebib pdf-tools
;;       request-deferred tablist treepy w3m
;;       wavefront-obj-mode exec-path-from-shell json-mode
;;       flycheck stylus-mode npm-mode erlang
;;       gitattributes-mode gitignore-mode gitconfig-mode
;;       openwith mark-multiple slime ox-mediawiki magit
;;       php-mode nginx-mode edit-indirect xah-css-mode
;;       pinentry org org-wc org-web-tools ob-http
;;       yatemplate yasnippet-snippets yasnippet
;;       ob-ipython elpy ess-smart-equals ess
;;       zenburn-theme wc-mode smtpmail-multi python-info
;;       pydoc-info projectile-speedbar projectile-sift
;;       pcomplete-extension multi-term mc-extras lua-mode
;;       highlight graphviz-dot-mode gnuplot-mode
;;       glsl-mode expand-region emmet-mode ein dot-mode
;;       dired-narrow dired-filter cmake-mode cmake-ide
;;       bbdb-csv-import auctex-latexmk apache-mode))

;; (setq bvr-package-list (append bvr-package-list
;;       '(skewer-mode js2-mode scad-mode ggtags auctex
;;       org-sync forge biblio biblio-core ghub helm
;;       helm-bibtex helm-core htmlize key-chord
;;       org-plus-contrib org-ref parsebib
;;       request-deferred tablist treepy graphql w3m
;;       wavefront-obj-mode exec-path-from-shell json-mode
;;       flycheck stylus-mode npm-mode erlang
;;       gitattributes-mode gitignore-mode gitconfig-mode
;;       openwith mark-multiple slime ox-mediawiki magit
;;       php-mode nginx-mode edit-indirect xah-css-mode
;;       pinentry org-wc org-web-tools ob-http yatemplate
;;       yasnippet-snippets yasnippet ob-ipython elpy
;;       ess-smart-equals ess echo-bell zenburn-theme
;;       wc-mode thingatpt+ synonyms smtpmail-multi
;;       python-info pydoc-info projectile-speedbar
;;       projectile-sift pp+ php+-mode pcomplete-extension
;;       multi-term mc-extras mb-depth+ markdown-mode+
;;       lua-mode lacarte jam-mode info+ icomplete+
;;       icicles highlight graphviz-dot-mode gnuplot-mode
;;       glsl-mode fuzzy-match frame-cmds expand-region
;;       emmet-mode ein dot-mode doremi-mac doremi-frm
;;       doremi-cmd dired-narrow dired-filter dired+
;;       crosshairs cmake-mode cmake-ide bookmark+
;;       bbdb-csv-import aurel auctex-latexmk
;;       apropos-fn+var apache-mode)))

;; (setq bvr-package-list (append bvr-package-list
;;       '(skewer-mode scad-mode ggtags auctex graphql
;;       echo-bell thingatpt+ synonyms pp+ php+-mode
;;       mb-depth+ markdown-mode+ lacarte jam-mode info+
;;       icomplete+ icicles fuzzy-match frame-cmds
;;       doremi-mac doremi-frm doremi-cmd dired+
;;       crosshairs bookmark+ aurel apropos-fn+var)))

;; (delete-dups bvr-package-list)

;; (setq bvr-packages-in-archive (remove-if-not (lambda
;;       (x) (assoc x (get-package-archive-contents)))
;;       bvr-package-list))
