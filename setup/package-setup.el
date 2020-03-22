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

(setq bvr-packages-in-archive '(project-explorer
      js2-mode org-sync forge biblio biblio-core ghub
      helm helm-bibtex helm-core htmlize hydra
      key-chord org-plus-contrib org-ref parsebib
      pdf-tools request-deferred tablist treepy w3m
      wavefront-obj-mode exec-path-from-shell json-mode
      flycheck stylus-mode npm-mode erlang
      gitattributes-mode gitignore-mode gitconfig-mode
      openwith mark-multiple slime ox-mediawiki magit
      php-mode nginx-mode edit-indirect xah-css-mode
      pinentry org org-wc org-web-tools ob-http
      yatemplate yasnippet-snippets yasnippet
      ob-ipython elpy ess-smart-equals ess
      zenburn-theme wc-mode smtpmail-multi python-info
      pydoc-info projectile-speedbar projectile-sift
      pcomplete-extension multi-term mc-extras lua-mode
      highlight graphviz-dot-mode gnuplot-mode
      glsl-mode expand-region emmet-mode ein dot-mode
      dired-narrow dired-filter cmake-mode cmake-ide
      bbdb-csv-import auctex-latexmk apache-mode
      skewer-mode scad-mode ggtags auctex graphql
      markdown-mode+ aurel))

(defun install-selected-packages ()
  (interactive)
  (get-package-archive-contents)
  (dolist (pkg bvr-packages-in-archive)
    (unless (package-installed-p pkg)
      (package-install pkg))))

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

(package-initialize)
(install-selected-packages)

(provide 'package-setup)
