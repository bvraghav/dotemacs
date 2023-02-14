;; Document viewers setup
;; -----------------------------------
;; PDF Tools
;; (use-package pdf-tools
;;   :ensure t
;;   :pin manual

;;   :demand

;;   :hook (pdf-view-mode . pdf-view-midnight-minor-mode)

;;   :config
;;   (require 'pdf-tools)
;;   (require 'pdf-occur)
;;   ;; (pdf-tools-install)
;;   )

;; Epub Reader
(use-package nov
  :ensure t
  :mode (("\\.epub\\'" . nov-mode))
  :hook (nov-mode . bvr/nov-mode-hook)

  :init ()

  :config
  (defun bvr/nov-mode-hook ()
    (face-rqemap-add-relative 'variable-pitch nil))
  (setq nov-text-width 55))

;; Org Noter
;; -----------------------------------
(use-package org-noter
  :ensure t)

;; Org Download
;; -----------------------------------
(use-package org-download
  :demand
  :ensure t
  :after org
  :bind (:map org-mode-map
              (("s-Y" . org-download-screenshot)
               ("s-y" . org-download-yank)))
  :hook (dired-mode . org-download-enable)

  :config

  ;; Variables
  (setq org-download-screenshot-method "gm import %s")
  (setq-default org-download-image-dir "org-download-images"))

;; Org Roam
;; -----------------------------------
(use-package org-roam
  :ensure t

  :init (setq org-roam-v2-ack t)

  :hook (after-init . org-roam-db-autosync-mode)

  :bind (("C-z C-d C-d" . org-roam-dailies-find-date)
         ("C-z C-d d"   . org-roam-dailies-find-date)
         ("C-z C-d C-z" . org-roam-dailies-find-today)
         ("C-z C-d C-f" . org-roam-dailies-find-tomorrow)
         ("C-z C-d C-b" . org-roam-dailies-find-yesterday)
         ("C-z C-d C-n" . org-roam-dailies-find-next-note)
         ("C-z C-d C-p" . org-roam-dailies-find-previous-note))

  :config
  (setq org-roam-directory "~/code/org-roam")
  (make-directory org-roam-directory t))

;; Org Roam Protocol
;; -----------------------------------
(use-package org-roam-protocol
  :ensure org
  :after org)

;; Bibtex completion
;; -----------------------------------
(use-package helm-bibtex
  :ensure t
  :after helm

  ;; :bind (("C-c [" . helm-bibtex))

  :config
  (setq bibtex-completion-bibliography
        '("~/bibliography.bib" "~/.bibliography.bib")

        bibtex-completion-pdf-field "file"

        bibtex-completion-pdf-open-function
        (lambda (fpath) (call-process "zathura" nil 0 nil fpath))))

;; Org Roam Bibtex
;; -----------------------------------
;; Install ruby: pacman -S rubygems
;; Install anystyle-cli: gem install anystyle-cli
(use-package org-roam-bibtex
  :ensure t
  :requires org-roam
  :hook (org-mode . org-roam-bibtex-mode)

  :config (setq

           orb-preformat-keywords
           '("citekey" "entry-type" "date" "pdf?" "note?" "file" "author"
	     "editor" "author-abbrev" "editor-abbrev" "author-or-editor-abbrev"
	     "author-or-editor" "date" "booktitle" "keywords" "url" "year")

           orb-process-file-keyword t

           orb-file-field-extensions '("pdf")
           orb-attached-file-extensions '("pdf")

           orb-templates '(("r" "ref" plain (function org-roam-capture--get-point)
                            ""
                            :file-name "${citekey}"
                            :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}

- tags ::
- keywords :: ${keywords}

* ${title}
:PROPERTIES:
:Custom_ID: ${citekey}
:URL: ${url}
:AUTHOR: ${author-or-editor}
:NOTER_DOCUMENT: ${file}
:NOTER_PAGE:
:END:"))
	   org-roam-capture-ref-templates
	   '(("r" "bibliography reference" plain
              "%?
%^{author} published %^{entry-type} in %^{date}: fullcite:%\\1."

	      :target
	      (file+head "ref/${citekey}.org"
			 "#+title: ${title}\n")
	      :unnarrowed t)))

  (add-to-list 'org-roam-capture-templates
	       '("r" "bibliography reference" plain
		  "%?"
		  :target
		  (file+head "ref/${citekey}.org"
			     "#+title: ${title}
#+PROPERTY: NOTER_DOCUMENT: ${file}

- tags :: 
- keywords :: ${keywords}
- author :: ${author-or-editor}
- url :: ${url}
- date :: ${year}
- booktitle :: ${booktitle}
- file :: [[${file}][PDF File]]

")
		  :unnarrowed t)))

(provide 'org-roam-setup)
