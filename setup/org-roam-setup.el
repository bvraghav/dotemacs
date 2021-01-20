;; Document viewers setup
;; -----------------------------------
;; PDF Tools
(use-package pdf-tools
  :ensure t
  :pin manual

  :demand

  :hook (pdf-view-mode . pdf-view-midnight-minor-mode)

  :config
  (require 'pdf-tools)
  (require 'pdf-occur)
  ;; (pdf-tools-install)
  )

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

  :hook (after-init . org-roam-mode)

  :bind (("C-z C-d C-d" . org-roam-dailies-find-date)
         ("C-z C-d d"   . org-roam-dailies-find-date)
         ("C-z C-d C-z" . org-roam-dailies-find-today)
         ("C-z C-d C-f" . org-roam-dailies-find-tomorrow)
         ("C-z C-d C-b" . org-roam-dailies-find-yesterday)
         ("C-z C-d C-n" . org-roam-dailies-find-next-note)
         ("C-z C-d C-p" . org-roam-dailies-find-previous-note))

  :config
  (setq org-roam-directory "~/org-roam")
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

  :config
  (setq bibtex-completion-bibliography
        '("~/bibliography.bib")

        bibtex-completion-pdf-field "file"))

;; Org Roam Bibtex
;; -----------------------------------
;; Install ruby: pacman -S rubygems
;; Install anystyle-cli: gem install anystyle-cli
(use-package org-roam-bibtex
  :ensure t
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)

  :config (setq

           orb-preformat-keywords
           '("citekey" "title" "url" "author-or-editor" "keywords" "file")

           orb-process-file-keyword t

           orb-file-field-extensions '("pdf")

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
:END:"))))

(provide 'org-roam-setup)
