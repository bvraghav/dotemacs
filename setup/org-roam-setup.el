;;; org-roam-setup.el --- Setting up Org Roam.
;;; ---------------------------------------------------

;; Author: Raghav B. Venkataramaiyer
;; Maintainer: Raghav B. Venkataramaiyer
;; Version: 0.1
;; Package-Requires: (org nov org-noter org-download org-roam org-roam-protocol org-roam-bibtex helm-bibtex helm-org-rifle)
;; Homepage: https://github.com/bvraghav/dotemacs/
;; Keywords: Org Roam Setup


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute
;; it and/or modify it under the terms of the GNU
;; General Public License as published by the Free
;; Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will
;; be useful, but WITHOUT ANY WARRANTY; without even
;; the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.

;; You should have received a copy of the GNU General
;; Public License along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.


;;; Commentary:
;;; ---------------------------------------------------
;;; Setting up Org Roam.

;;; Code:
;;; ---------------------------------------------------

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
    (face-remap-add-relative 'variable-pitch nil))
  (setq nov-text-width 55))

;; Org Noter
;; -----------------------------------
;; (use-package org-noter
;;   :ensure t)

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

  :bind (("C-z C-d C-d" . org-roam-dailies-goto-date)
         ("C-z C-d d"   . org-roam-dailies-goto-date)
         ("C-z C-d C-z" . org-roam-dailies-goto-today)
         ("C-z C-d C-f" . org-roam-dailies-goto-tomorrow)
         ("C-z C-d C-b" . org-roam-dailies-goto-yesterday)
         ("C-z C-d C-n" . org-roam-dailies-goto-next-note)
         ("C-z C-d C-p" . org-roam-dailies-goto-previous-note)
         ("C-z C-d SPC" . bvr/helm-org-roam-daily-rifle)
         ("C-z SPC"     . bvr/helm-org-roam-rifle)
         ("C-z C-d C-SPC" . bvr/helm-org-roam-daily-rifle)
         ("C-z C-SPC  " . bvr/helm-org-roam-rifle))

  :config
  (setq org-roam-directory "~/code/org-roam")
  (make-directory org-roam-directory t))

;; Org Roam Protocol
;; -----------------------------------
(use-package org-roam-protocol
  :ensure org
  :after org)

;; Bibtex completion
;; ----------------------------------------------------
;; TODO: Migrate this configuration to
;; latex-setup.el:155-191.
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


;; ----------------------------------------------------
;; Hacking up org-rifle to show buffer name
;; ----------------------------------------------------

(require 'helm-org-rifle)


(cl-defmacro bvr/helm-org-rifle-define-command (name args docstring &key sources (let nil) (transformer nil) (buffer nil))
  "Define interactive helm-org-rifle command, which will run the appropriate hooks.
Helm will be called with vars in LET bound."
  `(cl-defun ,(intern (concat "bvr/helm-org-rifle" (when (s-present? name) (concat "-" name)))) ,args
     ,docstring
     (interactive)
     (unwind-protect
         (progn
           (run-hooks 'helm-org-rifle-before-command-hook)
           (let* ((helm-candidate-separator " ")
                  ,(if transformer
                       ;; I wish there were a cleaner way to do this,
                       ;; because if this `if' evaluates to nil, `let' will
                       ;; try to set `nil', which causes an error.  The
                       ;; choices seem to be to a) evaluate to a list and
                       ;; unsplice it (since unsplicing `nil' evaluates to
                       ;; nothing), or b) return an ignored symbol when not
                       ;; true.  Option B is less ugly.
                       `(helm-org-rifle-transformer ,transformer)
                     'ignore)
                  ,@let)
             (helm :sources ,sources :buffer ,buffer)))
       (run-hooks 'helm-org-rifle-after-command-hook))))

(bvr/helm-org-rifle-define-command
 "files" (&optional files &key (buffer nil))
 "Rifle through FILES, where FILES is a list of paths to Org files.
If FILES is nil, prompt with `helm-read-file-name'.  All FILES
are searched; they are not filtered with
`helm-org-rifle-directories-filename-regexp'."
 :sources (--map (helm-org-rifle-get-source-for-file it) files)
 :buffer buffer
 :let ((files (helm-org-rifle--listify (or files
                                           (helm-read-file-name "Files: " :marked-candidates t))))
       (helm-candidate-separator " ")
       (helm-cleanup-hook (lambda ()
                            ;; Close new buffers if enabled
                            (when helm-org-rifle-close-unopened-file-buffers
                              (if (= 0 helm-exit-status)
                                  ;; Candidate selected; close other new buffers
                                  (let ((candidate-source (helm-attr 'name (helm-get-current-source))))
                                    (dolist (source helm-sources)
                                      (unless (or (equal (helm-attr 'name source)
                                                         candidate-source)
                                                  (not (helm-attr 'new-buffer source)))
                                        (kill-buffer (helm-attr 'buffer source)))))
                                ;; No candidates; close all new buffers
                                (dolist (source helm-sources)
                                  (when (helm-attr 'new-buffer source)
                                    (kill-buffer (helm-attr 'buffer source))))))))))


(defun bvr/helm-org-rifle-directories
    (&optional directories toggle-recursion buffer)
  "Rifle through Org files in DIRECTORIES.
DIRECTORIES may be a string or list of strings.  If DIRECTORIES
is nil, prompt with `helm-read-file-name'.  With prefix or
TOGGLE-RECURSION non-nil, toggle recursion from the default.
Files in DIRECTORIES are filtered using
`helm-org-rifle-directories-filename-regexp'."
  ;; This does not need to be defined with helm-org-rifle-define-command because it calls helm-org-rifle-files which is.
  (interactive)
  (let* ((recursive (if (or toggle-recursion current-prefix-arg)
                        (not helm-org-rifle-directories-recursive)
                      helm-org-rifle-directories-recursive))
         (directories (helm-org-rifle--listify
                       (or directories
                           (-select 'f-dir? (helm-read-file-name "Directories: " :marked-candidates t)))))
         (files (-flatten (--map (f-files it
                                          (lambda (file)
                                            (s-matches? helm-org-rifle-directories-filename-regexp (f-filename file)))
                                          recursive)
                                 directories))))
    (if files
        (bvr/helm-org-rifle-files files :buffer buffer)
      (error "No org files found in directories: %s" (s-join " " directories)))))


(defun bvr/helm-org-roam-daily-rifle ()
  "Search org-roam-dailies with helm-org-rifle."
  (interactive)
  (let* ((dir (expand-file-name org-roam-dailies-directory
                                org-roam-directory)))
    (bvr/helm-org-rifle-directories
     dir nil
     "Helm-Org-Rifle : Org Roam Dailies")))

(defun bvr/helm-org-roam-rifle ()
  "Search org-roam with helm-org-rifle."
  (interactive)
  (let* ((helm-org-rifle-directories-recursive nil)
         (pdir org-roam-directory)
         (jdir (expand-file-name "journal" pdir))
         (rdir (expand-file-name "ref" pdir)))
    (bvr/helm-org-rifle-directories
     (list pdir jdir rdir) nil
     "Helm-Org-Rifle : Org Roam Root/ Jounal/ Ref")))

;; ----------------------------------------------------
;; DONE: Hacking up org-rifle to show buffer name
;; ----------------------------------------------------

(provide 'org-roam-setup)
;;; org-roam-setup.el ends here
