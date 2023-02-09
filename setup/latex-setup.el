;; Latex Mode

;; ====================================================
;; CITATION
;; ----------------------------------------------------

(use-package helm-bibtex :ensure t)
(require 'helm-bibtex)

(defvar bvr/latex-cite-source
  (helm-build-sync-source
      "BibTeX entries for LaTeX (BVR)"
    :header-name
    (lambda (name)
      (format "%s%s: " name (if helm-bibtex-local-bib
                                " (local)"
                              "")))
    :candidates 'helm-bibtex-candidates
    :filtered-candidate-transformer
    #'helm-bibtex-candidates-formatter

    :action #'helm-bibtex-insert-citation)
  "Source for searching in BibTeX files.")


;;;###autoload
(defun bvr/latex-cite (&optional arg local-bib input)
  "Insert citation into a LaTeX source after searching
amongst BibTeX entries. Adapted from HELM-BIBTEX.


With a prefix ARG, the cache is invalidated and the
bibliography reread.

If LOCAL-BIB is non-nil, display that the BibTeX
entries are read from the local bibliography.

If INPUT is non-nil and a string, that value is going
to be used as a predefined search term.  Can be used to
define functions for frequent searches (e.g. your own
publications)."
  (interactive "P")
  (when arg
    (bibtex-completion-clear-cache))
  (bibtex-completion-init)
  (let* ((candidates (bibtex-completion-candidates))
         (key (bibtex-completion-key-at-point))
         (preselect
          (and key (cl-position-if
                    (lambda (cand)
                      (member (cons "=key=" key)
                              (cdr cand)))
                    candidates))))

    (helm :sources (list bvr/latex-cite-source)
          :full-frame nil       ;helm-bibtex-full-frame
          :buffer "*BVR LaTeX Cite*"
          :input input
          :preselect
          (lambda () (and preselect
                          (> preselect 0)
                          (helm-next-line preselect)))
          :candidate-number-limit
          (max 500 (1+ (or preselect 0)))

          ;; The following are forwarded to :SOURCES
          :bibtex-candidates candidates
          :bibtex-local-bib local-bib)))

;; ====================================================

(use-package tex
  :ensure auctex
  :init
  (progn (defun bvr-latex-mode-hook ()
           "Creates Latex Mode hook"
           (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
           (setq reftex-default-bibliography '("biblio.bib"))
           ;; (reftex-mode t)
           (auto-fill-mode)
           (local-set-key (kbd "'") 'TeX-insert-single-quote))

         (defun bvr/reftex-mode-hook ()
           (define-key reftex-mode-map (kbd "C-c [") nil)))
  

  :hook
  ((TeX-mode . turn-on-reftex)
   (TeX-mode . flyspell-mode)
   (TeX-mode . (lambda () (TeX-fold-mode 1)))
   (LaTeX-mode . bvr-latex-mode-hook)

   ;; for synctex integration
   (LaTeX-mode . TeX-PDF-mode)
   (LaTeX-mode . TeX-source-correlate-mode)

   ;; Use helm-bibtex instead of reftex citation
   (reftex-mode . bvr/reftex-mode-hook)
   )

  :bind ("C-c [" . bvr/latex-cite)

  :config
  (setq TeX-source-correlate-method 'synctex
	TeX-source-correlate-start-server t
	TeX-electric-math '("$" . "$"))
  
  (defadvice TeX-insert-quote (around wrap-region activate)
    (cond
     (mark-active
      (let ((skeleton-end-newline nil))
	(skeleton-insert `(nil ,TeX-open-quote _ ,TeX-close-quote) -1)))
     ((looking-at (regexp-opt (list TeX-open-quote TeX-close-quote)))
      (forward-char (length TeX-open-quote)))
     (t
      ad-do-it)))
  (put 'TeX-insert-quote 'delete-selection nil)

  (defun TeX-insert-single-quote (arg)
    (interactive "p")
    (cond
     (mark-active
      (let ((skeleton-end-newline nil))
	(skeleton-insert
	 `(nil ?` _ ?') -1)))
     ((or (looking-at "\\<")
	  (looking-back "^\\|\\s-\\|`"))
      (insert "`"))
     (t
      (self-insert-command arg))))

  (dolist (command '(("LatexMk" "latexmk %(-PDF)%S%(mode) %(file-line-error) %(extraopts) %t"
                      TeX-run-latexmk nil
                      (plain-tex-mode latex-mode doctex-mode)
                      :help "Run LatexMk")
                     ("MultiFileLaTeX" "latexmk" TeX-run-shell nil t)))
    (add-to-list 'TeX-command-list command))

  ;; Scaling latex preview for hidpi
  ;; https://emacs.stackexchange.com/a/30310
  (require 'get-dpi)
  ;; (and (< 192 (string-to-number (shell-command-to-string "get_dpi")))
  ;;      (setq preview-scale-function (lambda () 0.6)))
  (and (< 192 (get-dpi))
       (setq preview-scale-function (lambda () 0.6)))

  )

(use-package bibtex
  ;; :ensure auctex
  ;; :after tex
  :config

  (add-to-list 'bibtex-BibTeX-entry-alist
               '("patent" "Patent"
                 (("author") ("title"))
                 (("year"))
                 (("number") ("type"))))

  (add-to-list 'bibtex-biblatex-entry-alist
               '("patent" ""
                 (("author") ("title"))
                 (("year") ("number"))
                 (("type"))))


  ;; From John Kitchin's Configuration
  ;; https://github.com/jkitchin/org-ref#configuration
  (setq bibtex-completion-bibliography '("~/bibliography.bib"
					 "~/.bibliography.bib")
	bibtex-completion-notes-path "~/org-roam/ref/"
	bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"

	bibtex-completion-additional-search-fields '(keywords tags)
	bibtex-completion-display-formats
	'((t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:16} ${title:*}"))
	;; '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:16} ${title:*} ${journal:40}")
	;;   (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:16} ${title:*} Chapter ${chapter:32}")
	;;   (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:16} ${title:*} ${booktitle:40}")
	;;   (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:16} ${title:*} ${booktitle:40}")
	;;   (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:16} ${title:*}"))
	;; bibtex-completion-pdf-open-function
	;; (lambda (fpath)
	;;   (call-process "open" nil 0 nil fpath))
        ))

;; ;; https://tex.stackexchange.com/a/183814 for synctex
;; (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
;; (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
;; (setq TeX-source-correlate-method 'synctex)
;; (setq TeX-source-correlate-start-server t)

;; Latexmk setup
(use-package auctex-latexmk
  :ensure t

  :after (tex bibtex)

  :config
  (auctex-latexmk-setup))


;; Setup SyncTex with Zathura
(with-eval-after-load "tex"
  ;; add a new view program
  (add-to-list 'TeX-view-program-list
        '(;; arbitrary name for this view program
          "Zathura"
          (;; zathura command (may need an absolute path)
           "zathura"
           ;; %o expands to the name of the output file
           " %o"
           ;; insert page number if TeX-source-correlate-mode
           ;; is enabled
           (mode-io-correlate " --synctex-forward %n:0:%b"))))
  ;; use the view command named "Zathura" for pdf output
  (setcdr (assq 'output-pdf TeX-view-program-selection) '("Zathura")))


(provide 'latex-setup)
