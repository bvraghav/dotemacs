;; Latex Mode
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
    (add-to-list 'TeX-command-list command)))

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
