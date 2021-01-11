;; Latex Mode
(use-package tex
  :ensure auctex
  :init
  (defun bvr-latex-mode-hook ()
    "Creates Latex Mode hook"
    (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
    (setq reftex-default-bibliography '("biblio.bib"))
    ;; (reftex-mode t)
    (auto-fill-mode t)
    (local-set-key (kbd "'") 'TeX-insert-single-quote)
    )

  :hook
  ((TeX-mode-hook . turn-on-reftex)
   (TeX-mode-hook . flyspell-mode)
   (TeX-mode-hook . (lambda () (TeX-fold-mode 1)))
   (LaTeX-mode-hook . bvr-latex-mode-hook)

   ;; for synctex integration
   (LaTeX-mode-hook . TeX-PDF-mode)
   (LaTeX-mode-hook . TeX-source-correlate-mode))

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

  (add-to-list 'bibtex-BibTeX-entry-alist
               '("patent" "Patent"
                 (("author") ("title"))
                 (("year"))
                 (("number") ("type"))))

  (add-to-list 'bibtex-biblatex-entry-alist
               '("patent" ""
                 (("author") ("title"))
                 (("year") ("number"))
                 (("type")))))


;; ;; https://tex.stackexchange.com/a/183814 for synctex
;; (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
;; (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
;; (setq TeX-source-correlate-method 'synctex)
;; (setq TeX-source-correlate-start-server t)

;; Latexmk setup
(use-package auctex-latexmk
  :ensure t

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
