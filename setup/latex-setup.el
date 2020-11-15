;; Latex Mode
(require 'tex)

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

(defun bvr-latex-mode-hook ()
  "Creates Latex Mode hook"
  (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
  (setq reftex-default-bibliography '("biblio.bib"))
  ;; (reftex-mode t)
  (auto-fill-mode t)
  (local-set-key (kbd "'") 'TeX-insert-single-quote)
)

;; (add-hook 'TeX-mode-hook 'zotelo-minor-mode)
;; ^\^\^\
;; Should be removed completely since zotelo depends upon XUL Runner
;; based version of zotero. XUL Runner was discontinued by mozilla,
;; upon which the front end of zotero is based!!

;; (add-hook 'TeX-mode-hook 'reftex-mode)
(add-hook 'TeX-mode-hook 'turn-on-reftex) ;; Official docs support this now... 
(add-hook 'TeX-mode-hook 'flyspell-mode) ;; use C-, and C-. for next-error and auto-correct
(add-hook 'TeX-mode-hook (lambda ()
			   (TeX-fold-mode 1)))
(add-hook 'LaTeX-mode-hook 'bvr-latex-mode-hook)

;; https://tex.stackexchange.com/a/183814 for synctex
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(setq TeX-source-correlate-method 'synctex)
(setq TeX-source-correlate-start-server t)

;; Latexmk setup
(require 'auctex-latexmk)
(auctex-latexmk-setup)

;; Setup SyncTex with Zathura
(with-eval-after-load "tex"
  ;; enable synctex support for latex-mode
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
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
