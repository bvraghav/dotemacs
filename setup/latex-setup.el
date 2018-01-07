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
(add-hook 'TeX-mode-hook 'reftex-mode)
(add-hook 'TeX-mode-hook 'flyspell-mode) ;; use C-, and C-. for next-error and auto-correct
(add-hook 'LaTeX-mode-hook 'bvr-latex-mode-hook)

(require 'auctex-latexmk)
(auctex-latexmk-setup)

(provide 'latex-setup)
