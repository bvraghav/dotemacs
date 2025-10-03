;; Run ggtags if projectile root folder available
;; ====================================================
(defun bvr-python-mode-hook ()
  (customize-set-variable 'indent-tabs-mode nil)
  ;; (ggtags-mode 1)
  (projectile-mode 1)
  (hs-minor-mode)

  ;; FIXME:
  ;; --------------------------------------------------
  ;; helm-search in python-mode buffer fails
  ;; with error:
  ;; 
  ;; Debugger entered--Lisp error:
  ;; (outline-before-first-heading)
  ;; --------------------------------------------------
  ;; But with outline-minor-mode disabled; the error
  ;; disappears

  ;; (outline-minor-mode)

  ;; Use pyright flycheck checker
  (setq flycheck-checker 'python-pyright))

(use-package python
  :ensure t
  :hook (python-base-mode . bvr-python-mode-hook)
  :init
  (setq python-indent-offset 2)
  :bind
  (("C-M-t" . treemacs-select-window)
   ;; ("M-." . lsp-goto-type-definition)
   ;; ("M-]" . lsp-find-references)
   ))

(use-package conda
  :ensure t
  :defer t
  :init
  (setq conda-anaconda-home (expand-file-name "~/miniconda3")
        conda-env-home-directory (expand-file-name "~/miniconda3"))
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell))

(use-package jupyter
  :ensure t
  :defer t
  :init
  (setq org-babel-default-header-args:jupyter-python
        '((:async . "yes")
          (:session . "py")))
  :config
  ;; Push this to org-setup
  ;; (org-babel-jupyter-override-src-block "python")
  )

(use-package python-docstring
  :ensure t)

;; Minor mode to edit python docstrings as markdown.
;; ----------------------------------------------------
;; https://github.com/mangalam-research/software-standards/blob/master/emacs/mmm-rst-python.el
;; 
;; I tried using markdown mode instead and this seems
;; to be a disaster on performance. Not recommended!!
;; ----------------------------------------------------

;; Use edit-indirect-buffer for python docstring
;; ----------------------------------------------------
(use-package edit-indirect :ensure t)
(require 'edit-indirect)

(defun bvr/python/edit-indirect-docstring-at-pt ()
  "Edit python doctring at point in indirect-buffer."
  (interactive)
  (cl-labels ((beg-docstring? ()
                (buffer-substring (beg-quote)
                                  (+ 3 (beg-quote))))
              (in-string? () (nth 3 (syntax-ppss)))
              (beg-of-docstring () (1+ (nth 8 (syntax-ppss))))
              (beg-quote () (nth 2 (syntax-ppss)))
              (end-of-docstring ()
                (save-excursion
                  (and (search-forward-regexp "\"\"\"\\|\'\'\'" nil t)
                       (- (point) 3)))))
    (let* ((current-point (point))
           (docstring? (and (in-string?)
                            (beg-docstring?)))
	   (part-beg (and docstring? (beg-of-docstring)))
	   (part-end (and docstring? (end-of-docstring)))
           (docstring? (and docstring? part-end))
	   (part-buf (format "*MD Docstring %s*" (buffer-name))))
      ;; (message "part-beg: %s" part-beg)
      ;; (message "part-end: %s" part-end)
      ;; (message "part-buf: %s" part-buf)
      (when docstring?
        (edit-indirect-region part-beg part-end 't)
        (rename-buffer part-buf)
        (markdown-mode)
        ;; (message "point-min: %s" (point-min))
        (goto-char (point-min))
        ;; (message "pos-bol: %s" (pos-bol 2))
        (kill-rectangle (pos-bol 2) (point-max))
        ;; (message "point-max: %s" (point-max))
        ;; TODO Try to restore point without this dirty hack
        (goto-char (- (1+ current-point) part-beg))))))

(defun bvr/python/edit-indirect-docstring-maybe ()
  "Edit indirect: python docstring on the line at point."
  (interactive)
  (cl-labels ((edit-docstring-maybe (p)
                (save-excursion (goto-char p)
                                (call-interactively
                                 #'bvr/python/edit-indirect-docstring-at-pt))))
    (or (edit-docstring-maybe (point))
        (edit-docstring-maybe (pos-eol))
        (edit-docstring-maybe (pos-bol))
        (message "Not inside a docstring."))))

(define-key python-base-mode-map
            (kbd "C-c '")
            #'bvr/python/edit-indirect-docstring-maybe)
;; ----------------------------------------------------

(provide 'python-setup)
