;; Org Mode
(require 'yasnippet)
(require 'org-tempo)

;; Define org mode for .org folder
(add-to-list 'auto-mode-alist '("\\.org\\/[^.]*\(.org\)?\\'" . org-mode))
(setq-default major-mode 'org-mode)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c C-a") #'org-agenda)
;; Org Mode Keymap
(defun my-next-image ()
  (interactive)
  (save-excursion 
    (with-current-buffer "*image-dired*"
      (image-dired-forward-image)
      (image-dired-display-thumbnail-original-image))))

(defun my-prev-image ()
  (interactive)
  (save-excursion 
    (with-current-buffer "*image-dired*"
      (image-dired-backward-image)
      (image-dired-display-thumbnail-original-image))))

(defun my-insert-current-image-path ()
  (interactive)
  (insert
   (concat
    "[["
    (save-excursion
      (with-current-buffer "*image-dired*"
        (image-dired-original-file-name)))
    "]]")))


;; (eval-after-load 'org-mode
;;   (lambda () 
;;     (define-key org-mode-map (kbd "<f9> n") 'my-next-image)
;;     (define-key org-mode-map (kbd "<f9> p") 'my-prev-image)
;;     (define-key org-mode-map (kbd "<f9> i") 'my-insert-current-image-path)))

;; Mode customizations for Org mode
(defun bvr-org-setup ()
  "Basic Setup for Org Mode --- BVR"
  (org-indent-mode t)
  (auto-fill-mode t)
  (flyspell-mode t)
  (yas-minor-mode-on)
  (setq org-log-done 'time))
(add-hook 'org-mode-hook 'bvr-org-setup)

;; Org Mode Latex Export Syntax Highlighting
;; Include the latex-exporter
(require 'ox-latex)
;; Add minted to the defaults packages to include when exporting.
(add-to-list 'org-latex-packages-alist '("" "minted"))
;; Tell the latex export to use the minted package for source
;; code coloration.
(setq org-latex-listings 'minted)
;; Let the exporter use the -shell-escape option to let latex
;; execute external programs.
;; This obviously and can be dangerous to activate!
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; Org latex documentclass
(add-to-list 'org-latex-classes
             `("booksansparts"
               "\\documentclass{book}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
             )

;; Org Ref
(require 'org-ref)

;; Org Capture
(global-set-key (kbd "C-c c") 'org-capture)

;; Org Babel Evaluate Confirmation not for ipython codes or shell:
(setq bvr/org-babel-lang '("ipython" "python" "shell" "bash" "sh" "lisp" "js"))
(defun bvr/org-confirm-babel-evaluate (lang body)
  (not (member lang bvr/org-babel-lang)))
(setq org-confirm-babel-evaluate 'bvr/org-confirm-babel-evaluate)

;; org Babel Setup
(require 'org)

;; Add org link for sc
(org-add-link-type
 "fm" nil
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (cond
      ((equal path "sc")
       (format "<span style=\"font-variant:small-caps;\">%s</span>"
               desc))
      ((equal path "it")
       (format "<em>%s</em>" desc))
      ((equal path "bf")
       (format "<strong>%s</strong>" desc))
      ((equal path "tt")
       (format "<kbd>%s</kbd>" desc))
      (t (format "%s" desc))))
    ;; "</span>" )))
    ((eq format 'latex)
     (format "\\text%s{%s}" path desc))
    ((eq format 'odt)
     (cond
      ((equal path "sc")
       (format "<span style=\"font-variant:small-caps;\">hello</span>" desc))
      ;; more code for it, bf, tt etc.
      ))
    (t Y))))

(provide 'org-setup)
