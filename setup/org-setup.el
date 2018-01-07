;; Org Mode
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


(eval-after-load 'org-mode
  (lambda () 
    (define-key org-mode-map (kbd "<f9> n") 'my-next-image)
    (define-key org-mode-map (kbd "<f9> p") 'my-prev-image)
    (define-key org-mode-map (kbd "<f9> i") 'my-insert-current-image-path)))

;; Mode customizations for Org mode
(defun bvr-org-setup ()
  "Basic Setup for Org Mode --- BVR"
  (org-indent-mode t)
  (auto-fill-mode t)
  (flyspell-mode t)
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

;; org Babel Setup
(require 'org)

(provide 'org-setup)
