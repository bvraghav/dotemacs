;;; org-setup.el --- Set up Org Mode -*- lexical-binding: t -*-

;; Author: B.V. Raghav
;; Maintainer: B.V. Raghav
;; Version: 0.1
;; Package-Requires: (get-dpi yas-setup)
;; Homepage: https://github.com/bvraghav/dotemacs.git
;; Keywords: org-mode,setup


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Setup for Org Mode with BVR's Dotemacs.

;;; Code:


;; Org Mode
(require 'yas-setup)
(require 'get-dpi)


;; Change number of lines in org emphasis
;; --------------------------------------------
;; Necessary on top because, anything that invokes
;; (require 'org) prior to this will override this
;; setting.
(setq org-emphasis-regexp-components
      '("-[:space:]('\"{" "-[:space:].,:!?;'\")}\\[" "[:space:]" "." 4)

      )



;; Org Mode Latex Export Syntax Highlighting
;; Include the latex-exporter
;; (require 'ox-latex)
;; ;; Add minted to the defaults packages to include when exporting.
;; (add-to-list 'org-latex-packages-alist '("" "minted"))
;; ;; Tell the latex export to use the minted package for source
;; ;; code coloration.
;; (setq org-latex-listings 'minted)
;; ;; Let the exporter use the -shell-escape option to let latex
;; ;; execute external programs.
;; ;; This obviously and can be dangerous to activate!
;; (setq org-latex-pdf-process
;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; ;; Org latex documentclass
;; (add-to-list 'org-latex-classes
;;              `("booksansparts"
;;                "\\documentclass{book}"
;;                ("\\chapter{%s}" . "\\chapter*{%s}")
;;                ("\\section{%s}" . "\\section*{%s}")
;;                ("\\subsection{%s}" . "\\subsection*{%s}")
;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;; 	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
;; 	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
;;              )

;; Ob-Http
(use-package ob-http
  :ensure t)

;; ;; Org-Beautify
;; ----------------------------------------------------
;; Update: [2023-02-11 Sat]
;; ----------------------------------------------------
;; This one is really problematic with daemon
;; invocation, because it invokes x-font-list, which
;; in-turn painfully requires a display.
;; ----------------------------------------------------
;; Also, a prior installation shows that font
;; customisations are not allowed. So wait until
;; org-mode implements this functionality all by
;; itself!
;; ----------------------------------------------------
;; (use-package org-beautify-theme :ensure t)

(defun bvr/org-set-face ()
  "Set default agenda face to 14pt."
  (face-remap-set-base
   'default
   :height (if (< 192 (get-dpi)) 126 92)
   :weight 'extralight))


;; Org
(use-package org
  :ensure t

  :after ob-http

  :mode "\\.org\\/[^.]*\(.org\)?\\'"
  :hook ((org-mode        . bvr-org-setup)
         (org-mode        . bvr/org-set-face)
         (org-agenda-mode . bvr/org-set-face))

  :bind (("C-c l"   . org-store-link)
	 ("C-c a"   . org-agenda)
	 ("C-c C-a" . org-agenda)
	 ("C-c c"   . org-capture)
         ("C-c b"   . org-backward-heading-same-level))


  :init
  (require 'jupyter)

  ;; Define org mode as default
  (setq-default major-mode 'org-mode)


  ;; Mode customizations for Org mode
  (defun bvr-org-setup ()
    "Basic Setup for Org Mode --- BVR"
    (interactive)
    (org-indent-mode t)
    (auto-fill-mode t)
    (flyspell-mode t)
    (typo-mode t)

    (require 'yasnippet)
    (yas-minor-mode-on)

    (setq org-log-done 'time)

    ;; Use python as language for `#begin_src jupyter' blocks
    (require 'jupyter)
    (org-babel-jupyter-override-src-block "python")

    )
  ;; (add-hook 'org-mode-hook #'bvr-org-setup)


  :config
  (require 'ob)
  (require 'org-tempo)

  ;; Org Babel Evaluate Confirmation not for ipython codes or shell:
  (setq bvr/org-babel-lang
        '("jupyter"
          "jupyter-python"
          "python"
          "shell"
          "bash"
          "sh"
          "emacs-lisp"
          "elisp"
          "lisp"
          "js"
          "http"))
  (defun bvr/org-confirm-babel-evaluate (lang body)
    (not (member lang bvr/org-babel-lang)))
  (setq org-confirm-babel-evaluate 'bvr/org-confirm-babel-evaluate)

  ;; Org TODO Keywords
  (setq org-todo-keywords
	'((sequence "TODO(t)"
                    "TOREAD(r)"
                    "URGENT(u)"
                    "PROCESSING(p)"
                    "ONHOLD(h)"
                    "|"
                    "DONE(d)"
                    "ABANDONED(a!)"
                    "CANCELLED(c!)")
	  (sequence "DONOT(D)" "|")))

  (setq org-todo-keyword-faces '(("PROCESSING" . "#55aaff")
                                 ("ONHOLD" . "#ddbb55")))

  ;; Org Babel Load Languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((js . t)
     (http . t)
     (emacs-lisp . t)
     (R . t)
     (shell . t)
     (python . t)
     (jupyter . t)
     (perl . t)
     (dot . t)
     (gnuplot . t)
     (sql . t)
     (lisp . t)
     (scheme . t)))

  ;; Add org link for sc
  (org-add-link-type
   "fm" nil
   (lambda (path desc format)
     (cond
      ((eq format 'html)
       (cond
	((equal path "sc")
	 (format "<span style=\"font-variant:small-caps; text-transform: lowercase\">%s</span>"
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
	 (format "<span style=\"font-variant:small-caps; text-transform: lowercase\">%s</span>" desc))
	;; more code for it, bf, tt etc.
	))
      (t Y))))

  ;; Variables
  (setq org-default-notes-file "~/code/org/notes.org" ; notes

        ;; Agenda Files (and folders)
        org-agenda-files '("~/code/org" "~/code/org-roam" "~/code/org-roam/daily")

	;; Exporter
	org-export-backends
	(quote
	 (ascii beamer html icalendar latex md odt koma-letter))
	org-export-global-macros (quote (("sc" . "[[fm:sc][$1]]") ("tt" . "[[fm:tt][$1]]")))

	;; HTML Exporter
	org-html-head-extra
	"<style>
  pre.src {background: #3f3f3f; color: #dcdccc}
  #content {max-width: 600px; margin: auto}
  #text-table-of-contents ul {list-style: none; margin: 0; padding: 0}
</style>"
	org-html-postamble-format
	(quote
	 (("en" "<p class=\"date\">Updated <strong>%T</strong></p>
<p class=\"author\">by <strong>%a</strong> (%e)</p>
<p class=\"validation\">%v</p>")))

	;; Apps
	org-file-apps
	(quote
	 ((auto-mode . emacs)
	  ("\\.mm\\'" . default)
	  ("\\.x?html?\\'" . default)
	  ("\\.pdf\\'" . "zathura %s")
	  ("\\.png\\'" . "feh %s")
	  ("\\.jpg\\'" . "feh %s")
	  ("\\.gif\\'" . "feh %s")))

	;; Images
	org-image-actual-width (if (< 192 (get-dpi))
                                   '(768)
                                 '(320))

	;; Babel languages
	org-src-lang-modes
	(quote
	 (("jupyter-python" . python)
	  ("js" . js2)
	  ("http" . ob-http)
	  ("ipython" . python)
	  ("ocaml" . tuareg)
	  ("elisp" . emacs-lisp)
	  ("ditaa" . artist)
	  ("asymptote" . asy)
	  ("dot" . fundamental)
	  ("sqlite" . sql)
	  ("calc" . fundamental)
	  ("C" . c)
	  ("cpp" . c++)
	  ("C++" . c++)
	  ("screen" . shell-script)
	  ("shell" . sh)
	  ("bash" . sh)))

        ;; Org Refile
        org-refile-targets '((org-agenda-files :maxlevel . 1))
        org-archive-location "::* Archived Tasks"

        ;; Org Node Properties 
        org-use-property-inheritance t

        ;; Org user labels
        org-latex-prefer-user-labels t

        ;; Latex preview scale
        ;; https://emacs.stackexchange.com/a/30318
        ;; aka font size for latex
        ;; --------------------------------------------
        ;; Using DVIPNG
        ;; ---------------
        org-preview-latex-default-process 'dvipng
        ;; ---------------
        org-format-latex-options
        (plist-put org-format-latex-options
                   :scale 1.8)
        ;; --------------------------------------------
        ;; Using DVISVGM
        ;; ---------------
        ;; When using with dvisvgm the scale required,
        ;; is much smaller.
        ;; ---------------
        ;; org-preview-latex-default-process 'dvisvgm
        ;; ---------------
        ;; org-format-latex-options
        ;; (plist-put org-format-latex-options
        ;;            :scale 1.2)
        ;; --------------------------------------------

        ;; Startup with LaTeX Preview
        ;; --------------------------------------------
        org-startup-with-latex-preview t
        ;; --------------------------------------------

        ;; Org Priority Cookies
        ;; --------------------------------------------
        org-priority-lowest ?E
        org-priority-highest ?A
        org-priority-default ?D
        org-priority-faces
        '((?A  :foreground "orange" :weight bold)
          (?B  :foreground "gold" :weight normal)
          (?C  :foreground "yellow" :weight normal)
          (?D  :foreground "pale turquoise" :weight extralight)
          (?E  :foreground "Dodgerblue3" :weight extralight))
        ;; --------------------------------------------
        )

  ;; Default Properties
  (setq org-default-properties
        (append org-default-properties
                '("header-args"
                  "header-args+"
                  "header-args:sh"
                  "header-args:sh+"
                  "header-args:elisp"
                  "header-args:elisp+"
                  "header-args:python"
                  "header-args:python+")))

  ;; Org Link Abbreviations
  (setq org-link-abbrev-alist
        '(("ddg"      . "https://duckduckgo.com/?q=%h")
          ("search"   . "https://duckduckgo.com/?q=%h")))

  ;; Ob async
  (require 'ob-async)
  )

;; Ob-Async
(use-package ob-async
  :ensure t
  :after org
  :config
  (setq ob-async-no-async-languages-alist '("python" "jupyter-python"))
  )

(use-package org-attach
  :after org
  :config
  (setq org-attach-use-inheritance t))
  

;; Org-Autolist
(use-package org-autolist
  :ensure t
  :after org
  :hook (org-mode . org-autolist-mode)
  )


;; Org Mode Latex Export Syntax Highlighting
;; Include the latex-exporter
(use-package ox-latex
  :ensure org

  :config
  ;; Add minted to be exported by default
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  ;; Notify latex exporter about minted for source coloration
  (setq org-latex-listings 'minted

	;; ;; latex exporter cli
	;; org-latex-pdf-process
	;; '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
	org-latex-pdf-process
	'("latexmk -f -interaction=nonstopmode -output-directory=%o %f"))

  ;; latex document class(es)
  (add-to-list 'org-latex-classes
	       `("booksansparts"
		 "\\documentclass{book}"
		 ("\\chapter{%s}" . "\\chapter*{%s}")
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
	       `("letter" "\\documentclass{letter}" nil)))


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

;; ----------------------------------------------------
;; Org Ref
;; ----------------------------------------------------
(use-package org-ref
  :ensure t
  :after ob-http
  :hook (org-mode . ref-link-keymap)
  :config
  (setq org-ref-default-bibliography
        '("~/bibliography.bib" "~/.bibliography.bib"))

  (defun ref-link-keymap ()
    (define-key org-mode-map (kbd "C-c C-x [")
      #'org-ref-insert-link)
    (define-key org-mode-map (kbd "C-c C-x )")
      #'org-ref-insert-ref-link)))
;; ----------------------------------------------------

;; ----------------------------------------------------
;; Org Rifle
;; ----------------------------------------------------
(use-package helm-org-rifle
  :ensure t
  :after (helm dash f s))
;; ----------------------------------------------------
;; PATCH: helm-org-rifle.el L788
;; ----------------------------------------------------
;; 
;; Warning (emacs): Helm source ‘file1.org’: after-init-hook Should be defined as a symbol Disable showing Disable logging
;; ----------------------------------------------------
;; Fixed here : https://github.com/alphapapa/org-rifle/pull/77
;; ----------------------------------------------------
;; But not yet pushed to upstream. So may have to patch like this commit:
;; https://github.com/alphapapa/org-rifle/pull/77/commits/5f480ae651fd1f1842a637d79f2154caf36c4dfe
;; ----------------------------------------------------
;; helm-org-rifle.el L708 near
;; (defun helm-org-rifle-get-source-for-buffer (buffer) ...)
;; quote the hook name like this:
;; ----------------------------------------------------
;; :after-init-hook 'helm-org-rifle-after-init-hook


(provide 'org-setup)

;;; org-setup.el ends here
