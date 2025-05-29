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

;; Require cdlatex
(use-package cdlatex :ensure t)

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
   ;; Cascadia code regular seems good enough for most
   ;; of the cases.
   :weight (if (< 192 (get-dpi)) 'light 'regular)))


(defun bvr/org-refiles-from-agenda ()
  "List of refile target files computed from `org-agenda-files'."
  (cl-labels ((create-if-not-exists (fname)
                (let* ((cmd "FNAME=%s; GNAME=${FNAME/agenda/record};
if [[ $FNAME != $GNAME ]] ; then
  [[ -a $FNAME ]] && {
    if [[ -f ${FNAME} ]] ; then [ ! -f ${GNAME} ] && touch ${GNAME} ; echo ${GNAME} ; fi ;
    if [[ -d ${FNAME} ]] ; then [ ! -d ${GNAME} ] && mkdir ${GNAME} ; find ${GNAME} -iname \'*.org\' ; fi ;
  }
else
  if [[ -f ${FNAME} ]] ; then
    echo ${FNAME} ;
  else
    find ${FNAME} -iname \'*.org\' ;
  fi ;
fi ;")
                       (cmd (format cmd fname)))
                  (message "fname: %s" fname)
                  (message "cmd: %s" cmd)
                  (s-lines (s-chomp (shell-command-to-string cmd))))))

    (cl-loop for fname in org-agenda-files with gname
             if (and
                 (setq gname (create-if-not-exists fname))
                 (or (not (stringp gname)) (if (s-blank? gname) nil (list gname))))
             append gname)))



(defun bvr/latex-replace-some-unicode (backend)
  "Ensure (some) unicode chars are properly handled in LaTeX export.

Ensure (some) unicode chars, e.g.  \"“ ” ‘ ’ …\" are properly handled in LaTeX export

This is meant to be used with a hook before export and written
for use with the LaTeX backend.

Usage:
  (add-hook 'org-export-before-parsing-hook
            #'bvr/latex-replace-some-unicode)
"
  (message "[bvr/latex-filter-quotes] Backend: {%s}" backend)
  ;; (message "[bvr/latex-filter-quotes] Info: {%s}" info)
  (when (org-export-derived-backend-p backend 'latex)
    (message "[bvr/latex-filter-quotes] Exporting LaTeX.")
    (save-excursion
      (let* ((text (delete-and-extract-region (point-min) (point-max)))
             (text (replace-regexp-in-string "…" "\\\\ldots" text))
             (text (replace-regexp-in-string "“" "``" text))
             (text (replace-regexp-in-string "”" "''" text))
             (text (replace-regexp-in-string "‘" "`" text))
             (text (replace-regexp-in-string "’" "'" text)))
        (insert text)))))

;; Org
(use-package org
  :ensure t
  :pin gnu

  :after (ob-http cdlatex)

  :mode "\\.org\\/[^.]*\(.org\)?\\'"
  :hook ((org-mode        . bvr-org-setup)
         (org-mode        . bvr/org-set-face)
         (org-mode        . turn-on-org-cdlatex)
         (org-agenda-mode . bvr/org-set-face)
         (org-export-before-parsing . bvr/latex-replace-some-unicode))

  :bind (("C-c l"       . org-store-link)
	 ("C-c a"       . org-agenda)
	 ("C-c C-a"     . org-agenda)
	 ("C-c c"       . org-capture)
         ("C-c b"       . org-backward-heading-same-level)
         :map org-mode-map
         ("C-c C-v r"   . bvr/goto-result-beginning)
         ("C-c C-v C-r" . bvr/goto-result-beginning)
         ("C-c C-v C-t" . bvr/org-babel-tangle-block)
         ("C-c C-`"     . org-cdlatex-mode))


  :init
  (require 'jupyter)

  ;; Define org mode as default
  ;; --------------------------------------------------
  ;; FIXME: https://github.com/bvraghav/dotemacs/issues/10
  ;; (setq-default major-mode 'org-mode)


  ;; Mode customizations for Org mode
  (defun bvr-org-setup ()
    "Basic Setup for Org Mode --- BVR"
    (interactive)
    (org-indent-mode t)
    (auto-fill-mode t)
    (flyspell-mode t)
    (typo-mode t)

    ;; Images
    (setq org-image-actual-width
          (if (< 192 (get-dpi))
              '(768)
            '(320)))

    (require 'yasnippet)
    (yas-minor-mode-on)

    (setq org-log-done 'time)

    ;; Use python as language for `#begin_src jupyter'
    ;; blocks
    ;; ------------------------------------------------
    ;; Note, org-babel-default-header-args:python will
    ;; not be an alias of
    ;; org-babel-default-header-args:jupyter-python,
    ;; the value of the former is merely set to the
    ;; value of the latter after calling
    ;; org-babel-jupyter-override-src-block.
    ;;
    ;; https://github.com/emacs-jupyter/jupyter#org-mode-source-blocks
    (require 'jupyter)
    (org-babel-jupyter-override-src-block "python")
    ;; (org-babel-jupyter-restore-src-block "python")

    ;; Require org-ref
    ;; ------------------------------------------------
    ;; For some reason, this is necessary for latex
    ;; export of org-ref functionality.  I do not
    ;; understand why `(use-package 'org-ref)' isn't
    ;; simply enough!
    (require 'org-ref)
    )
  ;; (add-hook 'org-mode-hook #'bvr-org-setup)


  :config
  (require 'ob)
  (require 'org-tempo)

  ;; Org Babel Evaluate Confirmation not for ipython
  ;; codes or shell:
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
          "http"
          "awk"))
  (defun bvr/org-confirm-babel-evaluate (lang body)
    (not (member lang bvr/org-babel-lang)))
  (setq org-confirm-babel-evaluate
        'bvr/org-confirm-babel-evaluate)

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
   '((C . t)
     (js . t)
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
     (scheme . t)
     (awk . t)))

  ;; Use GAWK as AWK executable
  (setq org-babel-awk-command "/usr/bin/gawk")

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
      ((or (eq format 'beamer) (eq format 'latex))
       (format "\\text%s{%s}" path desc))
      ((eq format 'odt)
       (cond
	((equal path "sc")
	 (format "<span style=\"font-variant:small-caps; text-transform: lowercase\">%s</span>" desc))
	;; more code for it, bf, tt etc.
	))
      (t desc))))

  ;; Default values
  (setq-default

   ;; Latex preview scale
   ;; https://emacs.stackexchange.com/a/30318
   ;; aka font size for latex
   ;; --------------------------------------------
   ;; Using DVIPNG
   ;; ---------------
   org-preview-latex-default-process 'dvipng

   ;; Startup with LaTeX Preview
   ;; --------------------------------------------
   ;; org-startup-with-latex-preview t
   ;; --------------------------------------------
   )

  ;; Variables
  (setq org-default-notes-file "~/code/org/notes.org" ; notes

        ;; Agenda Files (and folders)
        org-agenda-files '("~/code/org/agenda")
        ;; org-agenda-files '("~/code/org" "~/code/org-roam" "~/code/org-roam/daily")

        ;; Refile targets
        org-refile-targets '((bvr/org-refiles-from-agenda :maxlevel . 2)
                             (org-agenda-files :maxlevel . 2))
        ;; org-refile-targets '(("~/code/org/record" :maxlevel . 1))

        ;; To refile as top level headings
        org-refile-use-outline-path 'file

        ;; Archive
        org-archive-location "::* Archived Tasks"

        ;; Columns Default Format
        org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS %SCHEDULED"

	;; Exporter
	org-export-backends
	'(ascii beamer html icalendar latex md odt koma-letter)

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
	  ("bash" . sh)

          ;; This is to avoid the ambiguity between
          ;; latex-mode and LaTeX mode
          ("latex" . LaTeX)))

        ;; Org Node Properties 
        org-use-property-inheritance t

        ;; Org user labels
        org-latex-prefer-user-labels t

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

        ;; Preview LaTeX Compilers
        ;; --------------------------------------------
        org-latex-inputenc-alist '(("utf8" . "utf8x"))
        org-preview-latex-process-alist
        '((dvipng :programs
                  ("latex" "dvipng")
                  :description "dvi > png"
                  :message "you need to install the programs: latex and dvipng."
                  :image-input-type "dvi"
                  :image-output-type "png"
                  :image-size-adjust (1.3 . 1.3)
                  :latex-compiler
                  ("latex -shell-escape -interaction nonstopmode -output-directory %o %f")
                  :image-converter
                  ("dvipng -D %D -T tight -o %O %f")
                  :transparent-image-converter
                  ("dvipng -D %D -T tight -bg Transparent -o %O %f"))
          (xdvsvgm :programs
                   ("xelatex" "dvisvgm")
                   :description "xdv > svg"
                   :message "you need to install the programs: xelatex and dvisvgm."
                   :use-xcolor t
                   :image-input-type "xdv"
                   :image-output-type "svg"
                   :image-size-adjust (1.5 . 1.5)
                   :latex-compiler
                   ("xelatex -shell-escape -no-pdf -interaction nonstopmode -output-directory %o %f")
                   :image-converter
                   ("dvisvgm %f -n -b min -c %S -o %O"))
          (dvisvgm :programs
                   ("latex" "dvisvgm")
                   :description "dvi > svg"
                   :message "you need to install the programs: latex and dvisvgm."
                   :image-input-type "dvi"
                   :image-output-type "svg"
                   :image-size-adjust (1.7 . 1.5)
                   :latex-compiler
                   ("latex -shell-escape -interaction nonstopmode -output-directory %o %f")
                   :image-converter
                   ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O"))
          (imagemagick :programs
                       ("latex" "convert")
                       :description "pdf > png"
                       :message "you need to install the programs: latex and imagemagick."
                       :image-input-type "pdf"
                       :image-output-type "png"
                       :image-size-adjust (1.0 . 1.0)
                       :latex-compiler
                       ("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
                       :image-converter
                       ("convert -density %D -trim -antialias %f -quality 100 %O")))
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

  ;; Require ox-md (https://orgmode.org/manual/Exporting.html)
  (require 'ox-md)
  )

;; Ob-Async
(use-package ob-async
  :ensure t
  :after org
  :config
  (setq ob-async-no-async-languages-alist '("python" "jupyter-python"))
  ;; Ob async
  (require 'ob-async)
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

  ;; Latex document class(es)
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
	       `("letter" "\\documentclass{letter}" nil))
  ;; Install separately:
  ;; https://github.com/bvraghav/qptiet_latex-class#from-source
  (add-to-list 'org-latex-classes
	       `("tiet-question-paper"
                 "\\documentclass{tiet-question-paper}
[NO-DEFAULT-PACKAGES]
\\usepackage{amsmath}
\\usepackage{graphicx}
\\usepackage{wrapfig}
\\usepackage{amssymb}
\\usepackage[unicode]{hyperref}
"
                 nil)))


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
                #'org-ref-cite-insert-helm)
      ;; #'org-ref-insert-link)
    (define-key org-mode-map (kbd "C-c C-x )")
                #'org-ref-insert-ref-link)))
;; ----------------------------------------------------


;; ----------------------------------------------------
;; Org-Contrib
;; ----------------------------------------------------
;; Requires Non-GNU Elpa (https://elpa.nongnu.org/)
;;
;; Homepage: https://elpa.nongnu.org/nongnu/org-contrib.html
;; ----------------------------------------------------
(use-package org-contrib
  :ensure t
  :config
  ;; Seems abandoned??
  ;; (require 'ox-bibtex)

  ;; Ignore headlines with tag `:ignore:'
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
  )

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



;; ----------------------------------------------------
;; Result Navigation
;; ----------------------------------------------------
;; Navigate to the beginning or end of results of the
;; source code block at point.
;; ----------------------------------------------------

; Uses condition-case
; --------------------------
; (condition-case nil
;     (delete-file filename)
;   ((debug error) nil))
;
; Refer [[info:elisp#Handling Errors]]
; $ info elisp "Handling Errors"
(defun bvr/result-beginning ()
  "Beginning of results of src-block or nil.

Nil if point is elsewhere."
  (condition-case nil
      (or (org-babel-where-is-src-block-result)
        (error "No source block"))
    (error (progn
             (message "Can't find beginning of results")
             nil))))

(defun bvr/result-end ()
  "End of results of src-block or nil.

Nil if point is elsewhere."
  (let* ((beg (bvr/result-beginning)))
    (if beg
        (save-excursion
          (goto-char beg)
          (org-babel-result-end))
      (progn
        (message "Can't find beginning of results")
        nil))))

(defun bvr/goto-result-beginning ()
  "Go to the beginning of results of a src block."
  (interactive)
  (let* ((c (bvr/result-beginning)))
    (and c (goto-char c))))

(defun bvr/goto-result-end ()
  "Go to the end of results of a src block."
  (interactive)
  (let* ((c (bvr/result-end)))
    (and c (goto-char c))))
;; ----------------------------------------------------


;; ----------------------------------------------------
;; Org Babel
;;
;; Ansi color results after org-babel-execute-src-block
;;
;; Use org-babel-after-execute-hook
;; (https://orgmode.org/worg/doc.html)
;;
;; As shown in this issue and comment
;; https://github.com/emacs-jupyter/jupyter/issues/366#issuecomment-985758277
;; titled "Error handling prints ANSI color sequences
;; in plaintext."
;;
;; Requires ansi-color
;; ----------------------------------------------------
(require 'ansi-color)

(defun bvr/ansi-color-result ()
  "Ansi color results of src-block."
  (interactive)
  (let* ((beg (or (bvr/result-beginning) (point-min)))
         (end (or (bvr/result-end) (point-max))))
    (ansi-color-apply-on-region beg end)
    (message "Applied Ansi Color")))

(add-hook 'org-babel-after-execute-hook
          #'bvr/ansi-color-result)
;; ----------------------------------------------------

;; ----------------------------------------------------
;; Org Babel Tangle Block
;;
;; Tangle the block at point.
;;
;; Under the hood call `org-babel-tangle' with a prefix
;; argument.  Adapted from here:
;; https://stackoverflow.com/a/39628921
;; ----------------------------------------------------
(defun bvr/org-babel-tangle-block ()
  (interactive)
  (let ((current-prefix-arg '(4)))
     (call-interactively 'org-babel-tangle)))
;; ----------------------------------------------------

(provide 'org-setup)

;;; org-setup.el ends here
