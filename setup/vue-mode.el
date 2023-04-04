;;; vue-mode.el --- Vue Mode -*- lexical-binding: t -*-

;; Author: B.V. Raghav
;; Maintainer: B.V. Raghav
;; Version: version
;; Package-Requires: (sgml-mode edit-indirect)
;; Homepage: https://github.com/bvraghav/dotemacs.git
;; Keywords: vue-mode


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

;; Vue Mode Definiiton

;;; Code:


(require 'sgml-mode)
(require 'edit-indirect)

(defvar vue-constants nil)

(defvar vue-keywords
  '("template" "script" "style"))

;; I'd probably put in a default that you want, as opposed to nil
(defvar vue-tab-width 2 "Width of a tab for Vue mode.")

;; Two small edits.
;; First is to put an extra set of parens () around the list
;; which is the format that font-lock-defaults wants
;; Second, you used ' (quote) at the outermost level where you wanted ` (backquote)
;; you were very close
(defvar vue-font-lock-defaults
  `((
     ;; stuff between double quotes
     ("\"\\.\\*\\?" . font-lock-string-face)
     ;; ; : , ; { } =>  @ $ = are all special elements
     (":\\|,\\|;\\|{\\|}\\|=>\\|@\\|$\\|=" . font-lock-keyword-face)
     ( ,(regexp-opt vue-keywords 'words) . font-lock-builtin-face)
     ( ,(regexp-opt vue-constants 'words) . font-lock-constant-face)
     )))

(defvar vue-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map html-mode-map)
    map))

(define-derived-mode vue-mode html-mode "V"
  "VUE mode is a major mode for editing Vue SFC files."
  ;; you again used quote when you had '((vue-hilite))
  ;; I just updated the variable to have the proper nesting (as noted above)
  ;; and use the value directly here
  (setq font-lock-defaults vue-font-lock-defaults)
  
  ;; when there's an override, use it
  ;; otherwise it gets the default value
  (when vue-tab-width
    (setq tab-width vue-tab-width))
  
  ;; for comments
  ;; ;; overriding these vars gets you what (I think) you want
  ;; ;; they're made buffer local when you set them
  ;; (setq comment-start "#")
  ;; (setq comment-end "")
  
  (modify-syntax-entry ?# "< b" vue-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" vue-mode-syntax-table)
  
  ;; Note that there's no need to manually call `vue-mode-hook'; `define-derived-mode'
  ;; will define `vue-mode' to call it properly right before it exits
  )

;; Auto Mode Alist for vue mode.
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

;; Vue Functions
;; ------------------------------------------------------------------------
(defun vue-part-end (part-name)
  "Find the end of the part defined by PART-NAME."
  (save-excursion
    (goto-char (point-max))
    (search-backward
     (format "</%s>" part-name))
    (backward-char)
    (point)))

(defun vue-part-beg (part-name)
  "Find the beginning of the part defined by PART-NAME."
  (save-excursion
    (goto-char (point-min))
    (search-forward
     (format "<%s" part-name))
    (search-forward ">")
    (forward-char)
    (point)))

(defun vue-part-buffer (part-name)
  "Compute edit-indirect buffer name.
Using PART-NAME and (current-buffer)."
  (format "*VUE %s: %s*"
	  (capitalize part-name)
	  (buffer-name (current-buffer))))

(defun vue-edit-part (part-name)
  "Edit PART-NAME in an indirect-buffer.
Mark PART-NAME, Edit it in indirect buffer, and delete other
windows."
  (let ((current-point (point))
	(part-beg (vue-part-beg part-name))
	(part-end (vue-part-end part-name))
	(part-buf (vue-part-buffer part-name)))
    (edit-indirect-region part-beg part-end 't)
    (rename-buffer part-buf)

    ;; TODO Try to restore point without this dirty hack
    (goto-char (- (1+ current-point) part-beg)))
  (call-interactively 'delete-other-windows))

(defun vue-edit-template ()
  "Edit template part."
  (interactive)
  (vue-edit-part "template")
  (html-mode))

(defun vue-edit-script ()
  "Edit script part."
  (interactive)
  (vue-edit-part "script")
  (js2-mode))

(defun vue-edit-css ()
  "Edit style part."
  (interactive)
  (vue-edit-part "style")
  (css-mode))

;; ;; Does not work as desired.. Shunned for now
;; (defun vue-kill-after-commit ()
;;   (make-local-variable 'edit-indirect-after-commit-functions)
;;   (add-to-list 'edit-indirect-after-commit-functions
;; 	       #'delete-windows-on))
;; (add-hook 'vue-mode-hook #'vue-kill-after-commit)

;; Key Bindings
(define-prefix-command 'vue-edit-map)
(define-key vue-mode-map (kbd "C-c C-e") 'vue-edit-map)
(define-key vue-edit-map (kbd "C-t") #'vue-edit-template)
(define-key vue-edit-map (kbd "t") #'vue-edit-template)
(define-key vue-edit-map (kbd "C-s") #'vue-edit-script)
(define-key vue-edit-map (kbd "s") #'vue-edit-script)
(define-key vue-edit-map (kbd "C-c") #'vue-edit-css)
(define-key vue-edit-map (kbd "c") #'vue-edit-css)


(provide 'vue-mode)

;;; vue-mode.el ends here
