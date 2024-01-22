;;; markdown-setup.el --- Setup for markdown-mode. -*- lexical-binding: t -*-

;; Author: B.V. Raghav
;; Maintainer: B.V. Raghav
;; Version: 1.0.0
;; Package-Requires: (markdown-mode)
;; Homepage: https://github.com/bvraghav/dotemacs
;; Keywords: dotemacs,markdown


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

;; Setup for markdown mode.

;;; Code:


;; ----------------------------------------------------
;; Use MarkServ
;;
;; depends: (s)
;; Install `markserv' using `npm i -g markserv'.
;;
;; Unless this issue:
;; https://github.com/markserv/markserv/issues/132 is
;; resolved,
;;
;; (UPDATE: The following doesn't help)
;; Manually insert a script tag into Mathjax config
;; here (replace _VERSION_ appropriately):
;; ~/.config/nvm/versions/node/_VERSION_/lib/node_modules/markserv/lib/templates/markdown.html#L14
;; ----------------------------------------------------
(use-package s :ensure t)

(defun bvr/ms/dirname ()
  "Get parent directory of the current buffer."
  (interactive)
  (directory-file-name
   (file-name-parent-directory
    (buffer-file-name))))

(defun bvr/ms/get-prog ()
  "Return the program to invoke based on buffer-filename.

If buffer-filename has a README.md at the end, \"readme\" else
\"markserv\"."
  (interactive)
  (if (s-ends-with? "README.md" (buffer-file-name) t)
      "readme"
    "markserv"))

(defun bvr/ms/proc-name ()
  "Get process name for `start-process'."
  (format "%s::%s"
          (bvr/ms/get-prog)
          (bvr/ms/dirname)))

(defun bvr/ms/start ()
  "Start the markserv process."
  (interactive)
  (or
   (and (get-process (bvr/ms/proc-name))
        (message (format "Process already running: %s"
                         (bvr/ms/proc-name))))
   (with-existing-directory (bvr/ms/dirname)
     (start-process (bvr/ms/proc-name)
                    (format "*%s*" (bvr/ms/proc-name))
                    (bvr/ms/get-prog)))))

(defun bvr/ms/end ()
  "End the markserv-process."
  (interactive)
  (delete-process (bvr/ms/proc-name)))

(defun bvr/ms/validate-markserv ()
  "Warn if markserv is not installed."
  (interactive)
  (and (< 0
          (call-process-shell-command
           "markserv --help"
           nil nil nil))
       (warn "%s\n%s"
             "Install markserv using:"
             "npm install -g markserv")))

;; ----------------------------------------------------

;; Markdown
;; ----------------------------------------------------
(use-package markdown-mode
  :ensure t
  :init (bvr/ms/validate-markserv)
  :hook (markdown-mode . auto-fill-mode)
  :bind (("C-c C-q C-l" . #'bvr/ms/start)
         ("C-c C-q C-q C-l" . #'bvr/ms/end)))

(provide 'markdown-setup)

;;; markdown-setup.el ends here
