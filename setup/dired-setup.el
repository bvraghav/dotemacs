;;; dired-setup.el --- Setup Dired Enhancements -*- lexical-binding: t -*-

;; Author: B.V. Raghav
;; Maintainer: B.V. Raghav
;; Version: 0.1
;; Package-Requires: ()
;; Homepage: https://github.com/bvraghav/dotemacs.git
;; Keywords: dired,dotemacs


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

;; Setup Dired Enhancements

;;; Code:


(use-package dired-x
  :after dired-aux

  :bind (:map dired-mode-map
              ("C-c C-n" . dired-narrow)
              ("C-c ."   . bvr-dired-kill-dot-files)
              ("C-c C-d" . bvr-dired-desktop-revert)
              ("C-c C-t" . bvr-dired-open-tmux-here))

  :init
  (defun bvr-dired-kill-dotfiles ()
    (interactive)
    (dired-mark-files-regexp "\\..*")
    (dired-do-kill-lines))


  (defun bvr-dired-desktop-revert ()
    (interactive)
    (let* ((filename (dired-get-file-for-visit))
	   (desktop-dirname (file-name-directory filename))
	   (desktop-base-file-name (file-name-nondirectory filename)))
      (desktop-revert)))

  (defun bvr-dired-open-tmux-here()
    (interactive)
    (let ((dirname (or (when (equal major-mode 'dired-mode)
                         (bvr-dired-dwim-directory))
                       default-directory)))
      (async-shell-command
       (format "xterm -e 'tmux new-session -c %s'" dirname))))

  (defun bvr-dired-dwim-directory()
    (let ((sel (dired-get-file-for-visit)))
      (if (file-directory-p sel)
          sel
        (file-name-directory sel))))

  :config

  ;; Dired Mode archive handler
  (add-to-list 'dired-compress-file-suffixes
               '("\\.zip\\'" ".zip" "unzip")))

(with-eval-after-load 'dired
  (setq  dired-isearch-filenames t
         dired-listing-switches "-alh"))

(use-package dired-quick-sort
  :ensure t
  :config
  (dired-quick-sort-setup))



(provide 'dired-setup)

;;; dired-setup.el ends here
