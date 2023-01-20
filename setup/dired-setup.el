;; Dired

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

(provide 'dired-setup)
