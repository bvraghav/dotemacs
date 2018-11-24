;; Dired

;; Functions
;; -----------------------------------

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

;; -----------------------------------

;; Dired Mode archive handler
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes 
		'("\\.zip\\'" ".zip" "unzip")))

;; Dired Mode Map

(define-key dired-mode-map (kbd "C-c C-n") 'dired-narrow)
(define-key dired-mode-map (kbd "C-c .") #'bvr-dired-kill-dotfiles)
(define-key dired-mode-map (kbd "C-c C-d") #'bvr-dired-desktop-revert)
(define-key dired-mode-map (kbd "C-c C-t") #'bvr-dired-open-tmux-here)
(define-key dired-mode-map (kbd "C-c C-t") #'bvr-dired-open-tmux-here)

(provide 'dired-setup)
