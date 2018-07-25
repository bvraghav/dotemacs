;; Dired

;; Functions
;; -----------------------------------

(defun bvr-dired-kill-dotfiles ()
  (interactive)
  (dired-mark-files-regexp "\\..*")
  (dired-do-kill-lines))

;; -----------------------------------

;; Dired Mode archive handler
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes 
		'("\\.zip\\'" ".zip" "unzip")))

;; Dired Mode Map

(define-key dired-mode-map (kbd "C-c C-n") 'dired-narrow)
(define-key dired-mode-map (kbd "C-c .") #'bvr-dired-kill-dotfiles)

(provide 'dired-setup)
