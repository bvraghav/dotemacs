;; Dired

;; Dired Mode archive handler
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes 
		'("\\.zip\\'" ".zip" "unzip")))

;; Dired Mode Map

(define-key dired-mode-map (kbd "C-c C-n") 'dired-narrow)

(provide 'dired-setup)
