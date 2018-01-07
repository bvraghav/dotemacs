
;; C-mode indentation
(require 'cc-mode)
(setq c-default-style "linux"
      c-basic-offset 2)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

;; c-mode-common
(defun bvr-c-mode-common ()
  "Customizations to the cc-mode"
  (yas-minor-mode t)
  (set (make-local-variable 'compile-command)
       "b2 -d 2 -j 12 -s PREFIX=$HOME/.local")
  ;; Enable ggtags-mode
  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'cc-mode)
    (ggtags-mode 1))
  (setq-local c-cleanup-list '(brace-else-brace
			       brace-elseif-brace
			       brace-catch-brace
			       defun-close-semi
			       comment-close-slash)))
(add-hook 'c-mode-common-hook 'bvr-c-mode-common)

;; Compile command for throw away c++ files
(defun bvr-compile-and-run(&optional cmd)
  (let ((cmd (or cmd "g++"))
	(fn (file-relative-name (buffer-file-name)))
	(fns (file-relative-name
	      (file-name-sans-extension (buffer-file-name)))))
   (message "%s -o %s %s && %s" cmd fns fn fns)))

;; c-mode compile command dwim
(defun bvr-c-compile()
  (interactive)
  (cond
   ((member 'compile-command
	    (mapcar #'car file-local-variables-alist))
    (call-interactively #'compile))
   ((projectile-project-p)
    (call-interactively #'projectile-compile-project))
   (t (call-interactively #'compile))))
(add-hook 'c-mode-common-hook
	  (lambda()
	    (local-set-key (kbd "C-M-x") #'bvr-c-compile)))

;; Doxymacs Mode auto load
;; (require 'doxymacs)
;; (add-hook 'c-mode-hook (lambda() (doxymacs-mode t)))
;; (add-hook 'c++-mode-hook (lambda() (doxymacs-mode t)))

;; C++ Mode implementation files
(add-to-list 'auto-mode-alist '("\\.him\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.\(hpp[_.]\)?impl\\'" . c++-mode))

;; Jam-mode auto mode alist
(add-to-list 'auto-mode-alist '("^[Jj]am\(root|file|base\)\\'" . jam-mode))
(add-to-list 'auto-mode-alist '("\\.jam\\'" . jam-mode))

(provide 'c-setup)
