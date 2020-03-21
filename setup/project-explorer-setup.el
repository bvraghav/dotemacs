(defun set-pe-keybindings ()
  ;; Instead of speedbar, use project explorer
  (global-set-key (kbd "C-z C-s") #'project-explorer-open)
  (global-set-key (kbd "C-z s") #'project-explorer-open))

(defun disable-linum ()
  (linum-mode -1))

(when (require 'project-explorer nil 'noerror)
  (set-pe-keybindings)
  (add-hook 'project-explorer-mode-hook
	    #'disable-linum))

(provide 'project-explorer-setup)
