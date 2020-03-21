(defun set-pe-keybindings ()
  ;; Instead of speedbar, use project explorer
  (global-set-key (kbd "C-z C-s") #'project-explorer-open)
  (global-set-key (kbd "C-z s") #'project-explorer-open))

(when (require 'project-explorer nil 'noerror)
    (set-pe-keybindings))

(provide 'project-explorer-setup)
