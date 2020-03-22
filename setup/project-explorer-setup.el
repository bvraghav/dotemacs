(defun disable-linum ()
  (linum-mode -1))

(when (require 'project-explorer nil 'noerror)
  (add-hook 'project-explorer-mode-hook
	    #'disable-linum))

(provide 'project-explorer-setup)
