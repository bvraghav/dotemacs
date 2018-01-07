(defun my/autoinsert-yas-expand()
      "Replace text in yasnippet template."
      (yas/expand-snippet (buffer-string) (point-min) (point-max)))


(setq auto-insert 'other
      auto-insert-directory "~/.emacs.d/auto-inserts/"
      auto-insert-query nil
      auto-insert-alist '((("_main\\.cpp\\'" . "C++ Main Source")
			   . ["_main.cpp" c++-mode my/autoinsert-yas-expand])
			  ))

(auto-insert-mode)

(message "Auto Insert Setup Loaded Successfully")

(provide 'auto-insert-setup)
