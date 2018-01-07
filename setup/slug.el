(defun slug (string)
  (interactive "sString for slug: ")
  (insert (shell-command-to-string (concat "slug '" string "'"))))

(defun slug-dwim ()
  (interactive)
  (if (minibufferp)
      (slug (car kill-ring))
    (slug (completing read "String for slug: " kill-ring nil t (car kill-ring)))))

(provide 'slug)
