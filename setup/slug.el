(defun get-slug (string)
  "Convert STRING to its slug."
  (shell-command-to-string (concat "slug '" string "'")))

(defun slug (string)
  (interactive "sString for slug: ")
  (insert (get-slug string)))

(defun slug-dwim ()
  (interactive)
  (if (minibufferp)
      (slug (car kill-ring))
    (slug (completing-read "String for slug: " kill-ring nil t (car kill-ring)))))

(defun slug-random-string (N alphabet)
  (let (str)
    (dotimes (i N str)
      (setq str (concat str (string (elt alphabet (random (length alphabet)))))))))

(defun slug-insert-random-string (N)
  (interactive "P")
      (let ((alphabet (concat "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                              "abcdefghijklmnopqrstuvwxyz"
                              "0123456789"
                              "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                              "abcdefghijklmnopqrstuvwxyz"
                              "0123456789"
                              "!@#$%^-_=+{}[];:<>,.?")))
        (insert (slug-random-string (or N 8) alphabet))))

(defun slug-insert-random-string-from-alphabet (N alphabet)
  (interactive (list (string-to-number (read-string "N:" nil nil "8"))
                     (let ((alp (list (concat "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                              "abcdefghijklmnopqrstuvwxyz"
                                              "0123456789"
                                              "0123456789"
                                              "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                              "abcdefghijklmnopqrstuvwxyz"
                                              "0123456789"
                                              "0123456789"
                                              "!@#$%^-_=+{}[];:<>,.?")
                                      (concat "abcdefghijklmnopqrstuvwxyz"
                                              "0123456789"
                                              "0123456789"
                                              "--"))))
                       (read-string "alphabet: " nil nil alp))))
  (insert (slug-random-string N alphabet)))
  

(provide 'slug)
