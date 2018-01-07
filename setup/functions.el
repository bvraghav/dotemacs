;; Insert buffer name
(defun bvr-insert-buffer-name ()
  "Simple command to insert buffer name"
  (interactive)
  (insert (buffer-name)))

;; Insert File name
(defun bvr-insert-path(arg path)
  (interactive "P\nGPath: ")
  (if (eq 4 (prefix-numeric-value arg))
      (insert (expand-file-name path))
    (insert (file-relative-name path default-directory))))

;; Insert Google Search Link
(defun insert-url-base
    (search-string url-prefix)
  (insert (format "%s%s"
		  url-prefix
		  (replace-regexp-in-string " " "+"
					    search-string))))
(defun bvr-insert-google-search-url(search-string)
  (interactive "sSearch String: ")
  (insert-url-base search-string "https://google.com/search?q="))

(defun bvr-insert-google-lucky-url(search-string)
  (interactive "sSearch String: ")
  (insert-url-base search-string "https://google.com/search?btnI=1&q="))

;; Insert Date Time
(defun bvr-insert-time(time-str)
  (interactive "sNow: ")
  (let ((time-format "%Y-%m-%d %H%M"))
    (insert
     (if (> 0 (length time-str))
	 (format-time-string time-format time-str)
       (format-time-string time-format)))))

(defun bvr-ts ()
  (interactive)
  (insert (format-time-string "%Y%m%d-%H%M")))

(provide 'functions)
