;; Requires
(require 'w3m-search)
(require 'w3m-lnum)
(require 'w3m-session)

;; Auto load browser
;; -----------------------------------
;; Set through customize-variable
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "w3m Web Browser." t)

(w3m-fb-mode)

;; w3m-lnum
;; -----------------------------------
(add-hook 'w3m-mode-hook #'w3m-lnum-mode)

;; w3m-session
;;-----------------------------------
(defun w3m-session-key-maps ()
  (define-key w3m-mode-map (kbd "z s") #'w3m-session-save)
  (define-key w3m-mode-map (kbd "z l") #'w3m-session-select)
  (define-key w3m-mode-map (kbd "F") #'w3m-toggle-filtering)
  (define-key w3m-mode-map (kbd "f") #'w3m-lnum-follow))
(add-hook 'w3m-mode-hook #'w3m-session-key-maps)
;; Search Engine Setup
;; -----------------------------------
;; Analogous to (webjumps for conkeror)
;; (setq w3m-search-default-engine "google-groups")
;; (dolist (engine '(("acronym" "http://www.acronymfinder.com/af-query.asp?acronym=%s&string=exact")
;; 		  ("ebay" "http://search.ebay.com/search/search.dll?query=%s")
;; 		  ("google-groups" "http://www.google.com/groups?q=%s")
;; 		  ("syndic8" "http://www.syndic8.com/feedlist.php?ShowStatus=all&ShowMatch=%s")
;; 		  ("weather" "http://www.weather.com/search/search?where=%s&what=WeatherLocalUndeclared")
;; 		  ("wikipedia-en" "http://en.wikipedia.org/wiki/Special:Search?search=%s")
;; 		  ("worldclock" "http://www.timeanddate.com/worldclock/results.html?query=%s")))
;;     (add-to-list 'w3m-search-engine-alist engine))

;; Make the previous search engine the default for the next
;; search.

(defadvice w3m-search (after change-default activate)
  (let ((engine (nth 1 minibuffer-history)))
    (when (assoc engine w3m-search-engine-alist)
      (setq w3m-search-default-engine engine))))


;; Trailing ws
;; -----------------------------------
(add-hook 'w3m-display-hook
	  (lambda (url)
	    (let ((buffer-read-only nil))
	      (delete-trailing-whitespace))))

;; Forget Authinfo
;; -----------------------------------
;; Required if I land up in `la la land'
;; For more info refer (bookmark) `EmacsWiki: w3m authentication forget la la land'

(defun w3m-erase-authinfo-root (root)
  (setq w3m-process-authinfo-alist
        (assq-delete-all 
         nil (mapcar 
              (lambda (elem) (if (not (equal root (car elem))) elem)) 
              w3m-process-authinfo-alist))))

(defun w3m-forget-authinfo ()
  (interactive)
  (let* ((roots (mapcar
                 (lambda (elem) (list (car elem) (car elem)))
                 w3m-process-authinfo-alist))
         (root (completing-read "URL: " roots nil t)))
    (w3m-erase-authinfo-root root)))


;; Provide Package
;; -----------------------------------
(provide 'w3m-ext)
