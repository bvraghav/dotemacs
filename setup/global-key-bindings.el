;; Unset Keys
(global-unset-key (kbd "C-z"))

;; Key Bindings
(global-set-key (kbd "C-c C-i") #'ibuffer)
(global-set-key (kbd "C-z C-a") #'align-regexp)
(global-set-key (kbd "C-z C-k") #'keep-lines)
;; (global-set-key (kbd "C-z C-f") #'flush-lines)
(global-set-key (kbd "C-z C-c") #'calc)
;; (global-set-key (kbd "C-z C-d") #'desktop-save)
(global-set-key (kbd "C-z d") #'desktop-save)

;; Functions
(global-set-key (kbd "C-z C-t") #'bvr-ts) ;; Defined later as auxilliary function
(global-set-key (kbd "C-z C-b") #'bvr-insert-buffer-name)
(global-set-key (kbd "C-z C-p") #'bvr-insert-path)
(global-set-key (kbd "C-z C-z C-u") #'bvr-insert-google-search-url)
(global-set-key (kbd "C-z C-z C-l") #'bvr-insert-google-lucky-url)
(global-set-key (kbd "C-z C-z C-d") #'bvr-insert-time)

;; C-Headers
(global-set-key (kbd "C-c C-f C-i") #'insert-c-header)

;; Rectangle Replace
(global-set-key (kbd "C-x r M-%") #'my-replace-string-rectangle)
(global-set-key (kbd "C-x r C-M-%") #'my-replace-regexp-rectangle)

;; Icicles
;; (global-set-key (kbd "C-z C-'") #'icy-mode)

;; (global-unset-key (kbd "C-M-j"))
;; (global-set-key (kbd "C-M-j") #'icicle-bookmark)

;; W3m
;; (global-unset-key (kbd "C-x m"))
;; (global-set-key (kbd "C-x m") #'browse-url-at-point)
;; (global-set-key (kbd "C-z C-;") #'w3m)

;; Speedbar
;; -----------------------------------
;; Unless already setup by other alternatives,
;; use the speedbar...
(defmacro f-if-exists (fname)
  `(when (fboundp #',fname)
     #',fname))
(defun get-speedbar ()
  (interactive)
  (funcall (or (f-if-exists project-explorer-toggle)
	       (f-if-exists speedbar-get-focus))))
(global-set-key (kbd "C-z C-s") #'get-speedbar)
(global-set-key (kbd "C-z s") #'get-speedbar)

;; Slug
(require 'slug)
(global-set-key (kbd "C-z C-l") #'slug)
(global-set-key (kbd "C-z l") #'slug-dwim)

;; Reverse characters in a region
(global-set-key (kbd "C-z C-r") #'bvr-reverse-region)

;; ;; Proxy
;; (global-set-key (kbd "C-z p") #'bvr-proxy-toggle)

;; Expand Region
(global-set-key (kbd "C-M-]") #'er/expand-region)

;; Man
(global-set-key (kbd "C-z C-m") #'man)
(global-set-key (kbd "C-z m") #'man)

;; Whitespace mode
;; view all whitespace characters in buffer.
(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-c C-w") 'whitespace-mode)

;; Line wrapping
(global-set-key (kbd "C-z $") 'toggle-truncate-lines)
(global-set-key (kbd "C-z C-$") 'toggle-truncate-lines)

;; Fill Line
;; ----------------------------------------------------
(require 'fill-line)
(global-set-key (kbd "M-#") #'fill-line)


(provide 'global-key-bindings)
