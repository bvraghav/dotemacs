;; Unset Keys
(global-unset-key (kbd "C-z"))

;; Key Bindings
(global-set-key (kbd "C-c C-i") #'ibuffer)
(global-set-key (kbd "C-z C-a") #'align-regexp)
(global-set-key (kbd "C-z C-k") #'keep-lines)
(global-set-key (kbd "C-z C-f") #'flush-lines)
(global-set-key (kbd "C-z C-c") #'calc)
(global-set-key (kbd "C-z C-d") #'desktop-save)
(global-set-key (kbd "C-z d") #'desktop-save)

;; Functions
(global-set-key (kbd "C-z C-t") #'bvr-ts) ;; Defined later as auxilliary function
(global-set-key (kbd "C-z C-b") #'bvr-insert-buffer-name)
(global-set-key (kbd "C-z C-p") #'bvr-insert-path)
(global-set-key (kbd "C-z C-z C-u") #'bvr-insert-google-search-url)
(global-set-key (kbd "C-z C-z C-l") #'bvr-insert-google-lucky-url)
(global-set-key (kbd "C-z C-z C-d") #'bvr-insert-time)

;; Sift Regexp Shortcut key
(global-set-key (kbd "C-z C-x") #'sift-regexp)

;; C-Headers
(global-set-key (kbd "C-c C-f C-i") #'insert-c-header)

;; Rectangle Replace
(global-set-key (kbd "C-x r M-%") #'my-replace-string-rectangle)
(global-set-key (kbd "C-x r C-M-%") #'my-replace-regexp-rectangle)

;; Icicles
(global-set-key (kbd "C-z C-'") #'icy-mode)

(global-unset-key (kbd "C-M-j"))
(global-set-key (kbd "C-M-j") #'icicle-bookmark)

;; W3m
(global-unset-key (kbd "C-x m"))
(global-set-key (kbd "C-x m") #'browse-url-at-point)
(global-set-key (kbd "C-z C-;") #'w3m)

;; Speedbar
(global-set-key (kbd "C-z C-s") #'speedbar-get-focus)
(global-set-key (kbd "C-z s") #'speedbar-get-focus)

;; Slug
(global-set-key (kbd "C-z C-l") #'slug)
(global-set-key (kbd "C-z l") #'slug-dwim)

;; Reverse characters in a region
(global-set-key (kbd "C-z C-r") #'bvr-reverse-region)

;; Magit
(global-set-key (kbd "C-x g") #'magit-status)

;; Proxy
(global-set-key (kbd "C-z p") #'bvr-proxy-toggle)

(provide 'global-key-bindings)
