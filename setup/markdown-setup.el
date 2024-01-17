(use-package markdown-mode
  :ensure t
  :hook (markdown-mode . auto-fill-mode)
  :bind (("C-c C-q C-l" . #'bvr/ms/start)


;; ----------------------------------------------------
;; Use MarkServ
;;
;; depeneds: (s)
;; ----------------------------------------------------
(use-package s :ensure t)

(defun bvr/ms/dirname ()
  "Get parent directory of the current buffer."
  (interactive)
  (directory-file-name
   (file-name-parent-directory
    (buffer-file-name))))

(defun bvr/ms/get-prog ()
  "Return the program to invoke based on buffer-filename.

If buffer-filename has a README.md at the end, \"readme\" else
\"markserv\"."
  (interactive)
  (if (s-ends-with? "README.md" (buffer-file-name) t)
      "readme"
    "markserv"))

(defun bvr/ms/proc-name ()
  "Get program-name for start-process."
  (format "%s::%s"
          (bvr/ms/get-prog)
          (bvr/ms/dirname)))

(defun bvr/ms/start ()
  "Start the markserv-process."
  (interactive)
  (or
   (and (get-process (bvr/ms/proc-name))
        (message (format "Process already running: %s"
                         (bvr/ms/proc-name))))
   (with-existing-directory (bvr/ms/dirname)
     (start-process (bvr/ms/proc-name)
                    (format "*%s*" (bvr/ms/proc-name))
                    (bvr/ms/get-prog)))))

(defun bvr/ms/end ()
  "End the markserv-process."
  (interactive)
  (delete-process (bvr/ms/proc-name)))

(provide 'markdown-setup)
