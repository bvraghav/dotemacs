(defun install-selected-packages ()
  (interactive)
  (dolist (pkg package-selected-packages)
    (unless (package-installed-p pkg)
      (package-install pkg))))

(package-initialize)

(package-refresh-contents)

(install-selected-packages)

(provide 'package-setup)
