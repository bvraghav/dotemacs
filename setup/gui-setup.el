;; GUI Setup
(defun gui-setup ()
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(defun graphic-p (&optional frame)
  (if (< emacs-major-version 23)
      ;; On a highly unlikely event of an old version
      ;; of emacs (as on date, Jan 2021)
      (or window-system (getenv "DISPLAY"))
    ;; Else test with the official tool for the same.
    (display-graphic-p frame)))

(defun new-frame-setup (frame)
  (when (graphic-p frame)
    (gui-setup)
    (remove-hook 'after-make-frame-functions
                 #'new-frame-setup)))
(add-hook 'after-make-frame-functions
          #'new-frame-setup)

(provide 'gui-setup)
