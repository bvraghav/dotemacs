(require 'recentf)
;; Activate recentf mode
;; ====================================================
(recentf-mode t)

;; ====================================================
;; Periodically saving the list of files
;; ----------------------------------------------------
;; https://www.emacswiki.org/emacs/RecentFiles#h5o-1
;; ----------------------------------------------------

(run-at-time nil (* 5 60) 'recentf-save-list)

;; ====================================================




;; ====================================================
;; Updating recentf-list when renaming files in dired
;; ----------------------------------------------------
;; https://www.emacswiki.org/emacs/RecentFiles#h5o-22
;; ----------------------------------------------------

;; Magic advice to rename entries in recentf when
;; moving files in dired.
(defun rjs/recentf-rename-notify
    (oldname newname &rest args)
  (if (file-directory-p newname)
      (rjs/recentf-rename-directory oldname newname)
    (rjs/recentf-rename-file oldname newname)))

(defun rjs/recentf-rename-file (oldname newname)
  (setq recentf-list
        (mapcar (lambda (name)
                  (if (string-equal name oldname)
                      newname
                    name))
                recentf-list)))

(defun rjs/recentf-rename-directory (oldname newname)
  ;; oldname, newname and all entries of recentf-list
  ;; should already be absolute and normalised so I
  ;; think this can just test whether oldname is a
  ;; prefix of the element.
  (setq recentf-list
        (mapcar (lambda (name)
                  (if (string-prefix-p oldname name)
                      (concat
                       newname
                       (substring name
                                  (length oldname)))
                    name))
                recentf-list)))

(advice-add 'dired-rename-file
            :after #'rjs/recentf-rename-notify)

;; ====================================================


(provide 'recentf-setup)
