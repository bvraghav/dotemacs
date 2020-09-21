;; Icicles Mode
(require 'icicles)

(defun bvr-icicle-mode-keymaps ()
  ;; "C-c '" conflicts with org mode edit special
  (define-key icicle-mode-map (kbd "C-c '") nil)
  (define-key icicle-mode-map (kbd "C-z C-o") #'icicle-occur)
  (define-key icicle-mode-map (kbd "C-z o") #'icicle-occur))
;; (add-hook 'icicle-mode-hook #'bvr-icicle-mode-keymaps)
;; (setq icicle-max-candidates 127)
;; (setq icicle-ido-like-mode 127)

;; Icicle settings
(custom-set-variables
 '(icicle-Completions-text-scale-decrease 0.0)
 '(icicle-top-level-key-bindings
   '(([pause]
      icicle-switch-to/from-minibuffer t)
     ("" icicle-search-generic t)
     ("$" icicle-search-word t)
     ("^" icicle-search-keywords t)
     ("o" icicle-occur t)
     ("=" icicle-imenu t)
     ("\"" icicle-search-text-property t)
     ("/" icicle-complete-thesaurus-entry
      (fboundp 'icicle-complete-thesaurus-entry))
     ([24 134217829]
      icicle-execute-named-keyboard-macro t)
     ([27 134217752]
      icicle-command-abbrev t)
     ("5o" icicle-select-frame t)
     ("" icicle-describe-option-of-type t)
     ([S-f4]
      icicle-kmacro t)
     (abort-recursive-edit icicle-abort-recursive-edit t)
     (apropos icicle-apropos t)
     (apropos-command icicle-apropos-command t)
     (apropos-value icicle-apropos-value t)
     (apropos-user-option icicle-apropos-option
                          (and
                           (fboundp 'icicle-apropos-option)
                           (fboundp 'apropos-user-option)))
     (apropos-variable icicle-apropos-option
                       (and
                        (fboundp 'icicle-apropos-option)
                        (not
                         (fboundp 'apropos-user-option))))
     (apropos-variable icicle-apropos-variable
                       (not
                        (fboundp 'icicle-apropos-option)))
     (apropos-zippy icicle-apropos-zippy t)
     (bookmark-jump icicle-bookmark t)
     (bookmark-jump-other-window icicle-bookmark-other-window t)
     (bmkp-bookmark-set-confirm-overwrite icicle-bookmark-cmd
                                          (fboundp 'bmkp-bookmark-set-confirm-overwrite))
     (bookmark-set icicle-bookmark-cmd t)
     (customize-apropos icicle-customize-apropos t)
     (customize-apropos-faces icicle-customize-apropos-faces t)
     (customize-apropos-groups icicle-customize-apropos-groups t)
     (customize-apropos-options icicle-customize-apropos-options t)
     (customize-face icicle-customize-face t)
     (customize-face-other-window icicle-customize-face-other-window t)
     (dabbrev-completion icicle-dabbrev-completion t)
     ([201326639]
      icicle-dispatch-C-M-/ t)
     (delete-window icicle-delete-window t)
     (delete-windows-for icicle-delete-window t)
     (describe-package icicle-describe-package
                       (fboundp 'describe-package))
     (dired icicle-dired
            (not
             (featurep 'dired+)))
     (dired-other-window icicle-dired-other-window
                         (not
                          (featurep 'dired+)))
     (exchange-point-and-mark icicle-exchange-point-and-mark t)
     (execute-extended-command icicle-execute-extended-command t)
     (find-file icicle-file t)
     (find-file-other-window icicle-file-other-window t)
     (find-file-read-only icicle-find-file-read-only t)
     (find-file-read-only-other-window icicle-find-file-read-only-other-window t)
     (insert-buffer icicle-insert-buffer t)
     (kill-buffer icicle-kill-buffer t)
     (kill-buffer-and-its-windows icicle-kill-buffer t)
     (load-library icicle-load-library
                   (> emacs-major-version 20))
     (minibuffer-keyboard-quit icicle-abort-recursive-edit
                               (fboundp 'minibuffer-keyboard-quit))
     (other-window icicle-other-window-or-frame t)
     (other-window-or-frame icicle-other-window-or-frame t)
     (pop-global-mark icicle-goto-global-marker-or-pop-global-mark t)
     (repeat-complex-command icicle-repeat-complex-command t)
     (set-mark-command icicle-goto-marker-or-set-mark-command t)
     (switch-to-buffer icicle-buffer t)
     (switch-to-buffer-other-window icicle-buffer-other-window t)
     (where-is icicle-where-is t)
     (yank icicle-yank-maybe-completing t)
     (yank-pop icicle-yank-pop-commands
               (featurep 'second-sel))
     (yank-pop-commands icicle-yank-pop-commands
                        (featurep 'second-sel))
     (zap-to-char icicle-zap-to-char
                  (fboundp 'read-char-by-name))
     (bmkp-autofile-set icicle-bookmark-a-file
                        (fboundp 'bmkp-bookmark-a-file))
     (bmkp-tag-a-file icicle-tag-a-file
                      (fboundp 'bmkp-tag-a-file))
     (bmkp-untag-a-file icicle-untag-a-file
                        (fboundp 'bmkp-untag-a-file))
     (bmkp-find-file icicle-find-file-handle-bookmark
                     (fboundp 'bmkp-find-file))
     (bmkp-find-file-other-window icicle-find-file-handle-bookmark-other-window
                                  (fboundp 'bmkp-find-file-other-window))
     (bmkp-autofile-jump icicle-bookmark-autofile
                         (fboundp 'bmkp-autofile-jump))
     (bmkp-autofile-jump-other-window icicle-bookmark-autofile-other-window
                                      (fboundp 'bmkp-autofile-jump))
     (bmkp-autonamed-jump icicle-bookmark-autonamed
                          (fboundp 'bmkp-autonamed-jump))
     (bmkp-autonamed-jump-other-window icicle-bookmark-autonamed-other-window
                                       (fboundp 'bmkp-autonamed-jump))
     (bmkp-autonamed-this-buffer-jump icicle-bookmark-autonamed-this-buffer
                                      (fboundp 'bmkp-autonamed-this-buffer-jump))
     (bmkp-bookmark-file-jump icicle-bookmark-bookmark-file
                              (fboundp 'bmkp-bookmark-file-jump))
     (bmkp-bookmark-list-jump icicle-bookmark-bookmark-list
                              (fboundp 'bmkp-bookmark-list-jump))
     (bmkp-desktop-jump icicle-bookmark-desktop
                        (fboundp 'bmkp-desktop-jump))
     (bmkp-dired-jump icicle-bookmark-dired
                      (fboundp 'bmkp-dired-jump))
     (bmkp-dired-jump-other-window icicle-bookmark-dired-other-window
                                   (fboundp 'bmkp-dired-jump))
     (bmkp-file-jump icicle-bookmark-file
                     (fboundp 'bmkp-file-jump))
     (bmkp-file-jump-other-window icicle-bookmark-file-other-window
                                  (fboundp 'bmkp-file-jump))
     (bmkp-file-this-dir-jump icicle-bookmark-file-this-dir
                              (fboundp 'bmkp-file-this-dir-jump))
     (bmkp-file-this-dir-jump-other-window icicle-bookmark-file-this-dir-other-window
                                           (fboundp 'bmkp-file-this-dir-jump))
     (bmkp-gnus-jump icicle-bookmark-gnus
                     (fboundp 'bmkp-gnus-jump))
     (bmkp-gnus-jump-other-window icicle-bookmark-gnus-other-window
                                  (fboundp 'bmkp-gnus-jump))
     (bmkp-image-jump icicle-bookmark-image
                      (fboundp 'bmkp-image-jump))
     (bmkp-image-jump-other-window icicle-bookmark-image-other-window
                                   (fboundp 'bmkp-image-jump))
     (bmkp-info-jump icicle-bookmark-info
                     (fboundp 'bmkp-info-jump))
     (bmkp-info-jump-other-window icicle-bookmark-info-other-window
                                  (fboundp 'bmkp-info-jump))
     (bmkp-local-file-jump icicle-bookmark-local-file
                           (fboundp 'bmkp-local-file-jump))
     (bmkp-local-file-jump-other-window icicle-bookmark-local-file-other-window
                                        (fboundp 'bmkp-local-file-jump))
     (bmkp-man-jump icicle-bookmark-man
                    (fboundp 'bmkp-man-jump))
     (bmkp-man-jump-other-window icicle-bookmark-man-other-window
                                 (fboundp 'bmkp-man-jump))
     (bmkp-non-file-jump icicle-bookmark-non-file
                         (fboundp 'bmkp-non-file-jump))
     (bmkp-non-file-jump-other-window icicle-bookmark-non-file-other-window
                                      (fboundp 'bmkp-non-file-jump))
     (bmkp-region-jump icicle-bookmark-region
                       (fboundp 'bmkp-region-jump))
     (bmkp-region-jump-other-window icicle-bookmark-region-other-window
                                    (fboundp 'bmkp-region-jump))
     (bmkp-remote-file-jump icicle-bookmark-remote-file
                            (fboundp 'bmkp-remote-file-jump))
     (bmkp-remote-file-jump-other-window icicle-bookmark-remote-file-other-window
                                         (fboundp 'bmkp-remote-file-jump))
     (bmkp-specific-buffers-jump icicle-bookmark-specific-buffers
                                 (fboundp 'bmkp-specific-buffers-jump))
     (bmkp-specific-buffers-jump-other-window icicle-bookmark-specific-buffers-other-window
                                              (fboundp 'bmkp-specific-buffers-jump))
     (bmkp-specific-files-jump icicle-bookmark-specific-files
                               (fboundp 'bmkp-specific-files-jump))
     (bmkp-specific-files-jump-other-window icicle-bookmark-specific-files-other-window
                                            (fboundp 'bmkp-specific-files-jump))
     (bmkp-temporary-jump icicle-bookmark-temporary
                          (fboundp 'bmkp-temporary-jump))
     (bmkp-temporary-jump-other-window icicle-bookmark-temporary-other-window
                                       (fboundp 'bmkp-temporary-jump))
     (bmkp-this-buffer-jump icicle-bookmark-this-buffer
                            (fboundp 'bmkp-this-buffer-jump))
     (bmkp-this-buffer-jump-other-window icicle-bookmark-this-buffer-other-window
                                         (fboundp 'bmkp-this-buffer-jump))
     (bmkp-url-jump icicle-bookmark-url
                    (fboundp 'bmkp-url-jump))
     (bmkp-url-jump-other-window icicle-bookmark-url-other-window
                                 (fboundp 'bmkp-url-jump))
     (bmkp-w3m-jump icicle-bookmark-w3m
                    (fboundp 'bmkp-w3m-jump))
     (bmkp-w3m-jump-other-window icicle-bookmark-w3m-other-window
                                 (fboundp 'bmkp-w3m-jump))
     ("jtj" icicle-bookmark-tagged
      (featurep 'bookmark+))
     ("4jtj" icicle-bookmark-tagged-other-window
      (featurep 'bookmark+))
     ("jt" icicle-find-file-tagged
      (featurep 'bookmark+))
     ("4jt" icicle-find-file-tagged-other-window
      (featurep 'bookmark+))
     (bmkp-find-file-all-tags icicle-find-file-all-tags
                              (fboundp 'bmkp-find-file-all-tags))
     (bmkp-find-file-all-tags-other-window icicle-find-file-all-tags-other-window
                                           (fboundp 'bmkp-find-file-all-tags))
     (bmkp-find-file-all-tags-regexp icicle-find-file-all-tags-regexp
                                     (fboundp 'bmkp-find-file-all-tags-regexp))
     (bmkp-find-file-all-tags-regexp-other-window icicle-find-file-all-tags-regexp-other-window
                                                  (fboundp 'bmkp-find-file-all-tags-regexp-other-window))
     (bmkp-find-file-some-tags icicle-find-file-some-tags
                               (fboundp 'bmkp-find-file-some-tags))
     (bmkp-find-file-some-tags-other-window icicle-find-file-some-tags-other-window
                                            (fboundp 'bmkp-find-file-some-tags-other-window))
     (bmkp-find-file-some-tags-regexp icicle-find-file-some-tags-regexp
                                      (fboundp 'bmkp-find-file-some-tags-regexp))
     (bmkp-find-file-some-tags-regexp-other-window icicle-find-file-some-tags-regexp-other-window
                                                   (fboundp 'bmkp-find-file-some-tags-regexp-other-window))
     (bmkp-autofile-all-tags-jump icicle-bookmark-autofile-all-tags
                                  (fboundp 'bmkp-autofile-all-tags-jump))
     (bmkp-autofile-all-tags-jump-other-window icicle-bookmark-autofile-all-tags-other-window
                                               (fboundp 'bmkp-autofile-all-tags-jump))
     (bmkp-autofile-all-tags-regexp-jump icicle-bookmark-autofile-all-tags-regexp
                                         (fboundp 'bmkp-autofile-all-tags-regexp-jump))
     (bmkp-autofile-all-tags-regexp-jump-other-window icicle-bookmark-autofile-all-tags-regexp-other-window
                                                      (fboundp 'bmkp-autofile-all-tags-regexp-jump))
     (bmkp-autofile-some-tags-jump icicle-bookmark-autofile-some-tags
                                   (fboundp 'bmkp-autofile-some-tags-jump))
     (bmkp-autofile-some-tags-jump-other-window icicle-bookmark-autofile-some-tags-other-window
                                                (fboundp 'bmkp-autofile-some-tags-jump))
     (bmkp-autofile-some-tags-regexp-jump icicle-bookmark-autofile-some-tags-regexp
                                          (fboundp 'bmkp-autofile-some-tags-regexp-jump))
     (bmkp-autofile-some-tags-regexp-jump-other-window icicle-bookmark-autofile-some-tags-regexp-other-window
                                                       (fboundp 'bmkp-autofile-some-tags-regexp-jump))
     (bmkp-all-tags-jump icicle-bookmark-all-tags
                         (fboundp 'bmkp-all-tags-jump))
     (bmkp-all-tags-jump-other-window icicle-bookmark-all-tags-other-window
                                      (fboundp 'bmkp-all-tags-jump))
     (bmkp-all-tags-regexp-jump icicle-bookmark-all-tags-regexp
                                (fboundp 'bmkp-all-tags-regexp-jump))
     (bmkp-all-tags-regexp-jump-other-window icicle-bookmark-all-tags-regexp-other-window
                                             (fboundp 'bmkp-all-tags-regexp-jump))
     (bmkp-some-tags-jump icicle-bookmark-some-tags
                          (fboundp 'bmkp-some-tags-jump))
     (bmkp-some-tags-jump-other-window icicle-bookmark-some-tags-other-window
                                       (fboundp 'bmkp-some-tags-jump))
     (bmkp-some-tags-regexp-jump icicle-bookmark-some-tags-regexp
                                 (fboundp 'bmkp-some-tags-regexp-jump))
     (bmkp-some-tags-regexp-jump-other-window icicle-bookmark-some-tags-regexp-other-window
                                              (fboundp 'bmkp-some-tags-regexp-jump))
     (bmkp-file-all-tags-jump icicle-bookmark-file-all-tags
                              (fboundp 'bmkp-file-all-tags-jump))
     (bmkp-file-all-tags-jump-other-window icicle-bookmark-file-all-tags-other-window
                                           (fboundp 'bmkp-file-all-tags-jump))
     (bmkp-file-all-tags-regexp-jump icicle-bookmark-file-all-tags-regexp
                                     (fboundp 'bmkp-file-all-tags-regexp-jump))
     (bmkp-file-all-tags-regexp-jump-other-window icicle-bookmark-file-all-tags-regexp-other-window
                                                  (fboundp 'bmkp-file-all-tags-regexp-jump))
     (bmkp-file-some-tags-jump icicle-bookmark-file-some-tags
                               (fboundp 'bmkp-file-some-tags-jump))
     (bmkp-file-some-tags-jump-other-window icicle-bookmark-file-some-tags-other-window
                                            (fboundp 'bmkp-file-some-tags-jump))
     (bmkp-file-some-tags-regexp-jump icicle-bookmark-file-some-tags-regexp
                                      (fboundp 'bmkp-file-some-tags-regexp-jump))
     (bmkp-file-some-tags-regexp-jump-other-window icicle-bookmark-file-some-tags-regexp-other-window
                                                   (fboundp 'bmkp-file-some-tags-regexp-jump))
     (bmkp-file-this-dir-all-tags-jump icicle-bookmark-file-this-dir-all-tags
                                       (fboundp 'bmkp-file-this-dir-all-tags-jump))
     (bmkp-file-this-dir-all-tags-jump-other-window icicle-bookmark-file-this-dir-all-tags-other-window
                                                    (fboundp 'bmkp-file-this-dir-all-tags-jump))
     (bmkp-file-this-dir-all-tags-regexp-jump icicle-bookmark-file-this-dir-all-tags-regexp
                                              (fboundp 'bmkp-file-this-dir-all-tags-regexp-jump))
     (bmkp-file-this-dir-all-tags-regexp-jump-other-window icicle-bookmark-file-this-dir-all-tags-regexp-other-window
                                                           (fboundp 'bmkp-file-this-dir-all-tags-regexp-jump))
     (bmkp-file-this-dir-some-tags-jump icicle-bookmark-file-this-dir-some-tags
                                        (fboundp 'bmkp-file-this-dir-some-tags-jump))
     (bmkp-file-this-dir-some-tags-jump-other-window icicle-bookmark-file-this-dir-some-tags-other-window
                                                     (fboundp 'bmkp-file-this-dir-some-tags-jump))
     (bmkp-file-this-dir-some-tags-regexp-jump icicle-bookmark-file-this-dir-some-tags-regexp
                                               (fboundp 'bmkp-file-this-dir-some-tags-regexp-jump))
     (bmkp-file-this-dir-some-tags-regexp-jump-other-window icicle-bookmark-file-this-dir-some-tags-regexp-other-window
                                                            (fboundp 'bmkp-file-this-dir-some-tags-regexp-jump))
     (find-tag icicle-find-tag
               (fboundp 'command-remapping))
     (find-tag-other-window icicle-find-first-tag-other-window t)
     (pop-tag-mark icicle-pop-tag-mark
                   (fboundp 'command-remapping))
     (eval-expression icicle-pp-eval-expression
                      (fboundp 'command-remapping))
     (pp-eval-expression icicle-pp-eval-expression
                         (fboundp 'command-remapping))
     ([S-f10]
      icicle-complete-menu-bar
      (fboundp 'icicle-complete-menu-bar))
     ([27 134217848]
      lacarte-execute-command
      (fboundp 'lacarte-execute-command))
     ([134217824]
      lacarte-execute-menu-command
      (fboundp 'lacarte-execute-menu-command))
     ([f10]
      lacarte-execute-menu-command
      (fboundp 'lacarte-execute-menu-command))))
 )

;; Switch on icicles mode
(icicle-mode t)

(provide 'icicles-setup)
