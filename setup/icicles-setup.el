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

;; Icicle Enhancements
(require 'crosshairs)
(crosshairs-mode t)

(require 'icomplete+)
(icompletep-cycling-mode t)

;; Dabbrev
(require 'dabbrev)

;; Dot Mode
(require 'dot-mode)
(dot-mode t)

;; ThingsAtPt+
(eval-after-load "thingatpt"
  '(when (require 'thingatpt+)
     (tap-redefine-std-fns)))

;; Bookmark+
(require 'bookmark+)

;; Dired+
(require 'dired+)

;; Switch on icicles mode
(icicle-mode t)

(provide 'icicles-setup)
