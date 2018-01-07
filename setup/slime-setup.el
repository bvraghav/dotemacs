;; slime-setup
;;-----------------------------------

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/bin/ecl")
(setq slime-contribs '(slime-fancy))

(provide 'slime-setup)
