;; slime-setup
;;-----------------------------------

(load (expand-file-name "~/quicklisp/slime-helper.el"))

(setq inferior-lisp-program "/usr/bin/sbcl")
;; There is trouble running autodoc with ecl
(remove-hook 'slime-mode-hook #'slime-autodoc--on)

(setq slime-contribs '(slime-fancy))

(provide 'slime-setup)
