(require 'openwith)
(openwith-mode t)
(setq openwith-associations '(("\\.pdf\\'" "evince" (file))
			      ("\\.png\\'" "feh" (file))
			      ("\\.jpe?g\\'" "feh" (file))
			      ("\\.gif\\'" "feh" (file))
			      ("\\.tiff?\\'" "feh" (file))
			      ))

(provide 'openwith-setup)