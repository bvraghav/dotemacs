(use-package gnuplot
  :ensure t
  :mode ("\\.gnuplot\\'" . gnuplot-mode)
  :bind (:map gnuplot-mode-map
              ("C-c C-c" . gnuplot-send-buffer-to-gnuplot))
  )                                     ; The END

(provide 'gnuplot-setup)
