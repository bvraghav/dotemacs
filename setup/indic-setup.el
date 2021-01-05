(defun devanagari-setup ()
  (interactive)

  ;; "Devanagari"  U+0900 - U+1097F
  (set-fontset-font "fontset-default"
                    (cons (decode-char 'ucs #x0900)
                          (decode-char 'ucs #x097f))
                    "Eczar")

  ;; "Devanagari Extended"  U+A8E0 - U+A8FF
  (set-fontset-font "fontset-default"
                    (cons (decode-char 'ucs #xa8e0)
                          (decode-char 'ucs #xa8ff))
                    "Eczar")

  ;; "Vedic Extensions" (Devanagari) U+1CD0 - U+1CFF
  (set-fontset-font "fontset-default"
                    (cons (decode-char 'ucs #x1cd0)
                          (decode-char 'ucs #x1cff))
                    "Eczar"))

(defun indic-fonts-setup ()
  (interactive)
  (devanagari-setup))

(indic-fonts-setup)

(provide 'indic-setup)
