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

(defun tamil-setup ()
  (interactive)

  ;; "Tamil" U+0B80 - U+0BFF
  (set-fontset-font "fontset-default"
                    (cons (decode-char 'ucs #x0b80)
                          (decode-char 'ucs #x0bff))
                    "Noto Sans Tamil")

  ;; "Tamil Supplement" U+11FC0 - U+11FFF
  (set-fontset-font "fontset-default"
                    (cons (decode-char 'ucs #x11fc0)
                          (decode-char 'ucs #x11fff))
                    "Noto Sans Tamil"))

(defun indic-fonts-setup ()
  (interactive)
  (devanagari-setup)
  (tamil-setup)
  (define-key 'iso-transl-ctl-x-8-map "r" [?₹]))

(indic-fonts-setup)

(provide 'indic-setup)
