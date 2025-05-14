(defun devanagari-setup ()
  (interactive)

  ;; "Devanagari"  U+0900 - U+097F
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

(defun gurmukhi-setup ()
  (interactive)

  ;; "Gurmukhi" U+0A00 - U+0A7f
  (set-fontset-font "fontset-default"
                    (cons (decode-char 'ucs #x0a00)
                          (decode-char 'ucs #x0a7f))
                    "Noto Serif Gurmukhi"))

(defun indic-fonts-setup ()
  (interactive)
  (devanagari-setup)
  (tamil-setup)
  (gurmukhi-setup)
  (define-key 'iso-transl-ctl-x-8-map "r" [?₹]))

(defun cjk-setup ()
  "Setup unicode for CJK."
  (interactive)
  ;; "CJK" U+4E00–9FFF
  (set-fontset-font "fontset-default"
                    (cons (decode-char 'ucs #x4e00)
                          (decode-char 'ucs #x9fff))
                    "Jigmo"))

(defun unicode-setup ()
  (interactive)
  (indic-fonts-setup)
  (cjk-setup))

(unicode-setup)

(provide 'unicode-setup)
