;; Gnus Message Mail Setup

;; Gnus User Details
(setq user-mail-address	"bvraghav@iitk.ac.in"
      user-full-name	"B.V. Raghav"
      mail-from-style   'angles
      )

;; Gnus Define posting styles
(setq gnus-posting-styles
      '((".*"
         (name "B.V. Raghav")
         (address "bvraghav@iitk.ac.in")
         (organization "Indian Institute of Technology, Kanpur")
         (signature "(B.V. Raghav)")
         ("Gcc" "nnimap+iitk:Sent"))
	(header "To" ".*\.iitk\.ac\.in"
		(organization "Indian Institute of Technology Kanpur")
		("Gcc" "nnimap+iitk:Sent"))
		;; (signature-file "~/.signature.gnus")
	(header "To" ".*\.bvraghav\.com"
		(signature "(B.V. Raghav)")
		(address "r@bvraghav.com")
		(organization "Indian Institute of Technology Kanpur")
		("X-Message-SMTP-Method" "smtp mail.bvraghav.com")
		("Gcc" "nnimap+r-bvr:INBOX.Sent"))
	))

;; Gnus Define message mode hooks
(add-hook 'message-mode-hook 'turn-on-orgtbl)

;; Gnus BBDB Setup
(bbdb-initialize 'gnus 'message)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(setq bbdb/news-auto-create-p t)
;; (require 'message-x)

(provide 'gnus-setup)
