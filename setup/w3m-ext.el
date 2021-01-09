;; Requires
(use-package w3m
  :ensure t

  :hook ((w3m-mode . w3m-lnum-mode)
	 (w3m-display-mode . bvr-w3m-remove-trailing-ws))

  :bind (:map w3m-mode-map
	      ("z s" . w3m-session-save)
	      ("z l" . w3m-session-save)
	      ("F" . w3m-toggle-filtering)
	      ("f" . w3m-lnum-follow))  

  :init
  (require 'w3m-search)
  (require 'w3m-lnum)
  (require 'w3m-session)
  (w3m-fb-mode)
  (autoload 'w3m-browse-url "w3m" "w3m Web Browser." t)

  :config

  ;; Variables
  (setq w3m-cookie-accept-bad-cookies (quote ask)
	w3m-enable-google-feeling-lucky t
	w3m-fill-column 55
	w3m-search-default-engine "glucky"
	w3m-search-engine-alist
	(quote
	 (("numpy" "https://www.google.com/search?btnI=1&q=site:docs.scipy.org+%s" nil)
	  ("python2-api" "https://www.google.com/search?btnI=1&q=site:docs.python.org%%2f2+%s" nil)
	  ("tensorflow-api" "https://www.google.com/search?btnI=1&q=site:tensorflow.org+%s" nil)
	  ("duckduckgo" "https://duckduckgo.com/lite/?q=%s" nil)
	  ("worldclock" "http://www.timeanddate.com/worldclock/results.html?query=%s" nil)
	  ("wikipedia-en" "http://en.wikipedia.org/wiki/Special:Search?search=%s" nil)
	  ("weather" "http://www.weather.com/search/search?where=%s&what=WeatherLocalUndeclared" nil)
	  ("syndic8" "http://www.syndic8.com/feedlist.php?ShowStatus=all&ShowMatch=%s" nil)
	  ("google-groups" "http://www.google.com/groups?q=%s" nil)
	  ("ebay" "http://search.ebay.com/search/search.dll?query=%s" nil)
	  ("acronym" "http://www.acronymfinder.com/af-query.asp?acronym=%s&string=exact" nil)
	  ("yahoo" "http://search.yahoo.com/bin/search?p=%s" nil)
	  ("yahoo-ja" "http://search.yahoo.co.jp/bin/search?p=%s" euc-japan)
	  ("alc" "http://eow.alc.co.jp/%s/UTF-8/" utf-8)
	  ("blog" "http://blogsearch.google.com/blogsearch?q=%s&oe=utf-8&ie=utf-8" utf-8)
	  ("blog-en" "http://blogsearch.google.com/blogsearch?q=%s&hl=en&oe=utf-8&ie=utf-8" utf-8)
	  ("glucky" "http://www.google.com/search?btnI=1&q=%s" utf-8)
	  ("google" "http://www.google.com/search?q=%s&ie=utf-8&oe=utf-8" utf-8)
	  ("google-en" "http://www.google.com/search?q=%s&hl=en&ie=utf-8&oe=utf-8" utf-8)
	  ("google news" "http://news.google.com/news?q=%s&ie=utf-8&oe=utf-8" utf-8)
	  ("google news-en" "http://news.google.com/news?q=%s&hl=en&ie=utf-8&oe=utf-8" nil)
	  ("google groups" "http://groups.google.com/groups?q=%s" nil)
	  ("All the Web" "http://www.alltheweb.com/search?q=%s&web&_sb_lang=en" nil)
	  ("All the Web-ja" "http://www.alltheweb.com/search?q=%s&web&_sb_lang=ja&cs=euc-jp" euc-japan)
	  ("technorati" "http://www.technorati.com/search/%s" utf-8)
	  ("technorati-ja" "http://www.technorati.jp/search/search.html?query=%s&language=ja" utf-8)
	  ("technorati-tag" "http://www.technorati.com/tag/%s" utf-8)
	  ("goo-ja" "http://search.goo.ne.jp/web.jsp?MT=%s" euc-japan)
	  ("excite-ja" "http://www.excite.co.jp/search.gw?target=combined&look=excite_jp&lang=jp&tsug=-1&csug=-1&search=%s" shift_jis)
	  ("altavista" "http://altavista.com/sites/search/web?q=\"%s\"&kl=ja&search=Search" nil)
	  ("rpmfind" "http://rpmfind.net/linux/rpm2html/search.php?query=%s" nil)
	  ("debian-pkg" "http://packages.debian.org/cgi-bin/search_contents.pl?directories=yes&arch=i386&version=unstable&case=insensitive&word=%s" nil)
	  ("debian-bts" "http://bugs.debian.org/cgi-bin/pkgreport.cgi?archive=yes&pkg=%s" nil)
	  ("freebsd-users-jp" "http://home.jp.FreeBSD.org/cgi-bin/namazu.cgi?key=\"%s\"&whence=0&max=50&format=long&sort=score&dbname=FreeBSD-users-jp" euc-japan)
	  ("iij-archie" "http://www.iij.ad.jp/cgi-bin/archieplexform?query=%s&type=Case+Insensitive+Substring+Match&order=host&server=archie1.iij.ad.jp&hits=95&nice=Nice" nil)
	  ("waei" "http://dictionary.goo.ne.jp/search.php?MT=%s&kind=je" euc-japan)
	  ("eiwa" "http://dictionary.goo.ne.jp/search.php?MT=%s&kind=ej" nil)
	  ("kokugo" "http://dictionary.goo.ne.jp/search.php?MT=%s&kind=jn" euc-japan)
	  ("eiei" "http://www.dictionary.com/cgi-bin/dict.pl?term=%s&r=67" nil)
	  ("amazon" "http://www.amazon.com/exec/obidos/search-handle-form/250-7496892-7797857" iso-8859-1 "url=index=blended&field-keywords=%s")
	  ("amazon-ja" "http://www.amazon.co.jp/gp/search?__mk_ja_JP=%%83J%%83%%5E%%83J%%83i&url=search-alias%%3Daps&field-keywords=%s" shift_jis)
	  ("emacswiki" "http://www.emacswiki.org/cgi-bin/wiki?search=%s" nil)
	  ("en.wikipedia" "http://en.wikipedia.org/wiki/Special:Search?search=%s" nil)
	  ("de.wikipedia" "http://de.wikipedia.org/wiki/Spezial:Search?search=%s" utf-8)
	  ("ja.wikipedia" "http://ja.wikipedia.org/wiki/Special:Search?search=%s" utf-8)
	  ("msdn" "http://search.msdn.microsoft.com/search/default.aspx?query=%s" nil)
	  ("teld" "https://newoag.iitk.ac.in/Tel/TelephoneDirectory1.php?Select0=S&Select=dept&FDept=%s" nil)
	  ("freshmeat" "http://freshmeat.net/search/?q=%s&section=projects" nil)))
	w3m-uri-replace-alist
	(quote
	 (("\\`np:" w3m-search-uri-replace "numpy")
	  ("\\`py:" w3m-search-uri-replace "python2-api")
	  ("\\`tf:" w3m-search-uri-replace "tensorflow-api")
	  ("\\`du:" w3m-search-uri-replace "duckduckgo")
	  ("\\`g:" w3m-search-uri-replace "glucky")
	  ("\\`gg:" w3m-search-uri-replace "google")
	  ("\\`ggg:" w3m-search-uri-replace "google groups")
	  ("\\`ya:" w3m-search-uri-replace "yahoo")
	  ("\\`al:" w3m-search-uri-replace "altavista")
	  ("\\`bts:" w3m-search-uri-replace "debian-bts")
	  ("\\`dpkg:" w3m-search-uri-replace "debian-pkg")
	  ("\\`archie:" w3m-search-uri-replace "iij-archie")
	  ("\\`alc:" w3m-search-uri-replace "alc")
	  ("\\`teld:" w3m-search-uri-replace "teld")
	  ("\\`urn:ietf:rfc:\\([0-9]+\\)" w3m-pattern-uri-replace "http://www.ietf.org/rfc/rfc\\1.txt"))))

  ;; Make the previous search engine the default for the next
  ;; search.
  (defadvice w3m-search (after change-default activate)
    (let ((engine (nth 1 minibuffer-history)))
      (when (assoc engine w3m-search-engine-alist)
	(setq w3m-search-default-engine engine))))

  ;; Remove trailing ws
  (defun bvr-w3m-remove-trailing-ws (url)
    (let ((buffer-read-only nil))
      (delete-trailing-whitespace)))

  ;; Forget Authinfo
  ;; -----------------------------------
  ;; Required if I land up in `la la land'
  ;; For more info refer (bookmark) `EmacsWiki: w3m authentication forget la la land'
  (defun w3m-erase-authinfo-root (root)
    (setq w3m-process-authinfo-alist
          (assq-delete-all 
           nil (mapcar 
		(lambda (elem) (if (not (equal root (car elem))) elem)) 
		w3m-process-authinfo-alist))))
  ;; contd...
  (defun w3m-forget-authinfo ()
    (interactive)
    (let* ((roots (mapcar
                   (lambda (elem) (list (car elem) (car elem)))
                   w3m-process-authinfo-alist))
           (root (completing-read "URL: " roots nil t)))
      (w3m-erase-authinfo-root root))))


;; Provide Package
;; -----------------------------------
(provide 'w3m-ext)
