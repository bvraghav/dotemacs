;; Define Group
(defgroup bvr-proxy nil
  "Set up environment proxy for network connections")

;; Define Custom vars
(defcustom bvr-proxy-url "http://hostname:port/"
  "The URL to set as proxy environment"
  :type '(string)
  :group 'bvr-proxy)

(defcustom bvr-proxy-vars
  '("http_proxy"
    "https_proxy"
    "ftp_proxy"
    "socks_proxy"
    "rsync_proxy")
  "The environment variables to set."
  :type '(repeat (string)))

;; Define script vars
(defvar bvr-proxy-is-active nil)

;; Define functions
(defun bvr-proxy-enable ()
  (interactive)
  (dolist (var bvr-proxy-vars)
    (setenv var bvr-proxy-url)
    (message "%s: %s" var (getenv var)))
  (setq bvr-proxy-is-active t))

(defun bvr-proxy-disable ()
  (interactive)
  (dolist (var bvr-proxy-vars)
    (setenv var "")
    (message "%s: %s" var (getenv var)))
  (setq bvr-proxy-is-active nil))

(defun bvr-proxy-toggle ()
  (interactive)
  (if bvr-proxy-is-active
      (bvr-proxy-disable)
    (bvr-proxy-enable)))

(provide 'proxy-setup)
