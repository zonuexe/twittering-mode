;;; twm-request-proxy.el --- HTTP Client for Emacs

;; Copyright (C) 2009-2015 Tadashi MATSUO
;;               2007, 2009-2011 Yuto Hayamizu.
;;               2008 Tsuyoshi CHO
;;               2014, 2015 Xavier Maillard

;; Author: Tadashi MATSUO <tad@mymail.twin.ne.jp>
;;	Y. Hayamizu <y.hayamizu@gmail.com>
;;	Tsuyoshi CHO <Tsuyoshi.CHO+develop@Gmail.com>
;;	Alberto Garcia <agarcia@igalia.com>
;;	Xavier Maillard <xavier@maillard.im>
;; Created: Sep 4, 2007
;; Version: HEAD
;; Identity: $Id: f0574b157c4e8ac5e3b25e0385a7131681260343 $
;; Keywords: http client
;; URL: https://github.com/zonuexe/twm-request

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; twm-request-proxy.el is HTTP Client for Emacs.
;; This library is drived from twittering-mode. http://github.com/hayamiz/twittering-mode

;;; Code:
(require 'url)

;;;;
;;;; Proxy setting / functions
;;;;

(defgroup twm-request-proxy nil
  "Subgroup handling `twm-request-mode' proxy setup."
  :group 'twm-request-mode)

(defcustom twm-request-proxy-use nil
  "*If non-nil, use PROXY.

See also `twm-request-proxy-server' for documentation."
  :type 'boolean
  :group 'twm-request-mode)

(defcustom twm-request-proxy-server nil
  "*Proxy server for `twm-request-mode'.

If both `twm-request-proxy-server' and `twm-request-proxy-port' are
non-nil, the variables `twm-request-proxy-*' have priority over other
variables `twm-request-http-proxy-*' or `twm-request-https-proxy-*'
regardless of HTTP or HTTPS.

To use individual proxies for HTTP and HTTPS, both `twm-request-proxy-server'
and `twm-request-proxy-port' must be nil."
  :group 'twm-request-proxy
  :type '(choice (const nil) string))

(defcustom twm-request-proxy-port nil
  "*Port number for `twm-request-mode'.

If both `twm-request-proxy-server' and `twm-request-proxy-port' are
non-nil, the variables `twm-request-proxy-*' have priority over other
variables `twm-request-http-proxy-*' or `twm-request-https-proxy-*'
regardless of HTTP or HTTPS.

To use individual proxies for HTTP and HTTPS, both `twm-request-proxy-server'
and `twm-request-proxy-port' must be nil."
  :group 'twm-request-proxy
  :type '(choice (const nil)
		 integer))

(defvar twm-request-proxy-keep-alive nil)
(defcustom twm-request-proxy-user nil
  "*Username for `twm-request-proxy-server'.

NOTE: If both `twm-request-proxy-server' and `twm-request-proxy-port' are
non-nil, the variables `twm-request-proxy-*' have priority over other
variables `twm-request-http-proxy-*' or `twm-request-https-proxy-*'
regardless of HTTP or HTTPS.")

(defcustom twm-request-proxy-password nil
  "*Password for `twm-request-proxy-server'.

NOTE: If both `twm-request-proxy-server' and `twm-request-proxy-port' are
non-nil, the variables `twm-request-proxy-*' have priority over other
variables `twm-request-http-proxy-*' or `twm-request-https-proxy-*'
regardless of HTTP or HTTPS."
  :group 'twm-request-proxy
  :type '(choice (const nil)
		 string))

(defcustom twm-request-http-proxy-server nil
  "*HTTP proxy server for `twm-request-mode'.
If nil, it is initialized on entering `twm-request-mode'.
The port number is specified by `twm-request-http-proxy-port'.
For HTTPS connection, the proxy specified by `twm-request-https-proxy-server'
and `twm-request-https-proxy-port' is used.

NOTE: If both `twm-request-proxy-server' and `twm-request-proxy-port' are
non-nil, the variables `twm-request-proxy-*' have priority over other
variables `twm-request-http-proxy-*' or `twm-request-https-proxy-*'
regardless of HTTP or HTTPS."
  :group 'twm-request-proxy
  :type '(choice (const nil)
		 string))

(defcustom twm-request-http-proxy-port nil
  "*Port number of a HTTP proxy server for `twm-request-mode'.
If nil, it is initialized on entering `twm-request-mode'.
The server is specified by `twm-request-http-proxy-server'.
For HTTPS connection, the proxy specified by `twm-request-https-proxy-server'
and `twm-request-https-proxy-port' is used.

NOTE: If both `twm-request-proxy-server' and `twm-request-proxy-port' are
non-nil, the variables `twm-request-proxy-*' have priority over other
variables `twm-request-http-proxy-*' or `twm-request-https-proxy-*'
regardless of HTTP or HTTPS."
  :group 'twm-request-proxy
  :type '(choice (const nil)
		 integer))

(defcustom twm-request-http-proxy-keep-alive nil
  "*If non-nil, the Keep-alive is enabled.  This is experimental."
  :group 'twm-request-proxy
  :type 'boolean)

(defcustom twm-request-http-proxy-user nil
  "*Username for `twm-request-http-proxy-server'.

NOTE: If both `twm-request-proxy-server' and `twm-request-proxy-port' are
non-nil, the variables `twm-request-proxy-*' have priority over other
variables `twm-request-http-proxy-*' or `twm-request-https-proxy-*'
regardless of HTTP or HTTPS."
  :group 'twm-request-proxy
  :type '(choice (const nil)
		 string))

(defcustom twm-request-http-proxy-password nil
  "*Password for `twm-request-http-proxy-server'.

NOTE: If both `twm-request-proxy-server' and `twm-request-proxy-port' are
non-nil, the variables `twm-request-proxy-*' have priority over other
variables `twm-request-http-proxy-*' or `twm-request-https-proxy-*'
regardless of HTTP or HTTPS."
  :group 'twm-request-proxy
  :type '(choice (const nil)
		 string))

(defcustom twm-request-https-proxy-server nil
  "*HTTPS proxy server for `twm-request-mode'.
If nil, it is initialized on entering `twm-request-mode'.
The port number is specified by `twm-request-https-proxy-port'.
For HTTP connection, the proxy specified by `twm-request-http-proxy-server'
and `twm-request-http-proxy-port' is used.

NOTE: If both `twm-request-proxy-server' and `twm-request-proxy-port' are
non-nil, the variables `twm-request-proxy-*' have priority over other
variables `twm-request-http-proxy-*' or `twm-request-https-proxy-*'
regardless of HTTP or HTTPS."
  :group 'twm-request-proxy
  :type '(choice (const nil)
		 string))

(defcustom twm-request-https-proxy-port nil
  "*Port number of a HTTPS proxy server for `twm-request-mode'.
If nil, it is initialized on entering `twm-request-mode'.
The server is specified by `twm-request-https-proxy-server'.
For HTTP connection, the proxy specified by `twm-request-http-proxy-server'
and `twm-request-http-proxy-port' is used.

NOTE: If both `twm-request-proxy-server' and `twm-request-proxy-port' are
non-nil, the variables `twm-request-proxy-*' have priority over other
variables `twm-request-http-proxy-*' or `twm-request-https-proxy-*'
regardless of HTTP or HTTPS."
  :group 'twm-request-proxy
  :type '(choice (const nil)
		 integer))

(defcustom twm-request-https-proxy-keep-alive nil
  "*If non-nil, the Keep-alive is enabled.  This is experimental."
  :group 'twm-request-proxy
  :type 'boolean)

(defcustom twm-request-https-proxy-user nil
  "*Username for `twm-request-https-proxy-server'.

NOTE: If both `twm-request-proxy-server' and `twm-request-proxy-port' are
non-nil, the variables `twm-request-proxy-*' have priority over other
variables `twm-request-http-proxy-*' or `twm-request-https-proxy-*'
regardless of HTTP or HTTPS."
  :group 'twm-request-proxy
  :type '(choice (const nil)
		 string))

(defcustom twm-request-https-proxy-password nil
  "*Password for `twm-request-https-proxy-server'.

NOTE: If both `twm-request-proxy-server' and `twm-request-proxy-port' are
non-nil, the variables `twm-request-proxy-*' have priority over other
variables `twm-request-http-proxy-*' or `twm-request-https-proxy-*'
regardless of HTTP or HTTPS."
  :group 'twm-request-proxy
  :type '(choice (const nil)
		 string))

(defun twm-request-normalize-proxy-vars ()
  "Normalize the type of `twm-request-http-proxy-port' and `twm-request-https-proxy-port'."
  (mapc (lambda (sym)
	  (let ((value (symbol-value sym)))
	    (cond
	     ((null value)
	      nil)
	     ((integerp value)
	      nil)
	     ((stringp value)
	      (set sym (string-to-number value)))
	     (t
	      (set sym nil)))))
	'(twm-request-proxy-port
	  twm-request-http-proxy-port
	  twm-request-https-proxy-port)))

(defun twm-request-proxy-info (scheme &optional item)
  "Return an alist for proxy configuration registered for SCHEME.
SCHEME must be a string \"http\", \"https\" or a symbol 'http or 'https.
The server name is a string and the port number is an integer."
  (twm-request-normalize-proxy-vars)
  (let ((scheme (if (symbolp scheme)
		    (symbol-name scheme)
		  scheme))
	(info-list
	 `((("http" "https")
	    . ((server . ,twm-request-proxy-server)
	       (port . ,twm-request-proxy-port)
	       (keep-alive . ,twm-request-proxy-keep-alive)
	       (user . ,twm-request-proxy-user)
	       (password . ,twm-request-proxy-password)))
	   (("http")
	    . ((server . ,twm-request-http-proxy-server)
	       (port . ,twm-request-http-proxy-port)
	       (keep-alive . ,twm-request-http-proxy-keep-alive)
	       (user . ,twm-request-http-proxy-user)
	       (password . ,twm-request-http-proxy-password)))
	   (("https")
	    . ((server . ,twm-request-https-proxy-server)
	       (port . ,twm-request-https-proxy-port)
	       (keep-alive . ,twm-request-https-proxy-keep-alive)
	       (user . ,twm-request-https-proxy-user)
	       (password . ,twm-request-https-proxy-password))))))
    (let ((info
	   (car (remove nil
			(mapcar
			 (lambda (entry)
			   (when (member scheme (car entry))
			     (let ((info (cdr entry)))
			       (when (and (cdr (assq 'server info))
					  (cdr (assq 'port info)))
				 info))))
			 info-list)))))
      (if item
	  (cdr (assq item info))
	info))))

(defun twm-request-url-proxy-services ()
  "Return the current proxy configuration in the format of `url-proxy-services'."
  (remove nil (mapcar
	       (lambda (scheme)
		 (let ((server (twm-request-proxy-info scheme 'server))
		       (port (twm-request-proxy-info scheme 'port)))
		   (when (and server port)
		     `(,scheme . ,(format "%s:%s" server port)))))
	       '("http" "https"))))

(defun twm-request-find-proxy (scheme)
  "Find proxy server and its port from the environmental variables and return
a cons pair of them.
SCHEME must be \"http\" or \"https\"."
  (cond
   ((require 'url-methods nil t)
    (url-scheme-register-proxy scheme)
    (let* ((proxy-service (assoc scheme url-proxy-services))
	   (proxy (if proxy-service (cdr proxy-service) nil)))
      (if (and proxy
	       (string-match "^\\([^:]+\\):\\([0-9]+\\)$" proxy))
	  (let ((host (match-string 1 proxy))
		(port (string-to-number (match-string 2 proxy))))
	    (cons host port))
	nil)))
   (t
    (let* ((env-var (concat scheme "_proxy"))
	   (env-proxy (or (getenv (upcase env-var))
			  (getenv (downcase env-var))))
	   (default-port (if (string= "https" scheme) "443" "80")))
      (if (and env-proxy
	       (string-match
		"^\\(https?://\\)?\\([^:/]+\\)\\(:\\([0-9]+\\)\\)?/?$"
		env-proxy))
	  (let* ((host (match-string 2 env-proxy))
		 (port-str (or (match-string 4 env-proxy) default-port))
		 (port (string-to-number port-str)))
	    (cons host port))
	nil)))))

(defun twm-request-setup-proxy ()
  "Setup HTTP proxy."
  (when (require 'url-methods nil t)
    ;; If `url-scheme-registry' is not initialized,
    ;; `url-proxy-services' will be reset by calling
    ;; `url-insert-file-contents' or `url-retrieve-synchronously', etc.
    ;; To avoid it, initialize `url-scheme-registry' by calling
    ;; `url-scheme-get-property' before calling such functions.
    (url-scheme-get-property "http" 'name)
    (url-scheme-get-property "https" 'name))
  (unless (and twm-request-http-proxy-server
	       twm-request-http-proxy-port)
    (let ((info (twm-request-find-proxy "http")))
      (setq twm-request-http-proxy-server (car-safe info))
      (setq twm-request-http-proxy-port (cdr-safe info))))
  (unless (and twm-request-https-proxy-server
	       twm-request-https-proxy-port)
    (let ((info (twm-request-find-proxy "https")))
      (setq twm-request-https-proxy-server (car-safe info))
      (setq twm-request-https-proxy-port (cdr-safe info))))
  (if (and twm-request-proxy-use
	   (null (twm-request-proxy-info "http"))
	   (null (twm-request-proxy-info "https")))
      (progn
	(message "Disabling proxy due to lack of configuration.")
	(setq twm-request-proxy-use nil))
    t))

(defun twm-request-toggle-proxy ()
  "Toggle to use HTTP proxy and return nil/'on/'off."
  (interactive)
  (setq twm-request-proxy-use
	(not twm-request-proxy-use))
  (if (twm-request-setup-proxy)
      (if twm-request-proxy-use 'on 'off))
  nil)

(provide 'twm-request-proxy)
;;; twm-request-proxy.el ends here
