twm-request
===========

HTTP Client for Emacs. This library is drived from [twittering-mode](http://github.com/hayamiz/twittering-mode).

Features
--------

* HTTP Proxy support
* Secure connection via HTTPS (cURL, GNU Wget, OpenSSL or GnuTLS is required)

Supported Emacsen
-----------------

* GNU Emacs 21 (some restrictions)
* GNU Emacs 22, 23, 24

Prerequisites
-------------

- For SSL connection, one of the followings is required.
  SSL connection is required for the Twitter REST API v1.1.
  - cURL http://curl.haxx.se/
  - GNU Wget http://www.gnu.org/software/wget/
  - OpenSSL http://www.openssl.org/
  - GnuTLS http://www.gnu.org/software/gnutls/

Quick start
-----------

### Manual Install

Put *twm-request.el* in a directory specified by the variable `load-path`. Note that the directories *emacs21* and *url-emacs21* must be placed at the same directory on Emacs 21. On Windows without curl or wget, the directory *win-curl* must be placed there. You can add a directory to the variable `load-path` by `(add-to-list 'load-path "ADDITIONAL-DIRECTORY")`.

Authors & Contributors
----------------------

- Y. Hayamizu
- naoya_t
- Tsuyoshi CHO
- Alberto Garcia
- Satoshi Yatagawa
- 高山智也
- Tadashi MATSUO (cvmat)
- 青田(naota)
- Jaemok Jeong(jmjeong)
- Thomas Danckaert
- IMAI Toshiyuki
