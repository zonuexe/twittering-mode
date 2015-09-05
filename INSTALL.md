Installation Guide
==================

For Windows users
-----------------

* Uncompress the archive file, and add a path of a directory
  containing `twm-request.el` to your `load-path`.
  * For example, if you uncompressed it under `C:\foo\`, there may be
    a directory `C:\your-emacs-libs\twm-request-X.X.X` which contains
    `twm-request.el`. So add the following code to your `.emacs`

```el
(add-to-list 'load-path "C:/your-emacs-libs/twm-request-X.X.X")
```

For Linux and other Unix users
------------------------------

* Uncompress the archive file, and add a path of a directory
  containing `twm-request.el` to your `load-path`.
  * For example, if you uncompressed it under `/path/to/foo`, there may be
    a directory `/path/to/foo/twm-request-X.X.X/` which contains
    `twm-request.el`. So add the following code to your `.emacs`

```el
(add-to-list 'load-path "/path/to/your-emacs-libs/twm-request-X.X.X")
```

* Install cURL, gnutls, or openssl for using SSL connection.
