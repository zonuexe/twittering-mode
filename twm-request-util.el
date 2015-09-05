;;; twm-request-util.el --- HTTP Client for Emacs

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

(defun twm-request-process-alive-p (proc)
  "Return non-nil if PROC is alive."
  (not (memq (process-status proc) '(nil closed exit failed signal))))

(defun twm-request-start-process-with-sentinel (name buffer program args sentinel)
  "Start PROGRAM in a subprocess with SENTINEL.

This function is the same as `start-process' except that SENTINEL must
be invoked when the process is successfully started."
  (let ((proc (apply 'start-process name buffer program args)))
    (when (and proc (functionp sentinel))
      (if (twm-request-process-alive-p proc)
	  (set-process-sentinel proc sentinel)
	;; Ensure that the sentinel is invoked if a subprocess is
	;; successfully started.
	(funcall sentinel proc "finished")))
    proc))

(provide 'twm-request-util)
;;; twm-request-util.el ends here
