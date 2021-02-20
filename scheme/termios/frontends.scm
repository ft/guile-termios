;; Copyright (c) 2021 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (termios frontends)
  #:use-module (ice-9 optargs)
  #:use-module (termios with-exceptions)
  #:use-module (termios system)
  #:export (termios-8n1))

(define* (termios-8n1 device #:optional (symbol-rate termios-B9600))
  "Set a DEVICE given by its file name to 8N1 mode.

Use SYMBOL-RATE as its symbol-rate. If SYMBOL-RATE is not given, the value
termios-B9600 from the (termios system) module is used."
  (let ((tty (open-io-file device))
        (ts (make-termios-struct)))
    (cf-make-raw! ts)
    (cf-set-speed! ts symbol-rate)
    (tc-set-attr tty ts)
    (close-port tty)))
