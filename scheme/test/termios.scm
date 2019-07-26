;; Copyright (c) 2014-2019 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (test termios)
  #:use-module (ice-9 optargs)
  #:export (open-device
            name-value-pair))

(define* (open-device #:key (device (getenv "GUILE_TERMIOS_TTY")))
  (open-io-file device))

(define-syntax name-value-pair
  (syntax-rules ()
    ((_ x) (cons 'x x))))
