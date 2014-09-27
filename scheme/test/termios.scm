;; Copyright (c) 2014 guile-termios workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (test termios)
  #:use-module (ice-9 optargs)
  #:export (open-device))

(define* (open-device #:key (device (getenv "GUILE_TERMIOS_TTY")))
  (open-io-file device))

