;; Copyright (c) 2014 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

;; This is a variant of the other test script. It uses the exception emitting
;; module of the library, so there' no need to throw manually.

(use-modules (termios with-exceptions))

(define tty (current-input-port))
(define ts (make-termios-struct))
(tc-get-attr! tty ts)
(write (parse-termios-struct ts))
(newline)
(quit 0)
