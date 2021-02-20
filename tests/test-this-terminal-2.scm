;; Copyright (c) 2014-2021 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

;; This is a variant of the other test script. It uses the exception emitting
;; module of the library, so there' no need to throw manually.

(use-modules (termios with-exceptions))

(define name "test-this-terminal-2")
(define tty (current-input-port))
(define ts (make-termios-struct))

(unless (isatty? tty)
  (format #t "Skipping ~a: stdin is not a terminal.~%" name)
  (quit 0))

(tc-get-attr! tty ts)
(write (parse-termios-struct ts))
(newline)
(quit 0)
