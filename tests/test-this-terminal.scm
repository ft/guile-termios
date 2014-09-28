;; Copyright (c) 2014 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

;; This is quite the trivial test: It works if stdin is connected to a
;; terminal; with that it queries the serial attributes of the terminal and
;; pretty-prints them to stdout. If ‘tc-get-attr!’ fails, the script will throw
;; an exception and the test will therefore fail. It's not much of a test, but
;; it makes sure, the module isn't complete broken.

(use-modules (termios))

(define name "test-this-terminal")
(define tty (current-input-port))
(define ts (make-termios-struct))

(unless (isatty? tty)
  (format #t "Skipping ~a: stdin is not a terminal.~%" name)
  (quit 0))

(when (termios-failure? (tc-get-attr! tty ts))
  (throw 'tc-get-attr!-failed))

(write (parse-termios-struct ts))
(newline)
(quit 0)
