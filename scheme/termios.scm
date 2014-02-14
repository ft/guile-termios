;; Copyright (c) 2014 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

;; Work in progress. Handling structs in guile's FFI isn't quite as convenient
;; as one might like. Also, for handling typedefs and #defines from C header
;; files, we kind of need to read those header files. This could be a job done
;; at build time, but... well, there's no code for that either. So I'm stuck.

;; This also misses some code to conveniently alter the standard fields in the
;; termios structure. (The speed entries have API functions for access.)

(define-module (termios)
  #:use-module (ice-9 optargs)
  #:use-module (system foreign)
  #:use-module (termios system)
  #:export (make-termios-struct
            parse-termios-struct

            cf-get-ispeed
            cf-get-ospeed
            cf-set-ispeed!
            cf-set-ospeed!

            tc-get-attr!
            tc-set-attr

            cf-make-raw
            tc-drain
            tc-flow
            tc-flush
            tc-send-break))

;; (dynamic-link "libc") fails on debian, because the "libc.so" file it finds
;; it not actually an ELF library but rather a GNU ld script. Without library,
;; this creates a library-handle for all symbols that are available when the
;; ‘dynamic-link’ call is made, which will likely contain the symbols from
;; libc, which is all we need.
(define libc (dynamic-link))

(define (make-termios-struct)
  (make-c-struct termios-struct
                 (list 0 0 0 0 0
                       (make-list termios-NCCS 0)
                       0 0)))

(define (parse-termios-struct termios)
  (parse-c-struct termios termios-struct))

;; Macro to help with multiple ‘pointer->procedure’ calls.
(define-syntax define-libc-procedure
  (syntax-rules ()
    ((_ retval name arg ...)
     (define name
       (pointer->procedure retval
                           (dynamic-func (symbol->string (quote name)) libc)
                           (list arg ...))))))

;; FFI links into the standard C library

(define-libc-procedure int tcdrain int)
(define-libc-procedure int tcflow int int)
(define-libc-procedure int tcflush int int)
(define-libc-procedure int tcsendbreak int int)

(define-libc-procedure int tcgetattr int '*)
(define-libc-procedure int tcsetattr int int '*)

(define-libc-procedure void cfmakeraw '*)

(define-libc-procedure speed-t cfgetispeed '*)
(define-libc-procedure speed-t cfgetospeed '*)

(define-libc-procedure int cfsetispeed '* speed-t)
(define-libc-procedure int cfsetospeed '* speed-t)
(define-libc-procedure int cfsetspeed '* speed-t)

;; Front-ends for the scheme world (interface using ports rather than file
;; descriptors).

(define (tc-drain port)
  (tcdrain (port->fdes port)))

(define (tc-flow port action)
  (tcflow (port->fdes port) action))

(define (tc-flush port queue-selector)
  (tcflush (port->fdes port) queue-selector))

(define (tc-send-break port duration)
  (tcsendbreak (port->fdes port) duration))

(define (tc-get-attr! port termios)
  (tcgetattr (port->fdes port) termios))

(define* (tc-set-attr port termios #:key (optional-action termios-TCSANOW))
  (tcsetattr (port->fdes port) optional-action termios))

(define (cf-make-raw termios)
  (cfmakeraw termios))

(define (cf-get-ispeed termios)
  (cfgetispeed termios))

(define (cf-get-ospeed termios)
  (cfgetospeed termios))

(define (cf-set-ispeed! termios speed)
  (cfsetispeed termios speed))

(define (cf-set-ospeed! termios speed)
  (cfsetospeed termios speed))

(define (cf-set-speed! termios speed)
  (cfsetspeed termios speed))
