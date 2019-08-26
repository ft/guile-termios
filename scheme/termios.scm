;; Copyright (c) 2014-2019 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (termios)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 optargs)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (termios system)
  #:export (make-termios-struct
            parse-termios-struct
            get-field-from-termios
            get-from-c-cc
            put-field-into-termios!
            put-into-c-cc

            cf-get-ispeed
            cf-get-ospeed
            cf-set-ispeed!
            cf-set-ospeed!
            cf-set-speed!

            tc-get-attr!
            tc-set-attr

            set-rts
            set-dtr

            cf-make-raw!
            tc-drain
            tc-flow
            tc-flush
            tc-send-break

            termios-failure?
            termios-version))

(define termios-version "0.5+git")

;; (dynamic-link "libc") fails on debian, because the "libc.so" file it finds
;; it not actually an ELF library but rather a GNU ld script. Without library,
;; this creates a library-handle for all symbols that are available when the
;; ‘dynamic-link’ call is made, which will likely contain the symbols from
;; libc, which is all we need.
;;
;; On cygwin, the library that contains the symbols required in here is
;; "cygwin1.dll".

(define-syntax dynamic-link-w/system
  (lambda (x)
    (syntax-case x ()
      ((_) (cond ((string-suffix? "-cygwin" %host-type)
                  #'(dynamic-link "cygwin1"))
                 (else #'(dynamic-link)))))))

(define libc (dynamic-link-w/system))

(define-syntax-rule (maybe dynamic name lib-handle)
  (catch #t
    (lambda () (dynamic name lib-handle))
    (lambda (k . a) #f)))

(define (termios-failure? result)
  (< result 0))

(define termios-struct
  (map (lambda (x) (cddr x))
       termios-struct-offsets))

(define termios-empty
  (map (lambda (x)
         (if (eq? (car x) 'c-cc)
             (make-list termios-NCCS 0)
             0))
       termios-struct-offsets))

(define* (make-termios-struct #:optional (value termios-empty))
  (make-c-struct termios-struct value))

(define (parse-termios-struct termios)
  (parse-c-struct termios termios-struct))

(define termios-fields (map (lambda (x) (car x))
                            termios-struct-offsets))

(define (get-field-from-termios lst field)
  (let ((idx (list-index termios-fields field)))
    (if idx
        (list-ref lst idx)
        #f)))

(define (put-field-into-termios! lst field value)
  (let ((idx (list-index termios-fields field)))
    (if idx
        (list-set! lst idx value)
        #f)))

(define (get-from-c-cc lst field)
  (list-ref (get-field-from-termios lst 'c-cc) field))

(define (put-into-c-cc lst field value)
  (list-set! (get-field-from-termios lst 'c-cc) field value))

;; Macro to help with multiple ‘pointer->procedure’ calls.
(define-syntax define-libc-procedure*
  (syntax-rules ()
    ((_ with-errno? retval name arg ...)
     (define name
       (let ((df (catch #t
                   (lambda ()
                     (dynamic-func (symbol->string (quote name)) libc))
                   (lambda (k . a)
                     (unless (memq (quote name)
                                   '(cfsetspeed cfmakeraw))
                       ;; If we're in here, the FFI couldn't find a function
                       ;; symbol in the given library link (the libc variable)
                       ;; and there is no workaround for the missing function.
                       ;; Warn the user about that upon loading the module.
                       (format (current-error-port)
                               "guile-termios: WARNING! Symbol not found: ~a~%"
                               (quote name)))
                     #f))))
         (if df
             (pointer->procedure retval df (list arg ...)
                                 #:return-errno? with-errno?)
             #f))))))

(define-syntax-rule (define-libc-procedure args ...)
  (define-libc-procedure* #f args ...))

(define-syntax-rule (define-errno-procedure args ...)
  (define-libc-procedure* #t args ...))

;; FFI links to the POSIX termios functions in the C library

(define-errno-procedure int tcdrain int)
(define-errno-procedure int tcflow int int)
(define-errno-procedure int tcflush int int)
(define-errno-procedure int tcsendbreak int int)

(define-errno-procedure int tcgetattr int '*)
(define-errno-procedure int tcsetattr int int '*)

(define-libc-procedure void cfmakeraw '*)

(define-libc-procedure speed-t cfgetispeed '*)
(define-libc-procedure speed-t cfgetospeed '*)

(define-errno-procedure int cfsetispeed '* speed-t)
(define-errno-procedure int cfsetospeed '* speed-t)
(define-errno-procedure int cfsetspeed '* speed-t)

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

(define* (tc-set-attr port termios #:key (when termios-TCSANOW))
  (tcsetattr (port->fdes port) when termios))

(define (cfmakeraw-fallback termios)
  (let* ((ts-scm (parse-termios-struct termios))
         (cflag (get-field-from-termios ts-scm 'c-cflag))
         (oflag (get-field-from-termios ts-scm 'c-oflag))
         (iflag (get-field-from-termios ts-scm 'c-iflag))
         (lflag (get-field-from-termios ts-scm 'c-lflag)))
    (put-field-into-termios!
     ts-scm 'c-iflag (logand iflag
                             (logxor iflag
                                     (logior termios-BRKINT
                                             termios-IGNBRK
                                             termios-PARMRK
                                             termios-ISTRIP
                                             termios-INLCR
                                             termios-IGNCR
                                             termios-ICRNL
                                             termios-IXON))))
    (put-field-into-termios!
     ts-scm 'c-oflag (logand oflag (logxor oflag termios-OPOST)))
    (put-field-into-termios!
     ts-scm 'c-lflag (logand lflag
                             (logxor lflag
                                     (logior termios-ECHO
                                             termios-ECHONL
                                             termios-ICANON
                                             termios-ISIG
                                             termios-IEXTEN))))
    (put-field-into-termios!
     ts-scm 'c-cflag (logior termios-CS8
                             (logand cflag
                                     (logxor cflag
                                             (logior termios-CSIZE
                                                     termios-PARENB)))))
    (make-termios-struct ts-scm)))

(define-syntax cf-make-raw!
  (syntax-rules ()
    ((_ termios)
     (if cfmakeraw
         (cfmakeraw termios)
         (set! termios (cfmakeraw-fallback termios))))))

(define (cf-get-ispeed termios)
  (cfgetispeed termios))

(define (cf-get-ospeed termios)
  (cfgetospeed termios))

(define (cf-set-ispeed! termios speed)
  (cfsetispeed termios speed))

(define (cf-set-ospeed! termios speed)
  (cfsetospeed termios speed))

(define (cf-set-speed! termios speed)
  (if cfsetspeed
      (cfsetspeed termios speed)
      (let  ((res (cfsetispeed termios speed)))
        (if (< res 0)
            res
            (cfsetospeed termios speed)))))

;; Beyond termios

(define-libc-procedure int ioctl int long '*)

(define (ioctl-tiocmget port)
  (let* ((n (sizeof int))
         (bv (make-bytevector n 0))
         (p (bytevector->pointer bv)))
    (ioctl (port->fdes port) termios-TIOCMGET p)
    (bytevector-sint-ref (pointer->bytevector p n) 0 (native-endianness) n)))

(define (ioctl-tiocmset port value)
  (let ((bv (make-bytevector (sizeof int))))
    (bytevector-sint-set! bv 0 value (native-endianness) (sizeof int))
    (ioctl (port->fdes port) termios-TIOCMSET (bytevector->pointer bv))))

(define (set-io-bit port bit value)
  (let ((raw (ioctl-tiocmget port)))
    (ioctl-tiocmset port (if value
                             (logior bit raw)
                             (logand (lognot bit) raw)))))

(define (set-rts port value) (set-io-bit port termios-TIOCM-RTS value))
(define (set-dtr port value) (set-io-bit port termios-TIOCM-DTR value))
