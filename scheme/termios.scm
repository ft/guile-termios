;; Copyright (c) 2014 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (termios)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 optargs)
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

            call-with-errno
            get-errno
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

;; ‘errno’ handling:
;;
;; Most termios procedures in the C library set the ‘errno’ variable in case of
;; errors to point to the right diagnostic for the underlying problem. At this
;; point, Guile's dynamic FFI does not provide portable access to that value:
;;
;;   http://debbugs.gnu.org/cgi/bugreport.cgi?bug=18592
;;
;; The following implements a procedure ‘get-errno’, that returns the current
;; value of ‘errno’. The naïve thing to do would be:
;;
;;   (tc-drain prt)
;;   (get-errno)
;;
;; The issue with this is, that Guile's runtime might run some functionality
;; that touches errno in between those two calls. On IRC, Ludovic Courtès
;; mentions the use of ‘call-with-blocked-asyncs’ circumvent this.

(define-syntax-rule (maybe dynamic name lib-handle)
  (catch #t
    (lambda () (dynamic name lib-handle))
    (lambda (k . a) #f)))

;; The ‘errno’ value in this module looks like this:
;;
;; (<pointer> <type> <name>)
;;
;; or #f in case no suitable pointer could be found.
;;
;; <type> is either ‘function’ or ‘variable’. <name> is a string: The name of
;; the symbol in the C library to lookup.
;;
;; Why would there be functions? Well, some C libraries have per-thread values
;; for ‘errno’ and the functions return a pointer to the location of the value
;; for the particular thread. Thus ‘errno’ is defined like this:
;;
;;   #define errno (*__errno())
;;
;; This code supports these kinds of functions and falls back to a raw ‘errno’
;; value in case no such function could be found.

(define errno-locations
  '(("__errno_location" . function)     ; glibc
    ("__errno" . function)              ; cygwin
    ("errno" . variable)))              ; fallback

(define errno (let loop ((loc errno-locations))
                (cond ((null? loc) #f)
                      ((eq? 'function (cdar loc))
                       (let* ((name (caar loc))
                              (func (maybe dynamic-func name libc)))
                         (if func
                             (list (pointer->procedure '* func '())
                                   'function name)
                             (loop (cdr loc)))))
                      (else
                       (let* ((name (caar loc))
                              (ptr (maybe dynamic-pointer name libc)))
                         (if ptr
                             (list ptr 'variable name)
                             (loop (cdr loc))))))))

;; In case ‘errno’ is #f, something went horribly wrong. In that case
;; ‘get-errno’ always returns 0, because the real errno value could not be
;; retrieved. The same is true, if the location that was indicated by the C
;; library is the NULL pointer. Otherwise the location is parsed using
;; ‘parse-c-struct’ with a single entry: ‘errno-t’
;;
;; ‘errno-t’ is determined upon module generation. It's actual type can be
;; found in (termios system).

(define get-errno
  (if errno
      (if (eq? 'function (cadr errno))
          (lambda ()
            (let* ((func (car errno))
                   (raw (func)))
              (if (null-pointer? raw)
                  0
                  (car (parse-c-struct raw (list errno-t))))))
          (if (null-pointer? (car errno))
              (lambda () 0)
              (lambda () (car (parse-c-struct (car errno)
                                              (list errno-t))))))
      (lambda () 0)))

;; To deal with ‘call-with-blocked-asyncs’ easier, here is a bit of syntactic
;; sugar:
;;
;;   ...
;;   (define tty "/dev/ttyUSB0")
;;   (define prt (open-io-file tty))
;;   (define ta (make-termios-struct)
;;   (call-with-errno (errno (tc-get-attr! prt ts))
;;     (strerror errno)
;;     (close prt)
;;     (quit EXIT_FAILURE))
;;   ...
;;
;; It's like a ‘let’ with only one value, where the expressions in the body are
;; called only of the expression setting the value failed. In case it didn't
;; fail the whole expression returns #t.

(define-syntax-rule (call-with-errno (errno exp) fail0 fail1 ...)
  (let-values (((failed? errno) (call-with-blocked-asyncs
                                 (lambda ()
                                   (let* ((f? (termios-failure? exp))
                                          (err (if f? (get-errno) 0)))
                                     (values f? err))))))
    (if failed?
        (begin fail0 fail1 ...)
        #t)))

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
(define-syntax define-libc-procedure
  (syntax-rules ()
    ((_ retval name arg ...)
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
             (pointer->procedure retval df (list arg ...))
             #f))))))

;; FFI links to the POSIX termios functions in the C library

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
    (ioctl (port->fdes port) TIOCMGET p)
    (bytevector-sint-ref (pointer->bytevector p n) 0 (native-endianness) n)))

(define (ioctl-tiocmset port value)
  (let ((bv (make-bytevector (sizeof int))))
    (bytevector-sint-set! bv 0 value (native-endianness) (sizeof int))
    (ioctl (port->fdes port) TIOCMSET (bytevector->pointer bv))))

(define (set-io-bit port bit value)
  (let ((raw (ioctl-tiocmget port)))
    (ioctl-tiocmset port (if value
                             (logior bit raw)
                             (logand (lognot bit) raw)))))

(define (set-rts port value) (set-io-bit port termios-TIOCM-RTS value))
(define (set-dtr port value) (set-io-bit port termios-TIOCM-DTR value))
