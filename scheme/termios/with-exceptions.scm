;; Copyright (c) 2014 guile-termios workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (termios with-exceptions)
  #:use-module ((termios) #:prefix base:)
  #:use-module (termios system)
  #:re-export ((base:cf-get-ispeed . cf-get-ispeed)
               (base:cf-get-ospeed . cf-get-ospeed)
               (base:cf-make-raw! . cf-make-raw!)
               (base:make-termios-struct . make-termios-struct)
               (base:parse-termios-struct . parse-termios-struct)
               (base:termios-failure? . termios-failure?)
               (base:termios-version . termios-version))
  #:export (get-field-from-termios
            put-field-into-termios!

            cf-set-ispeed!
            cf-set-ospeed!
            cf-set-speed!

            tc-get-attr!
            tc-set-attr

            tc-drain
            tc-flow
            tc-flush
            tc-send-break))

(define (get-field-from-termios lst field)
  (let ((res (base:get-field-from-termios lst field)))
    (unless res (throw 'termios/no-such-field field lst))
    res))

(define (put-field-into-termios! lst field value)
  (let ((res (base:put-field-into-termios! lst field value)))
    (unless res (throw 'termios/no-such-field field lst))
    res))

;; The rest of the functions return either 0 (on success) or -1 (on failure).
;; In the case of failure, error-reporting is done via POSIX's ‘errno’
;; facility.

(define (maybe-termios-error fnc res)
  (if (base:termios-failure? res)
      (let ((errno (base:get-errno)))
        (scm-error 'system-error
                   (symbol->string fnc)
                   "~s"
                   (map strerror errno)
                   errno))))

(define-syntax defexcp
  (syntax-rules ()
    ((_ fe be args ...)
     (define (fe args ...)
       (let ((res (be args ...)))
         (maybe-termios-error 'fe res)
         res)))))

(defexcp cf-set-ispeed! base:cf-set-ispeed! termios speed)
(defexcp cf-set-ospeed! base:cf-set-ospeed! termios speed)
(defexcp cf-set-speed! base:cf-set-speed! termios speed)
(defexcp tc-drain base:tc-drain port)
(defexcp tc-flow base:tc-flow port action)
(defexcp tc-flush base:tc-flush port queue-selector)
(defexcp tc-get-attr! base:tc-get-attr! port termios)
(defexcp tc-send-break base:tc-send-break port duration)

(define* (tc-set-attr port termios #:key (optional-action termios-TCSANOW))
  (let ((res (base:tc-set-attr port termios #:optional-action optional-action)))
    (maybe-termios-error 'tc-set-attr res)
    res))