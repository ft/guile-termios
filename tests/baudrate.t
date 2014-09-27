;; -*- scheme -*-

(use-modules (test tap)
             (test termios)
             ((termios) #:prefix base:)
             ((termios with-exceptions) #:prefix ex:)
             (termios system))

(define (add-base-test tty ts rate reset getter setter)
  (tap/comment "--- --- ---")
  (define-test (format #f "~a works (~a)"
                       (car setter)
                       (car rate))
    (pass-if-false (base:termios-failure? ((cdr setter) ts (cdr rate)))))
  (define-test (format #f "base:tc-set-attr works (~a)"
                       (car rate))
    (pass-if-false (base:termios-failure? (base:tc-set-attr tty ts))))
  (define-test (format #f "~a works (~a) [reset-step]"
                       (car setter)
                       (car reset))
    (pass-if-false (base:termios-failure? ((cdr setter) ts (cdr reset)))))
  (define-test "base:tc-get-attr! works"
    (pass-if-false (base:termios-failure? (base:tc-get-attr! tty ts))))
  (define-test (format #f "~a works (~a)"
                       (car getter)
                       (car rate))
    (pass-if-= (cdr rate) ((cdr getter) ts))))

(define (add-ex-test tty ts rate reset getter setter)
  (tap/comment "--- --- ---")
  (define-test (format #f "~a works (~a)"
                       (car setter)
                       (car rate))
    (pass-if-no-exception ((cdr setter) ts (cdr rate))))
  (define-test (format #f "ex:tc-set-attr works (~a)"
                       (car rate))
    (pass-if-no-exception (ex:tc-set-attr tty ts)))
  (define-test (format #f "~a works (~a) [reset-step]"
                       (car setter)
                       (car reset))
    (pass-if-no-exception ((cdr setter) ts (cdr reset))))
  (define-test "ex:tc-get-attr! works"
    (pass-if-no-exception (ex:tc-get-attr! tty ts)))
  (define-test (format #f "~a works (~a)"
                       (car getter)
                       (car rate))
    (pass-if-= (cdr rate) ((cdr getter) ts))))

(with-test-bundle (guile termios baudrate)
  (plan (+ 2 (* 5 8)))

  ;; Setup
  (let ((tty (open-device))
        (ts (base:make-termios-struct)))

    (define-test "Initial base:tc-get-attr! works"
      (pass-if-false (base:termios-failure? (base:tc-get-attr! tty ts))))
    (define-test "Default baudrate (19200bd) of test device detected"
      (pass-if-= termios-B19200 (base:cf-get-ispeed ts)))

    (add-base-test tty ts
                   (name-value-pair termios-B115200)
                   (name-value-pair termios-B9600)
                   (name-value-pair base:cf-get-ispeed)
                   (name-value-pair base:cf-set-speed!))
    (add-base-test tty ts
                   (name-value-pair termios-B300)
                   (name-value-pair termios-B9600)
                   (name-value-pair base:cf-get-ispeed)
                   (name-value-pair base:cf-set-speed!))
    (add-base-test tty ts
                   (name-value-pair termios-B1200)
                   (name-value-pair termios-B9600)
                   (name-value-pair base:cf-get-ispeed)
                   (name-value-pair base:cf-set-ispeed!))
    (add-base-test tty ts
                   (name-value-pair termios-B2400)
                   (name-value-pair termios-B9600)
                   (name-value-pair base:cf-get-ospeed)
                   (name-value-pair base:cf-set-ospeed!))
    (add-ex-test tty ts
                 (name-value-pair termios-B115200)
                 (name-value-pair termios-B9600)
                 (name-value-pair ex:cf-get-ispeed)
                 (name-value-pair ex:cf-set-speed!))
    (add-ex-test tty ts
                 (name-value-pair termios-B300)
                 (name-value-pair termios-B9600)
                 (name-value-pair ex:cf-get-ispeed)
                 (name-value-pair ex:cf-set-speed!))
    (add-ex-test tty ts
                 (name-value-pair termios-B1200)
                 (name-value-pair termios-B9600)
                 (name-value-pair ex:cf-get-ispeed)
                 (name-value-pair ex:cf-set-ispeed!))
    (add-ex-test tty ts
                 (name-value-pair termios-B2400)
                 (name-value-pair termios-B9600)
                 (name-value-pair ex:cf-get-ospeed)
                 (name-value-pair ex:cf-set-ospeed!))

    ;; Teardown
    (close-port tty)))
