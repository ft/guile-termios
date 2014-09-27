;; -*- scheme -*-

(use-modules (test tap)
             (test termios)
             (termios)
             (termios system))

(define (add-base-test tty ts rate reset getter setter)
  (tap/comment "--- --- ---")
  (define-test (format #f "base-test: ~a works (~a)"
                       (car setter)
                       (car rate))
    (pass-if-false (termios-failure? ((cdr setter) ts (cdr rate)))))
  (define-test (format #f "base-test: tc-set-attr works (~a)"
                       (car rate))
    (pass-if-false (termios-failure? (tc-set-attr tty ts))))
  (define-test (format #f "base-test: ~a works (~a) [reset-step]"
                       (car setter)
                       (car reset))
    (pass-if-false (termios-failure? ((cdr setter) ts (cdr reset)))))
  (define-test "base-test: tc-get-attr! works"
    (pass-if-false (termios-failure? (tc-get-attr! tty ts))))
  (define-test (format #f "base-test: ~a works (~a)"
                       (car getter)
                       (car rate))
    (pass-if-= (cdr rate) ((cdr getter) ts))))

(with-test-bundle (guile termios baudrate)
  (plan (+ 2 (* 5 4)))

  ;; Setup
  (let ((tty (open-device))
        (ts (make-termios-struct)))

    (define-test "Initial tc-get-attr! works"
      (pass-if-false (termios-failure? (tc-get-attr! tty ts))))
    (define-test "Default baudrate (19200bd) of test device detected"
      (pass-if-= termios-B19200 (cf-get-ispeed ts)))

    ;; Here's what we do, once drawn out in detail:

    (add-base-test tty ts
                   (name-value-pair termios-B115200)
                   (name-value-pair termios-B9600)
                   (name-value-pair cf-get-ispeed)
                   (name-value-pair cf-set-speed!))
    (add-base-test tty ts
                   (name-value-pair termios-B300)
                   (name-value-pair termios-B9600)
                   (name-value-pair cf-get-ispeed)
                   (name-value-pair cf-set-speed!))
    (add-base-test tty ts
                   (name-value-pair termios-B1200)
                   (name-value-pair termios-B9600)
                   (name-value-pair cf-get-ispeed)
                   (name-value-pair cf-set-ispeed!))
    (add-base-test tty ts
                   (name-value-pair termios-B2400)
                   (name-value-pair termios-B9600)
                   (name-value-pair cf-get-ospeed)
                   (name-value-pair cf-set-ospeed!))


    ;; Teardown
    (close-port tty)))
