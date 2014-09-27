;; -*- scheme -*-

(use-modules (test tap)
             (test termios)
             (termios)
             (termios system))

(with-test-bundle (guile termios baudrate)
  (plan 5)

  ;; Setup
  (let ((tty (open-device))
        (ts (make-termios-struct)))

    (tc-get-attr! tty ts)
    (define-test "Default baudrate (19200bd) of test device detected"
      (pass-if-= termios-B19200 (cf-get-ispeed ts)))

    ;; Here's what we do, once drawn out in detail:

    ;; Change entry in termios data structure
    (cf-set-speed! ts termios-B115200)
    ;; Transfer changed settings to device
    (tc-set-attr tty ts)
    ;; Make sure, there' something wrong in the structure
    (cf-set-speed! ts termios-B9600)
    ;; Read the current settings back.
    (tc-get-attr! tty ts)
    ;; Test.
    (define-test "Changing baudrate to 115200bd works"
      (pass-if-= termios-B115200 (cf-get-ispeed ts)))

    (cf-set-speed! ts termios-B300)
    (tc-set-attr tty ts)
    (cf-set-speed! ts termios-B9600)
    (tc-get-attr! tty ts)
    (define-test "Changing baudrate to 300bd works"
      (pass-if-= termios-B300 (cf-get-ispeed ts)))

    (cf-set-ispeed! ts termios-B1200)
    (tc-set-attr tty ts)
    (cf-set-ispeed! ts termios-B9600)
    (tc-get-attr! tty ts)
    (define-test "Changing baudrate to 1200bd works (ispeed)"
      (pass-if-= termios-B1200 (cf-get-ispeed ts)))

    (cf-set-ospeed! ts termios-B2400)
    (tc-set-attr tty ts)
    (cf-set-ospeed! ts termios-B9600)
    (tc-get-attr! tty ts)
    (define-test "Changing baudrate to 2400bd works (ospeed)"
      (pass-if-= termios-B2400 (cf-get-ospeed ts)))

    ;; Teardown
    (close-port tty)))
