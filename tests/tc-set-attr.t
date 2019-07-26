;; -*- scheme -*-

;; Copyright (c) 2014-2019 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.

(use-modules (test tap)
             (test termios)
             ((termios) #:prefix base:)
             ((termios with-exceptions) #:prefix ex:)
             (termios system))

(with-test-bundle (guile termios tc-set-attr)
  (plan 9)

  (let ((tty (open-device))
        (ts (base:make-termios-struct)))

    (define-test "Initial base:tc-get-attr! works"
      (pass-if-false (base:termios-failure? (base:tc-get-attr! tty ts))))

    (define-test "tc-set-attr without #:when works"
      (pass-if-false (base:termios-failure? (base:tc-set-attr tty ts))))

    (define-test "tc-set-attr #:when => TCSANOW works"
      (pass-if-false (base:termios-failure?
                      (base:tc-set-attr tty ts #:when termios-TCSANOW))))

    (define-test "tc-set-attr #:when => TCSADRAIN works"
      (pass-if-false (base:termios-failure?
                      (base:tc-set-attr tty ts #:when termios-TCSADRAIN))))

    (define-test "tc-set-attr #:when => TCSAFLUSH works"
      (pass-if-false (base:termios-failure?
                      (base:tc-set-attr tty ts #:when termios-TCSAFLUSH))))

    (define-test "tc-set-attr without #:when works (with-exceptions)"
      (pass-if-no-exception (ex:tc-set-attr tty ts)))

    (define-test "tc-set-attr #:when => TCSANOW works (with-exceptions)"
      (pass-if-no-exception (base:tc-set-attr tty ts #:when termios-TCSANOW)))

    (define-test "tc-set-attr #:when => TCSADRAIN works (with-exceptions)"
      (pass-if-no-exception (base:tc-set-attr tty ts #:when termios-TCSADRAIN)))

    (define-test "tc-set-attr #:when => TCSAFLUSH works (with-exceptions)"
      (pass-if-no-exception (base:tc-set-attr tty ts #:when termios-TCSAFLUSH)))

    (close-port tty)))
