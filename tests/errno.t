;; -*- scheme -*-

;; Copyright (c) 2014 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.

(use-modules (test tap)
             (test termios)
             ((termios) #:prefix base:)
             ((termios with-exceptions) #:prefix ex:))

(with-test-bundle (guile termios errno)
  (plan 2)
  (let ((f (open-io-file "README")))
    (let ((res (call-with-blocked-asyncs
                (lambda ()
                  (base:tc-drain f)
                  (base:get-errno)))))
      (define-test (format #f "errno is not zero (~a: ~a)"
                           res (strerror res))
        (pass-if-false (zero? res))))
    (define-test "with-exceptions: Failure throws system-error"
      (pass-if-exception 'system-error
          (ex:tc-drain f)))
    (close f)))
