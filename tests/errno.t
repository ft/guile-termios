;; -*- scheme -*-

;; Copyright (c) 2014-2021 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.

(use-modules (srfi srfi-11)
             (test tap)
             (test termios)
             ((termios) #:prefix base:)
             ((termios with-exceptions) #:prefix ex:))

(with-test-bundle (guile termios errno)
  (plan 2)
  (let ((f (open-io-file "README")))
    (let-values (((value errno) (base:tc-drain f)))
      (define-test (format #f "base: Return value indicates error [~a]"
                           (strerror errno))
        (pass-if-true (base:termios-failure? value))))
    (define-test "with-exceptions: Failure throws system-error"
      (pass-if-exception 'system-error (ex:tc-drain f)))
    (close f)))
