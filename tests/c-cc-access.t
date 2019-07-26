;; -*- scheme -*-

;; Copyright (c) 2014-2019 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.

(use-modules (test tap)
             (test termios)
             (termios)
             (termios system))

(with-test-bundle (guile termios c-cc-access)
  (plan 3)
  (let* ((ts (make-termios-struct))
         (scm (parse-termios-struct ts)))
    (define-test "Get VSUSP from fresh data structure"
      (pass-if-true (zero? (get-from-c-cc scm termios-VSUSP))))
    (define-test "Set VSUSP to 7"
      (pass-if-no-exception (put-into-c-cc scm termios-VSUSP 7)))
    (define-test "Get VSUSP from data structure (should be 7 now)"
      (pass-if-= 7 (get-from-c-cc scm termios-VSUSP)))))
