;; -*- scheme -*-

;; Copyright (c) 2014 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.

(use-modules (test tap)
             (test termios)
             ((termios) #:prefix base:)
             ((termios with-exceptions) #:prefix ex:)
             (termios system))

(define termios-fields (@@ (termios) termios-fields))

(define (get-item x)
  (if (list? x) (car x) x))

(with-test-bundle (guile termios struct termios)
  (plan (+ 4
           (* 3 (length termios-fields))))
  (let* ((ts (base:make-termios-struct))
         (lst (base:parse-termios-struct ts))
         (n 23)
         (l (list 23 1 2 3)))
    ;; We don't mind if we break the scheme list here while testing, because we
    ;; don't put it back into C-land by calling:
    ;;
    ;;   (make-termios-struct lst)
    ;;
    ;; Otherwise, putting an interger in place of a list (in case of the ‘c-cc’
    ;; entry) is not a great idea.
    (for-each (lambda (x)
                (tap/comment "--- --- ---")
                (let ((cur (base:get-field-from-termios lst x)))
                  (define-test (format #f "get-field ~a" x)
                    (pass-if-= 0 (get-item cur)))
                  (define-test (format #f "put-field ~a: ~a" x n)
                    (pass-if-true (base:put-field-into-termios! lst x n)))
                  (define-test (format #f "get-field ~a (n := ~a)" x n)
                    (pass-if-= n (base:get-field-from-termios lst x))))
                (set! n (+ 1 n)))
              termios-fields))
  (tap/comment "--- --- ---")
  (let* ((ts (base:make-termios-struct))
         (lst (base:parse-termios-struct ts)))
    (define-test "get-field: non-existent field fails"
      (pass-if-false (base:get-field-from-termios lst 'does-not-exist)))
    (define-test "put-field: non-existent field fails"
      (pass-if-false (base:put-field-into-termios! lst 'does-not-exist 123)))
    (define-test "get-field: non-existent field fails (with-exceptions)"
      (pass-if-exception 'termios/no-such-field
          (ex:get-field-from-termios lst 'does-not-exist)))
    (define-test "put-field: non-existent field fails (with-exceptions)"
      (pass-if-exception 'termios/no-such-field
          (ex:put-field-into-termios! lst 'does-not-exist 123)))))
