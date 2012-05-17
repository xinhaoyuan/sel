;; Requires env.ss

(define (make-test-ctx)
  (lambda (op . args)

    (cond
     
     ((eq? op 'error)
      (display (car args)) (newline))

     ((eq? op 'system-rule?) #f)

     (else
      (begin
        (display "CTX: ") (display op) (display " ") (display args) (newline)
        #f))
    )))

(define system-env (make-env))
