(define (make-flex-vector)
  (vector 0 1 (make-vector 1 '())))

(define (flex-vector-length vec)
  (vector-ref vec 0))

(define (flex-vector-ref vec i)
  (if (< i (vector-ref vec 0))
      (vector-ref (vector-ref vec 2) i)
      '()))

(define (flex-vector-set! vec i v)
  (if (< i (vector-ref vec 0))
      (vector-set! (vector-ref vec 2) i v)
      '()))

(define (flex-vector-push! vec v)
  (if (= (vector-ref vec 0) (vector-ref vec 1))
      (begin
        (vector-set! vec 1 (* (vector-ref vec 1) 2))
        (let ((ovec (vector-ref vec 2))
              (nvec (make-vector (vector-ref vec 1) '())))
          (let recur ((cur 0))
            (if (< cur (vector-ref vec 0))
                (begin
                  (vector-set! nvec cur (vector-ref ovec cur))
                  (recur (+ cur 1)))
                (vector-set! vec 2 nvec)
                )))))
  (let ((result (vector-ref vec 0)))
    (vector-set! (vector-ref vec 2) result v)
    (vector-set! vec 0 (+ result 1))
    result
    ))

(define (unit-test-flex-vector)
  (let ((a (make-flex-vector)))
    (flex-vector-push! a 1)
    (flex-vector-push! a 3)
    (flex-vector-push! a 2)
    (flex-vector-push! a 5)
    (flex-vector-push! a 7)
    (flex-vector-push! a 4)
    (display (vector-ref a 2))))
