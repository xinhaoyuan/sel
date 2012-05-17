(define (call-with-rebinding env input proc)
  (if (vector? input)
      (proc (vector-ref input 0) (vector-ref input 1))
      (proc env input)))

(define (rebinding env input)
  (call-with-rebinding
   '() input
   (lambda (unused input)
     (vector env input))))

(define (rebinding-clean input)
  (if (vector? input)
      (let ((exp (vector-ref input 1)))
        (if (pair? exp)
            (let recur ((cur     exp)
                        (result '()))
              (if (pair? cur)
                  (recur (cdr cur) (cons (rebinding-clean (car cur)) result))
                  (reverse result)))
            exp))
      input))
