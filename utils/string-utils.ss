(define (string-quote string)
  (let ((length (string-length string)))
    (let recur ((current 0)
                (result '(#\")))

      (if (< current length)
          (let ((char (string-ref string current)))
            
            (if (eq? char #\")
                (recur (+ current 1)
                       (cons char (cons #\\  result)))
                (recur (+ current 1)
                       (cons char result))
                ))
          (list->string (reverse (cons #\" result)))
          )))
  )

(define (string-escape string)
  (let ((length (string-length string)))
    (let recur ((current 0)
                (result '()))

      (if (< current length)
          (let ((char (string-ref string current)))
            
            (if (or (char-alphabetic? char)
                    (char-numeric? char))
                (recur (+ current 1)
                       (cons char result))
                (recur (+ current 1)
                       (cons #\_ result))
                ))
          (list->string (reverse result))
          ))))

(define (serialize-to-string input)
  (cond
   ((symbol? input)
    (symbol->string input))
   
   ((number? input)
    (number->string input))
   
   ((string? input) input)
   
   ((pair? input)
     (let ((a (serialize-to-string (car input)))
           (b (serialize-to-string (cdr input))))
       
       (string-append a b)
       ))
   
   (else "")))
