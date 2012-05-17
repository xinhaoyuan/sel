(define (make-plist-writer)
  (let ((level-stack (deque (deque))))

    (let ((write!
           (lambda (data)
             ((level-stack 'head) 'push-tail! data)
             ))

          (set-tail!
           (lambda (data)
             ((level-stack 'head) 'set-tail! data)
             ))

          (push-level!
           (lambda ()
             (level-stack 'push-head! (deque))))

          (pop-level!
           (lambda ()
             (let ((temp (level-stack 'head)))
               (level-stack 'pop-head!)
               ((level-stack 'head) 'push-tail! (temp 'plist))
               )))

          (pop-level-expand!
           (lambda ()
             (let ((temp (level-stack 'head)))
               (level-stack 'pop-head!)
               ((level-stack 'head) 'merge-tail! temp)
               )))
          
          (pop-back!
           (lambda ()
             (let ((temp (level-stack 'head)))
               (level-stack 'pop-head!)
               (temp 'plist)
               )))

          (finish!
           (lambda ()
             (let loop ()
               (let ((result (level-stack 'head)))
                 
                 (level-stack 'pop-head!)
                 (if (level-stack 'empty?)
                     (begin
                       ;; reset
                       (set! level-stack (deque (deque)))
                       (result 'plist))
                     
                     (begin
                       ((level-stack 'head) 'push-tail! (result 'plist))
                       (loop))
                     ))))
           )

          (top-level?
           (lambda ()
             (level-stack 'dump
                          (lambda (size head tail)
                            (eq? size 1)))))

          )

      (lambda (op . args)
        (cond
         ((eq? op 'write!)            (for-each write! args))
         ((eq? op 'set-tail!)         (apply set-tail! args))
         ((eq? op 'push-level!)       (apply push-level! args))
         ((eq? op 'pop-level!)        (apply pop-level! args))
         ((eq? op 'pop-level-expand!) (apply pop-level-expand! args))
         ((eq? op 'pop-back!)         (apply pop-back! args))
         ((eq? op 'finish!)           (apply finish! args))
         ((eq? op 'top-level?)        (apply top-level? args))
         (else
          (display "plist-writer : unknown operation - ")
          (display op) (newline))
         ))
      
      )))

(define (make-plist-reader init-data)
  (let ((level-stack (deque init-data)))

    (let ((tail?
           (lambda () (not (pair? (level-stack 'head)))))

          (read
           (lambda ()
             (let ((result (level-stack 'head)))

               (if (pair? result)
                   (car result)
                   result)
               )))

          (next!
           (lambda ()
             (level-stack 'set-head! (cdr (level-stack 'head)))
             ))

          (push-level!
           (lambda ()
             (level-stack 'push-head!
                          (if (pair? (level-stack 'head))
                              (car (level-stack 'head))
                              (level-stack 'head)))
             ))

          (pop-level!
           (lambda ()
             (level-stack 'pop-head!)))

          (top-level?
           (lambda ()
             (level-stack 'dump
                          (lambda (size head tail)
                            (eq? size 1)))))
          )

      (lambda (op . args)

        (cond

         ((eq? op 'tail?) (apply tail? args))
         ((eq? op 'read) (apply read args))
         ((eq? op 'move-next) (apply next! args))
         ((eq? op 'push-level!) (apply push-level! args))
         ((eq? op 'pop-level!) (apply pop-level! args))
         ((eq? op 'top-level?) (apply top-level? args))

         (else
          (display "plist-reader : unknown operation - ")
          (display op) (newline))
         ))
      )))


(define (unit-test-plist)
  ;; test for list-writer

  (let ((foo (make-plist-writer)))
    
    (foo 'write! 1)
    (foo 'write! 2)
    (foo 'push-level!)
    (foo 'write! 3)
    (foo 'write! 4)
    (foo 'pop-level!)
    (foo 'write! 5)
    (display (foo 'finish!)) (newline)

    ;; test for list-reader

    (set! foo (make-plist-reader '(1 2 3 (4 5) 6)))
    (display (foo 'read))(foo 'move-next)
    (display (foo 'read))(foo 'move-next)
    (display (foo 'read))(foo 'move-next)
    (display (foo 'read))(newline)
    (foo 'push-level!)
    (display (foo 'read))(foo 'move-next)
    (display (foo 'read))(foo 'move-next)
    (newline)
    (foo 'pop-level!)
    (foo 'move-next)
    (display (foo 'read))(foo 'move-next)
    ))
