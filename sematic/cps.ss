;; Continuation Passing Style Converting
;; Requires exp.ss, sys.ss (for testing)

(define (cps-eval ctx exps id cont)
  (let ((type (exp-type exps id)))
    
    (cond
     
     ((or (eq? type EXP_TYPE_GOTO)
          (eq? type EXP_TYPE_SETANDGOTO))
      id)

     ((or (eq? type EXP_TYPE_CONSTANT)
          (eq? type EXP_TYPE_GLOBALREF)
          (eq? type EXP_TYPE_LOCALREF)
          (eq? type EXP_TYPE_LABEL))

      (exp-goto-new exps 2 (vector cont id)))

     ((eq? type EXP_TYPE_SET)
      (let ((ncont (exp-label-new exps 0 1)))
        (exp-label-body-set!
         exps ncont
         (exp-setandgoto-new exps
                             (exp-set-ref-get exps id)
                             (exp-localref-new exps ncont 0) cont))
        (cps-eval ctx exps (exp-set-val-get exps id) ncont)))

     ((eq? type EXP_TYPE_LAMBDA)
      (begin
        (exp-lambda-to-label! exps id)
        (exp-label-argsize-set! exps id (+ (exp-label-argsize-get exps id) 1))
        (let ((inner-cont-id (- (exp-label-argbase-get exps id) 1))
              (inner-cont   EXP_NULL))
          (set! inner-cont (exp-localref-new exps id inner-cont-id))
          (exp-label-argbase-set! exps id inner-cont-id)
          (exp-label-body-set! exps id (cps-eval ctx exps (exp-label-body-get exps id) inner-cont))
          (exp-goto-new exps 2 (vector cont id)))
        ))

     ((eq? type EXP_TYPE_BEGIN)
      (let recur ((cur (exp-begin-list-get exps id))
                  (cur-cont cont))
        (let ((result (cps-eval ctx exps (car cur) cur-cont)))
          (if (pair? (cdr cur))
              (begin
                (set! cur-cont (exp-label-new exps 0 0))
                (exp-label-body-set! exps cur-cont result)
                (recur (cdr cur) cur-cont))
              result))
          ))

     ((eq? type EXP_TYPE_WITH)
      (begin
        (exp-with-body-set!
         exps id
         (cps-eval ctx exps (exp-with-body-get exps id) cont))
        id))
      

     ((eq? type EXP_TYPE_IF)
      (let ((l (exp-label-new exps 0 1)))
        (let ((outer-cont (exp-localref-new exps l 0))
              (inner-cont (exp-label-new    exps 0 1))
              (if-cond    (exp-if-cond-get  exps id)))
          (exp-if-cond-set! exps id (exp-localref-new exps inner-cont 0))
          (exp-if-then-set! exps id (cps-eval ctx exps (exp-if-then-get exps id) outer-cont))
          (exp-if-else-set! exps id (cps-eval ctx exps (exp-if-else-get exps id) outer-cont))
          (exp-label-body-set! exps inner-cont id)
          (exp-label-body-set! exps l (cps-eval ctx exps if-cond inner-cont))
          (exp-goto-new exps 2 (vector l cont))
          )))

     ((eq? type EXP_TYPE_APPLY)
      (let ((argc (exp-apply-argc-get exps id)))
        (let ((w (exp-with-new exps 0 argc))
              (b (exp-begin-new exps))
              (g (exp-goto-new exps (+ argc 1) #f)))

          (let recur ((i 0))
            (if (< i argc)
                (begin
                  (exp-begin-list-push!
                   exps b
                   (exp-set-new exps (exp-localref-new exps w i) (exp-apply-argv-get exps id i)))
                  (exp-apply-argv-set! exps g (if (= i 0) 0 (+ i 1)) (exp-localref-new exps w i))
                  (recur (+ i 1)))))

          (exp-goto-argv-set! exps g 1 cont)
          (exp-begin-list-push! exps b g)
          (exp-with-body-set! exps w (cps-eval ctx exps b cont))

          w)))
     
     ((eq? type EXP_TYPE_CALLCC)
      (let ((l (exp-label-new exps 0 1)))
        (let ((outer-cont (exp-localref-new exps l 0))
              (inner-cont (exp-label-new    exps 0 1)))
          (exp-label-body-set!
           exps inner-cont
           (exp-goto-new exps 3
                         (vector (exp-localref-new exps inner-cont  0)
                                 outer-cont
                                 outer-cont)))
          (exp-label-body-set!
           exps l
           (cps-eval ctx exps (exp-callcc-callee-get exps id) inner-cont))
          (exp-goto-new exps 2 (vector l cont))
          )))
     )))

(define (unit-test-cps)
  (let ((ctx system-ctx))
    
    (let ((exps (make-exp-pool)))
      (display (exp-dump
                exps
                (cps-eval ctx exps (exp-set-new exps (exp-localref-new exps 0 0) (exp-localref-new exps 1 1)) EXP_NULL)))
      (newline))

    (let ((exps (make-exp-pool)))
      (let ((b (exp-begin-new exps))
            (w (exp-with-new  exps 0 2)))
        (exp-begin-list-push! exps b (exp-constant-new exps 'foo "bar")) 
        (exp-begin-list-push! exps b (exp-constant-new exps 'foo2 "bar2"))
        (exp-with-body-set! exps w b)
        (display (exp-dump
                  exps
                  (cps-eval ctx exps w EXP_NULL)))
        (newline)))

    (let ((exps (make-exp-pool)))
      (let ((b (exp-begin-new exps))
            (l (exp-lambda-new  exps 0 2)))
        (exp-begin-list-push! exps b (exp-constant-new exps 'foo "bar")) 
        (exp-begin-list-push! exps b (exp-constant-new exps 'foo2 "bar2"))
        (exp-lambda-body-set! exps l b)
        (display (exp-dump
                  exps
                  (cps-eval ctx exps
                            (exp-apply-new exps 2 (vector
                                                   l
                                                   (exp-constant-new exps 'foo "bar")))
                            EXP_NULL)))
        (newline)))

    (let ((exps (make-exp-pool)))
      (display (exp-dump
                exps
                (cps-eval ctx exps
                          (exp-if-new exps
                                      (exp-localref-new exps 0 0)
                                      (exp-localref-new exps 1 1)
                                      (exp-constant-new exps 'foo "bar"))
                          EXP_NULL)))
      (newline))

    (let ((exps (make-exp-pool)))
      (display (exp-dump
                exps
                (cps-eval ctx exps (exp-callcc-new exps (exp-constant-new exps 'foo "bar")) EXP_NULL)))
      (newline))
    
    ))
