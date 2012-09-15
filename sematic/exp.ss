;; Data representation of expressions
;; Requires flex-vector.ss

(define EXP_TYPE_GOTO             0)
(define EXP_TYPE_SETANDGOTO       1)
(define EXP_TYPE_REFANDGOTO       2)
(define EXP_TYPE_CONSTANT         3)
(define EXP_TYPE_LOCALREF         4)
(define EXP_TYPE_GLOBALREF        5)
(define EXP_TYPE_LABEL            6)
(define EXP_TYPE_SET              7)
(define EXP_TYPE_LAMBDA           8)
(define EXP_TYPE_BEGIN            9)
(define EXP_TYPE_WITH             10)
(define EXP_TYPE_IF               11)
(define EXP_TYPE_APPLY            12)
(define EXP_TYPE_CALLCC           13)
  
(define EXP_FIELD_TYPE            0)

(define EXP_NULL                  -1)

(define (make-exp-pool) (make-flex-vector))
(define (exp-pool-traverse exps id proc)
  (and (>= id 0) (< id (flex-vector-length exps))
       (let ((type (exp-type exps id)))
         (proc id)
         (cond
          ((eq? type EXP_TYPE_GOTO)
           (let recur ((i 0))
             (if (< i (exp-goto-argc-get exps id))
                 (begin
                   (exp-pool-traverse exps (exp-goto-argv-get exps id i) proc)
                   (recur (+ i 1))))
             ))

         ((eq? type EXP_TYPE_SETANDGOTO)
          (exp-pool-traverse exps (exp-setandgoto-ref-get exps id) proc)
          (exp-pool-traverse exps (exp-setandgoto-val-get exps id) proc)
          (exp-pool-traverse exps (exp-setandgoto-cont-get exps id) proc) 
          )

         ((eq? type EXP_TYPE_REFANDGOTO)
          (exp-pool-traverse exps (exp-refandgoto-ref-get exps id) proc)
          (exp-pool-traverse exps (exp-refandgoto-cont-get exps id) proc) 
          )

         ((eq? type EXP_TYPE_LABEL)
          (exp-pool-traverse exps (exp-label-body-get exps id) proc))
         
         ((eq? type EXP_TYPE_SET)
          (exp-pool-traverse exps (exp-set-ref-get exps id) proc)
          (exp-pool-traverse exps (exp-set-val-get exps id) proc)
          )

         ((eq? type EXP_TYPE_LAMBDA)
          (exp-pool-traverse exps (exp-lambda-body-get exps id) proc)
          )

         ((eq? type EXP_TYPE_BEGIN)
          (let recur ((cur (exp-begin-list-get exps id)))
            (if (pair? cur)
                (begin
                  (exp-pool-traverse exps (car cur) proc)
                  (recur (cdr cur))))))

         ((eq? type EXP_TYPE_WITH)
          (exp-pool-traverse exps (exp-with-body-get exps id) proc)
          )

         ((eq? type EXP_TYPE_IF)
          (exp-pool-traverse exps (exp-if-cond-get exps id) proc)
          (exp-pool-traverse exps (exp-if-then-get exps id) proc)
          (exp-pool-traverse exps (exp-if-else-get exps id) proc)
          )

         ((eq? type EXP_TYPE_APPLY)
          (let recur ((i 0))
            (if (< i (exp-apply-argc-get exps id))
                (begin
                  (exp-pool-traverse exps (exp-apply-argv-get exps id i) proc)
                  (recur (+ i 1)))
                )))

         ((eq? (exp-type exps id) EXP_TYPE_CALLCC)
          (exp-pool-traverse exps (exp-callcc-callee-get exps id) proc)
          )
         ))))

(define (exp-type exps id)
  (vector-ref (flex-vector-ref exps id) EXP_FIELD_TYPE))
(define (exp-new exps exp)
  (flex-vector-push! exps exp))

(define EXP_FIELD_GOTO_ARGC       1)
(define EXP_FIELD_GOTO_ARGS       2)

(define (exp-goto-new exps argc argv)
  (exp-new exps (vector EXP_TYPE_GOTO argc (or argv (make-vector argc EXP_NULL)))))
(define (exp-goto-argc-get exps id)
  (vector-ref (flex-vector-ref exps id) EXP_FIELD_GOTO_ARGC))
(define (exp-goto-argc-set! exps id argc)
  (vector-set (flex-vector-ref exps id) EXP_FIELD_GOTO_ARGC argc))
(define (exp-goto-argv-get exps id index)
  (vector-ref
   (vector-ref (flex-vector-ref exps id) EXP_FIELD_GOTO_ARGS) index))
(define (exp-goto-argv-set! exps id index arg)
  (vector-set!
   (vector-ref (flex-vector-ref exps id) EXP_FIELD_GOTO_ARGS) index arg))

(define EXP_FIELD_SETANDGOTO_REF  1)
(define EXP_FIELD_SETANDGOTO_VAL  2)
(define EXP_FIELD_SETANDGOTO_CONT 3)

(define (exp-setandgoto-new exps ref val cont)
  (exp-new exps (vector EXP_TYPE_SETANDGOTO ref val cont)))
(define (exp-setandgoto-ref-get exps id)
  (vector-ref (flex-vector-ref exps id) EXP_FIELD_SETANDGOTO_REF))
(define (exp-setandgoto-ref-set! exps id ref)
  (vector-set! (flex-vector-ref exps id) EXP_FIELD_SETANDGOTO_REF ref))
(define (exp-setandgoto-val-get exps id)
  (vector-ref (flex-vector-ref exps id) EXP_FIELD_SETANDGOTO_VAL))
(define (exp-setandgoto-val-set! exps id val)
  (vector-set! (flex-vector-ref exps id) EXP_FIELD_SETANDGOTO_VAL val))
(define (exp-setandgoto-cont-get exps id)
  (vector-ref (flex-vector-ref exps id) EXP_FIELD_SETANDGOTO_CONT))
(define (exp-setandgoto-cont-set! exps id cont)
  (vector-set! (flex-vector-ref exps id) EXP_FIELD_SETANDGOTO_CONT cont))

(define EXP_FIELD_REFANDGOTO_REF  1)
(define EXP_FIELD_REFANDGOTO_CONT 2)

(define (exp-refandgoto-new exps ref cont)
  (exp-new exps (vector EXP_TYPE_REFANDGOTO ref cont)))
(define (exp-refandgoto-ref-get exps id)
  (vector-ref (flex-vector-ref exps id) EXP_FIELD_REFANDGOTO_REF))
(define (exp-refandgoto-ref-set! exps id ref)
  (vector-set! (flex-vector-ref exps id) EXP_FIELD_REFANDGOTO_REF ref))
(define (exp-refandgoto-cont-get exps id)
  (vector-ref (flex-vector-ref exps id) EXP_FIELD_REFANDGOTO_CONT))
(define (exp-refandgoto-cont-set! exps id cont)
  (vector-set! (flex-vector-ref exps id) EXP_FIELD_REFANDGOTO_CONT cont))

(define EXP_FIELD_CONSTANT_TYPE   1)
(define EXP_FIELD_CONSTANT_VAL    2)

(define (exp-constant-new exps type val)
  (exp-new exps (vector EXP_TYPE_CONSTANT type val)))
(define (exp-constant-type-get exps id)
  (vector-ref (flex-vector-ref exps id) EXP_FIELD_CONSTANT_TYPE))
(define (exp-constant-val-get exps id)
  (vector-ref (flex-vector-ref exps id) EXP_FIELD_CONSTANT_VAL))

(define EXP_FIELD_LOCALREF_BASE     1)
(define EXP_FIELD_LOCALREF_OFFSET   2)

(define (exp-localref-new exps base offset)
  (exp-new exps (vector EXP_TYPE_LOCALREF base offset)))
(define (exp-localref-base-get exps id)
  (vector-ref (flex-vector-ref exps id) EXP_FIELD_LOCALREF_BASE))
(define (exp-localref-base-set! exps id base)
  (vector-set! (flex-vector-ref exps id) EXP_FIELD_LOCALREF_BASE base))
(define (exp-localref-offset-get exps id)
  (vector-ref (flex-vector-ref exps id) EXP_FIELD_LOCALREF_OFFSET))
(define (exp-localref-offset-set! exps id offset)
  (vector-set! (flex-vector-ref exps id) EXP_FIELD_LOCALREF_OFFSET offset))

(define EXP_FIELD_GLOBALREF_NAME     1)

(define (exp-globalref-new exps name)
  (exp-new exps (vector EXP_TYPE_GLOBALREF name)))
(define (exp-globalref-name-get exps id)
  (vector-ref (flex-vector-ref exps id) EXP_FIELD_GLOBALREF_NAME))
(define (exp-globalref-name-set! exps id name)
  (vector-set! (flex-vector-ref exps id) EXP_FIELD_GLOBALREF_NAME name))

(define EXP_FIELD_LABEL_ARGBASE   1)
(define EXP_FIELD_LABEL_ARGSIZE   2)
(define EXP_FIELD_LABEL_BODY      3)

(define (exp-label-new exps argbase argsize)
  (exp-new exps (vector EXP_TYPE_LABEL argbase argsize EXP_NULL)))
(define (exp-label-argbase-get exps id)
  (vector-ref (flex-vector-ref exps id) EXP_FIELD_LABEL_ARGBASE))
(define (exp-label-argbase-set! exps id argbase)
  (vector-set! (flex-vector-ref exps id) EXP_FIELD_LABEL_ARGBASE argbase))
(define (exp-label-argsize-get exps id)
  (vector-ref (flex-vector-ref exps id) EXP_FIELD_LABEL_ARGSIZE))
(define (exp-label-argsize-set! exps id argsize)
  (vector-set! (flex-vector-ref exps id) EXP_FIELD_LABEL_ARGSIZE argsize))
(define (exp-label-body-get exps id)
  (vector-ref (flex-vector-ref exps id) EXP_FIELD_LABEL_BODY))
(define (exp-label-body-set! exps id body)
  (vector-set! (flex-vector-ref exps id) EXP_FIELD_LABEL_BODY body))

(define EXP_FIELD_SET_REF         1)
(define EXP_FIELD_SET_VAL         2)

(define (exp-set-new exps ref val)
  (exp-new exps (vector EXP_TYPE_SET ref val)))
(define (exp-set-ref-get exps id)
  (vector-ref (flex-vector-ref exps id) EXP_FIELD_SET_REF))
(define (exp-set-ref-set! exps id ref)
  (vector-set! (flex-vector-ref exps id) EXP_FIELD_SET_REF ref))
(define (exp-set-val-get exps id)
  (vector-ref (flex-vector-ref exps id) EXP_FIELD_SET_VAL))
(define (exp-set-val-set! exps id val)
  (vector-set! (flex-vector-ref exps id) EXP_FIELD_SET_VAL val))

(define EXP_FIELD_LAMBDA_ARGBASE  1)
(define EXP_FIELD_LAMBDA_ARGSIZE  2)
(define EXP_FIELD_LAMBDA_BODY     3)

(define (exp-lambda-new exps argbase argsize)
  (exp-new exps (vector EXP_TYPE_LAMBDA argbase argsize EXP_NULL)))
(define (exp-lambda-to-label! exps id)
  (vector-set! (flex-vector-ref exps id) EXP_FIELD_TYPE EXP_TYPE_LABEL))
(define (exp-lambda-argbase-get exps id)
  (vector-ref (flex-vector-ref exps id) EXP_FIELD_LAMBDA_ARGBASE))
(define (exp-lambda-argbase-set! exps id argbase)
  (vector-set! (flex-vector-ref exps id) EXP_FIELD_LAMBDA_ARGBASE argbase))
(define (exp-lambda-argsize-get exps id)
  (vector-ref (flex-vector-ref exps id) EXP_FIELD_LAMBDA_ARGSIZE))
(define (exp-lambda-argsize-set! exps id argsize)
  (vector-set! (flex-vector-ref exps id) EXP_FIELD_LAMBDA_ARGSIZE argsize))
(define (exp-lambda-body-get exps id)
  (vector-ref (flex-vector-ref exps id) EXP_FIELD_LAMBDA_BODY))
(define (exp-lambda-body-set! exps id body)
  (vector-set! (flex-vector-ref exps id) EXP_FIELD_LAMBDA_BODY body))

(define EXP_FIELD_BEGIN_LIST      1)

(define (exp-begin-new exps)
  (exp-new exps (vector EXP_TYPE_BEGIN '())))
(define (exp-begin-list-push! exps id exp)
  (let ((b (flex-vector-ref exps id)))
    (vector-set! b EXP_FIELD_BEGIN_LIST (cons exp (vector-ref b EXP_FIELD_BEGIN_LIST)))))
(define (exp-begin-list-get exps id)
  (vector-ref (flex-vector-ref exps id) EXP_FIELD_BEGIN_LIST))

(define EXP_FIELD_WITH_ARGBASE    1)
(define EXP_FIELD_WITH_ARGSIZE    2)
(define EXP_FIELD_WITH_BODY       3)

(define (exp-with-new exps argbase argsize)
  (exp-new exps (vector EXP_TYPE_WITH argbase argsize EXP_NULL)))
(define (exp-with-argbase-get exps id)
  (vector-ref (flex-vector-ref exps id) EXP_FIELD_WITH_ARGBASE))
(define (exp-with-argbase-set! exps id argbase)
  (vector-set! (flex-vector-ref exps id) EXP_FIELD_WITH_ARGBASE argbase))
(define (exp-with-argsize-get exps id)
  (vector-ref (flex-vector-ref exps id) EXP_FIELD_WITH_ARGSIZE))
(define (exp-with-argsize-set! exps id argsize)
  (vector-set! (flex-vector-ref exps id) EXP_FIELD_WITH_ARGSIZE argsize))
(define (exp-with-body-get exps id)
  (vector-ref (flex-vector-ref exps id) EXP_FIELD_WITH_BODY))
(define (exp-with-body-set! exps id body)
  (vector-set! (flex-vector-ref exps id) EXP_FIELD_WITH_BODY body))

(define EXP_FIELD_IF_COND         1)
(define EXP_FIELD_IF_THEN         2)
(define EXP_FIELD_IF_ELSE         3)

(define (exp-if-new exps c t e)
  (exp-new exps (vector EXP_TYPE_IF c t e)))
(define (exp-if-cond-get exps id)
  (vector-ref (flex-vector-ref exps id) EXP_FIELD_IF_COND))
(define (exp-if-cond-set! exps id cond)
  (vector-set! (flex-vector-ref exps id) EXP_FIELD_IF_COND cond))
(define (exp-if-then-get exps id)
  (vector-ref (flex-vector-ref exps id) EXP_FIELD_IF_THEN))
(define (exp-if-then-set! exps id then)
  (vector-set! (flex-vector-ref exps id) EXP_FIELD_IF_THEN then))
(define (exp-if-else-get exps id)
  (vector-ref (flex-vector-ref exps id) EXP_FIELD_IF_ELSE))
(define (exp-if-else-set! exps id else)
  (vector-set! (flex-vector-ref exps id) EXP_FIELD_IF_ELSE else))

(define EXP_FIELD_APPLY_ARGC      1)
(define EXP_FIELD_APPLY_ARGS      2)

(define (exp-apply-new exps argc argv)
  (exp-new exps (vector EXP_TYPE_APPLY argc (if argv argv (make-vector argc EXP_NULL)))))
(define (exp-apply-argc-get exps id)
  (vector-ref (flex-vector-ref exps id) EXP_FIELD_APPLY_ARGC))
(define (exp-apply-argc-set! exps id argc)
  (vector-set (flex-vector-ref exps id) EXP_FIELD_APPLY_ARGC argc))
(define (exp-apply-argv-get exps id index)
  (vector-ref
   (vector-ref (flex-vector-ref exps id) EXP_FIELD_APPLY_ARGS) index))
(define (exp-apply-argv-set! exps id index arg)
  (vector-set!
   (vector-ref (flex-vector-ref exps id) EXP_FIELD_APPLY_ARGS) index arg))

(define EXP_FIELD_CALLCC_CALLEE   1)

(define (exp-callcc-new exps callee)
  (exp-new exps (vector EXP_TYPE_CALLCC callee)))
(define (exp-callcc-callee-get exps id)
  (vector-ref (flex-vector-ref exps id) EXP_FIELD_CALLCC_CALLEE))
(define (exp-callcc-callee-set! exps id callee)
  (vector-set! (flex-vector-ref exps id) EXP_FIELD_CALLCC_CALLEE callee))

(define (exp-dump exps id)
  (if (or (< id 0) (>= id (flex-vector-length exps)))
      "?"
      (let ((result (string-append (number->string id) ":")))
        (cond
         ((eq? (exp-type exps id) EXP_TYPE_GOTO)
          (begin
            (set! result (string-append result "(goto"))
            (let recur ((i 0))
              (if (< i (exp-goto-argc-get exps id))
                  (begin
                    (set! result (string-append
                                  result " "
                                  (exp-dump exps (exp-goto-argv-get exps id i))))
                    (recur (+ i 1)))
                  (set! result (string-append result ")"))))
            ))

         ((eq? (exp-type exps id) EXP_TYPE_SETANDGOTO)
          (set! result
                (string-append
                 result
                 "(set-and-goto! "
                 (exp-dump exps (exp-setandgoto-ref-get exps id)) " "
                 (exp-dump exps (exp-setandgoto-val-get exps id)) " "
                 (exp-dump exps (exp-setandgoto-cont-get exps id)) ")"
                 )))

         ((eq? (exp-type exps id) EXP_TYPE_REFANDGOTO)
          (set! result
                (string-append
                 result
                 "(ref-and-goto "
                 (exp-dump exps (exp-refandgoto-ref-get exps id)) " "
                 (exp-dump exps (exp-refandgoto-cont-get exps id)) ")"
                 )))

         ((eq? (exp-type exps id) EXP_TYPE_CONSTANT)
          (set! result (string-append
                        result "[constant]")))

         ((eq? (exp-type exps id) EXP_TYPE_LOCALREF)
          (set! result (string-append
                        result
                        "[localref:"
                        (number->string (exp-localref-base-get exps id)) ","
                        (number->string (exp-localref-offset-get exps id)) "]"
                        )))
         
         ((eq? (exp-type exps id) EXP_TYPE_GLOBALREF)
          (set! result (string-append
                        result
                        "[globalref:"
                        (exp-globalref-name-get exps id) "]"
                        )))

         ((eq? (exp-type exps id) EXP_TYPE_LABEL)
          (set! result (string-append
                        result
                        "(label ["
                        (number->string (exp-label-argbase-get exps id)) ":"
                        (number->string (exp-label-argsize-get exps id)) "] "
                        (exp-dump exps (exp-label-body-get exps id)) ")")))

         ((eq? (exp-type exps id) EXP_TYPE_SET)
          (set! result
                (string-append
                 result
                 "(set! "
                 (exp-dump exps (exp-set-ref-get exps id)) " "
                 (exp-dump exps (exp-set-val-get exps id)) ")"
                 )))

         ((eq? (exp-type exps id) EXP_TYPE_LAMBDA)
          (set! result (string-append
                        result
                        "(lambda ["
                        (number->string (exp-lambda-argbase-get exps id)) ":"
                        (number->string (exp-lambda-argsize-get exps id)) "] "
                        (exp-dump exps (exp-lambda-body-get exps id)) ")")))

         ((eq? (exp-type exps id) EXP_TYPE_BEGIN)
          (begin
            (set! result (string-append
                          result "(begin"))
            (let recur ((cur (reverse (exp-begin-list-get exps id))))
              (if (pair? cur)
                  (begin
                    (set! result
                          (string-append
                           result " "
                           (exp-dump exps (car cur))))
                    (recur (cdr cur)))
                  (set! result (string-append result ")"))))))

         ((eq? (exp-type exps id) EXP_TYPE_WITH)
          (set! result (string-append
                        result
                        "(with ["
                        (number->string (exp-with-argbase-get exps id)) ":"
                        (number->string (exp-with-argsize-get exps id)) "] "
                        (exp-dump exps (exp-with-body-get exps id)) ")")))

         ((eq? (exp-type exps id) EXP_TYPE_IF)
          (set! result
                (string-append
                 result
                 "(if "
                 (exp-dump exps (exp-if-cond-get exps id)) " "
                 (exp-dump exps (exp-if-then-get exps id)) " "
                 (exp-dump exps (exp-if-else-get exps id)) ")"
                 )))

         ((eq? (exp-type exps id) EXP_TYPE_APPLY)
          (begin
            (set! result (string-append result "(apply"))
            (let recur ((i 0))
              (if (< i (exp-apply-argc-get exps id))
                  (begin
                    (set! result (string-append
                                  result " "
                                  (exp-dump exps (exp-apply-argv-get exps id i))))
                    (recur (+ i 1)))
                  (set! result (string-append result ")"))))
            ))

         ((eq? (exp-type exps id) EXP_TYPE_CALLCC)
          (set! result (string-append
                        result "(call/cc "
                        (exp-dump exps (exp-callcc-callee-get exps id)) ")")))

         (else (set! result (string-append result "?")))
         )
        result
        )))

(define (unit-test-exp)
  (let ((exps (make-exp-pool)))
    (display (exp-dump exps (exp-goto-new exps 2 (vector EXP_NULL EXP_NULL)))) (newline)
    (display (exp-dump exps (exp-setandgoto-new exps EXP_NULL EXP_NULL EXP_NULL))) (newline)
    (display (exp-dump exps (exp-refandgoto-new exps EXP_NULL EXP_NULL))) (newline)
    (display (exp-dump exps (exp-constant-new exps 'string "string"))) (newline)
    (display (exp-dump exps (exp-localref-new exps EXP_NULL 0))) (newline)
    (display (exp-dump exps (exp-globalref-new exps "global_var"))) (newline)
    (display (exp-dump exps (exp-label-new exps 0 0))) (newline)
    (display (exp-dump exps (exp-set-new exps EXP_NULL EXP_NULL))) (newline)
    (display (exp-dump exps (exp-lambda-new exps 0 0))) (newline)
    (let ((id (exp-begin-new exps)))
      (exp-begin-list-push! exps id 0)
      (exp-begin-list-push! exps id 1)
      (display (exp-dump exps id)) (newline))
    (display (exp-dump exps (exp-with-new exps 0 0))) (newline)    
    (display (exp-dump exps (exp-if-new exps 0 1 2))) (newline)    
    (display (exp-dump exps (exp-apply-new exps 2 #f))) (newline)    
    (display (exp-dump exps (exp-callcc-new exps 0))) (newline)    
    ))
