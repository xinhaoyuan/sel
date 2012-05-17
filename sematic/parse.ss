;; Requires base.ss, exp.ss, sys.ss(for test), rebinding.ss

(define (parse-eval
         ctx                            ; compiling context
         env                            ; current lexical binding (rules/variables)
         exps                           ; expression pool
         input                          ; input possibly with lexical rebinding
         )
  
  (call-with-rebinding
   env input
   (lambda (env input)
     
     (cond

      ((number? input)
       (exp-constant-new exps 'number input))
      
      ((symbol? input)
       (parse-var-ref ctx env exps input))

      ((pair? input)
       (call-with-rebinding
        env (car input)
        (lambda (head-env head)
          (if (system-rule? ctx head)
              (parse-system-rule ctx env exps head (cdr input))
              (let ((result (env-lookup head-env head)))
                (if (and result (eq? (binding-meta result) 'rule))
                    (parse-eval ctx env exps (apply-rule ctx env (binding-value result) (cdr input)))
                    (parse-system-rule ctx env exps SYSTEM_RULE_APPLY input)))
              ))))

      (else
       (ctx 'error "Unknown expression to parse")))
     
     )))

(define (parse-var-ref
         ctx
         env
         exps
         var-sym)
  
  (let ((result (env-lookup-in-class env var-sym 'variable)))
    (if result
        (exp-localref-new exps (car (binding-value result)) (cdr (binding-value result)))
        (exp-globalref-new exps (symbol->string var-sym)))))
  
(define (parse-system-rule
         ctx
         env
         exps
         head
         input                          ; passed by parse, which not contains redirection
         )

  (cond
   
   ((eq? head SYSTEM_RULE_SET)
    (if (eq? (length input) 2)
        (call-with-rebinding
         env (car input)
         (lambda (head-env head)
           (if (symbol? head)
               (let ((ref (parse-var-ref ctx head-env exps head))
                     (val (parse-eval ctx env exps (car (cdr input)))))
                 (exp-set-new exps ref val))
               (ctx 'error "reference name should be symbol"))))
        (ctx 'error "syntax error on ``set!''")))

   ((eq? head SYSTEM_RULE_LAMBDA)
    (if (eq? (length input) 2)
        (let ((argslist (rebinding-clean (car input)))
              (result EXP_NULL)
              (inner-env (env-level-add env)))

          (set! result (exp-lambda-new exps 0 (length argslist)))
          
          (let recur ((cur argslist)
                      (id  0))
            (if (pair? cur)
                (if (symbol? (car cur))
                    (begin
                      (env-binding-add! inner-env (car cur) 'variable (cons result id))
                      (recur (cdr cur) (+ id 1)))
                    (ctx 'error "arguments must be symbols"))
                (if (not (eq? cur '()))
                    (ctx 'error "arguments should be in a list"))
                ))

          (exp-lambda-body-set! exps result (parse-eval ctx inner-env exps (car (cdr input))))
          result
          )
        (ctx 'error "syntax error on ``lambda''")))

   ((eq? head SYSTEM_RULE_WITH)
    (if (eq? (length input) 2)
        (let ((argslist (rebinding-clean (car input)))
              (result EXP_NULL)
              (inner-env (env-level-add env)))
          
          (set! result (exp-with-new exps 0 (length argslist)))
          
          (let recur ((cur argslist)
                      (id  0))
            (if (pair? cur)
                (if (symbol? (car cur))
                    (begin
                      (env-binding-add! inner-env (car cur) 'variable (cons result id))
                      (recur (cdr cur) (+ id 1)))
                    (ctx 'error "arguments must be symbols"))
                (if (not (eq? cur '()))
                    (ctx 'error "arguments should be in a list"))
                ))
          
          (exp-with-body-set! exps result (parse-eval ctx inner-env exps (car (cdr input))))
          result
          )
        (ctx 'error "syntax error on ``with''")))

   ((eq? head SYSTEM_RULE_BEGIN)
    (let ((result (exp-begin-new exps)))

      (let recur ((cur input))
        (if (pair? cur)
            (begin
              (exp-begin-list-push! exps result
                                    (parse-eval ctx env exps (car cur)))
              (recur (cdr cur)))
            (if (not (eq? cur '()))
                (ctx 'error "syntax error on ``begin''"))))

      result
      
      ))

   ((eq? head SYSTEM_RULE_IF)
    (if (eq? (length input) 3)
        (let ((c (parse-eval ctx env exps (car input)))
              (t (parse-eval ctx env exps (car (cdr input))))
              (e (parse-eval ctx env exps (car (cdr (cdr input))))))
          (exp-if-new exps c t e))
        (ctx 'error "syntax error on ``if''")))

   ((eq? head SYSTEM_RULE_APPLY)
    (if (> (length input) 0)
        (let recur ((i input)
                    (o '())
                    (c 0))
          (if (pair? i)
              (recur (cdr i) (cons (parse-eval ctx env exps (car i)) o) (+ c 1))
              (if (eq? i '())
                  (recur (exp-apply-new exps c #f) o c)
                  (if (eq? c 0)
                      i
                      (begin
                        (exp-apply-argv-set! exps i (- c 1) (car o))
                        (recur i (cdr o) (- c 1)))))))
                
          
        (ctx 'error "syntax error on ``apply''")))

   ((eq? head SYSTEM_RULE_CALLCC)
    (if (eq? (length input) 1)
        (let ((callee (parse-eval ctx env exps (car input))))
          (exp-callcc-new exps callee))
        (ctx 'error "syntax error on ``call/cc''")))

   (else
    (ctx 'parse-system-rule env exps head input))
   ))

(define (unit-test-parse)

  (let ((input '(@lambda (a) (@set! a b)))
        (exps   (make-exp-pool)))
    (display (exp-dump exps (parse-eval (make-test-ctx) system-env exps input)))
    (newline))

  (let ((input '(@with (a) (a b)))
        (exps   (make-exp-pool)))
    (display (exp-dump exps (parse-eval (make-test-ctx) system-env exps input)))
    (newline))

  (let ((input '(@lambda (a) (@if a b c)))
        (exps   (make-exp-pool)))
    (display (exp-dump exps (parse-eval (make-test-ctx) system-env exps input)))
    (newline))

  (let ((input '(@begin (a) (@if a b c)))
        (exps   (make-exp-pool)))
    (display (exp-dump exps (parse-eval (make-test-ctx) system-env exps input)))
    (newline))
  
  )
