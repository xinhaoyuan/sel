;; convert CPS into C snippets
;; Requires string-utils.ss, plist.ss, exp.ss

(define c2c-head-string
  "\
#include <execution.h>
")
(define c2c-tail-string "")

(define (make-c2c-context) (cons 0 ""))

(define c2c-context-dump cdr)
(define c2c-context-dump-set! set-cdr!)

(define c2c-context-label-start car)
(define c2c-context-label-start-set! set-car!)

(define (quote-constant exps id)
  (let ((type (exp-constant-type-get exps id))
        (value (exp-constant-val-get exps id)))
    (cond
     ((eq? type 'integer)
      (list "SEE_MAKE_INTEGER(execution," value ")"))

     ((eq? type 'number)
      (list "SEE_MAKE_NUMBER(execution," value ")"))

     ((eq? type 'symbol)
      (list "SEE_MAKE_STRING_SYMBOL(execution," (quote-string (symbol->string value)) ")"))

     ((eq? type 'boolean)
      (list "SEE_MAKE_BOOLEAN_" (if value "TRUE" "FALSE") "(execution)"))

     ;; ((pair? value)
     ;;  (let recur ((count 0)
     ;;              (cur value)
     ;;              (result '())
     ;;              )
     ;;    (if (pair? cur)
     ;;        (recur (+ 1 count)
     ;;               (cdr cur)
     ;;               (cons (quote-exp (car cur))
     ;;                     (cons "," result)))
     ;;        (cons "LCONS(execution,"
     ;;              (cons count
     ;;                    (reverse
     ;;                     (cons ")" (cons (quote-exp cur) (cons "," result)))
     ;;                     )))
     ;;        )))

     ;; ((vector? value)
     ;;  (let recur ((idx (vector-length value))
     ;;              (result '(")")))
     ;;    (if (eq? idx 0)
     ;;        (cons (list "VECTOR(execution," (vector-length value)) result)
     ;;        (begin
     ;;          (set! idx (- idx 1))
     ;;          (recur idx
     ;;                 (cons ","
     ;;                       (cons (quote-exp (vector-ref value idx)) result)))
     ;;          ))))

     ;; ((char? value)
     ;;  (list "INTEGER(execution," (char->integer value) ")"))
     
     ;; ((eq? value '())
     ;;  "OBJECT_NULL")

     (else
      (begin (display "unknown value to represent ") (display type) (display " ") (display value) (newline)))
     )))

(define (c2c ctx c2c-context main-name exps id)
  (let ((exp-depth          #f)
        
        (label-count        1)
        (label-define-vec   #f)
        (label-constant-vec #f)
        (label-start (c2c-context-label-start c2c-context))
        (next-offset 1)
        (offset-stack (list 0))
        (current-depth 0)

        (c2c-internal #f)
        )

    (exp-pool-traverse
     exps id
     (lambda (id)
       (if (eq? (exp-type exps id) EXP_TYPE_LABEL)
           (set! label-count (+ label-count 1)))))

    (set! label-define-vec   (make-vector label-count #f))
    (set! label-constant-vec (make-vector label-count #f))
    
    (set! exp-depth (make-vector (flex-vector-length exps) #f))
    (set! c2c-internal
          (lambda (id)
            (if (eq? id EXP_NULL)
                "SEE_OBJECT_NULL"
                (let ((type (exp-type exps id)))
                  (cond

                   ((eq? type EXP_TYPE_CONSTANT)
                    (let ((offset (car offset-stack))
                          (l #f))
                      (set! l (vector-ref label-constant-vec offset))
                      (if (pair? l)
                          (vector-set! label-constant-vec offset
                                       (cons (cons (+ 1 (car (car l))) (quote-constant exps id))
                                             l))
                          (vector-set! label-constant-vec offset
                                       (cons (cons 0 (quote-constant exps id)) l))
                          )
                      (list "constant_" (car (car (vector-ref label-constant-vec offset))))
                      ))

                   ((eq? type EXP_TYPE_GLOBALREF)
                    (list "GLOBALREF(execution," (string-quote (exp-globalref-name-get exps id)) ")")
                    )
                   
                   ((eq? type EXP_TYPE_LOCALREF)
                    (let ((base (exp-localref-base-get exps id))
                          (offset (exp-localref-offset-get exps id)))
                      (list "see_local_ref(execution,"
                            (- current-depth (vector-ref exp-depth base)) ","
                            (- offset
                               (cond
                                ((eq? (exp-type exps base) EXP_TYPE_LABEL)
                                 (exp-label-argbase-get exps base))
                                ((eq? (exp-type exps base) EXP_TYPE_WITH)
                                 (exp-with-argbase-get exps base))
                                (else
                                 (ctx 'error "cannot process base exp for local ref"))))
                            ")"
                            )
                      ))
                   
                   ((eq? type EXP_TYPE_LABEL)
                    (let ((offset (+ next-offset label-start)))
                      (set! offset-stack (cons next-offset offset-stack))
                      (set! next-offset (+ 1 next-offset))
                      (set! current-depth (+ current-depth 1))
                      (vector-set! exp-depth id current-depth)
                      (vector-set! label-define-vec (- offset label-start)
                                   (c2c-internal (exp-label-body-get exps id))
                                   )
                      (set! current-depth (- current-depth 1))
                      (set! offset-stack (cdr offset-stack))
                      (list "see_label_new(execution,label_" offset "," (exp-label-argsize-get exps id) ")")
                      )
                    )

                   ((eq? type EXP_TYPE_WITH)
                    (begin
                      (set! current-depth (+ current-depth 1))
                      (vector-set! exp-depth id current-depth)
                      (let ((result
                             (list "do{if(see_with(execution," (exp-with-argsize-get exps id) ") == 0){\n"
                                   (c2c-internal (exp-with-body-get exps id)) ";}}while(0)")))
                        (set! current-depth (- current-depth 1))
                        result)
                      ))

                   ((eq? type EXP_TYPE_GOTO)
                    (let ((writer (make-plist-writer)))
                      (writer 'push-level!)
                      (writer 'write! "see_goto(execution")
                      
                      (let recur ((i 0))
                        (if (< i (exp-goto-argc-get exps id))
                            (begin
                              (writer 'write! ",")
                              (writer 'write! (c2c-internal (exp-goto-argv-get exps id i)))
                              (recur (+ i 1)))
                            ))

                      (writer 'write! ",NULL)")
                      (car (writer 'finish!)))
                    )

                   ((eq? type EXP_TYPE_SETANDGOTO)
                    (let ((ref (exp-setandgoto-ref-get exps id)))
                      (if (eq? (exp-type exps ref) EXP_TYPE_LOCALREF)
                          (let ((base (exp-localref-base-get exps ref))
                                (offset (exp-localref-offset-get exps ref)))
                            (list "see_set_and_goto(execution,"
                                  (- current-depth (vector-ref exp-depth base)) ","
                                  (- offset
                                     (cond
                                      ((eq? (exp-type exps base) EXP_TYPE_LABEL)
                                       (exp-label-argbase-get exps base))
                                      ((eq? (exp-type exps base) EXP_TYPE_WITH)
                                       (exp-with-argbase-get exps base))
                                      (else
                                       (ctx 'error "cannot process base exp for local ref")))) ","
                                       (c2c-internal (exp-setandgoto-val-get exps id)) ","
                                       (c2c-internal (exp-setandgoto-cont-get exps id)) ")"))
                          (list "see_global_set_and_goto(execution,"
                                (string-quote (exp-globalref-name-get exps ref)) ","
                                (c2c-internal (exp-setandgoto-val-get exps id)) ","
                                (c2c-internal (exp-setandgoto-cont-get exps id)) ")"))))

                   ((eq? type EXP_TYPE_REFANDGOTO)
                    (list "see_global_ref_and_goto(execution,"
                          (string-quote (exp-globalref-name-get exps (exp-refandgoto-ref-get exps id))) ","
                          (c2c-internal (exp-refandgoto-cont-get exps id)) ")"))

                   ((eq? type EXP_TYPE_IF)
                    (list "((IS_TRUE(" (c2c-internal (exp-if-cond-get exps id)) "))?"
                          (c2c-internal (exp-if-then-get exps id)) ":"
                          (c2c-internal (exp-if-else-get exps id)) ")"))

                   (else
                    (display type) (newline)
                    (ctx 'error "unknown exp to compile"))
                   )))
            ))

    (vector-set! label-define-vec 0 (c2c-internal id))

    (c2c-context-label-start-set! c2c-context (+ next-offset label-start))
    (c2c-context-dump-set!
     c2c-context
     (let after-recur ((count (vector-length label-define-vec))
                       (declare-result '())
                       (define-result '()))
       (if (> count 0)
           (after-recur (- count 1)
                        (cons (list "static void label_" (+ count label-start -1) "(see_execution_t execution);\n")
                              declare-result)
                        (cons (list "static void label_" (+ count label-start -1) "(see_execution_t execution) {\n"
                                    ;; process for constants
                                    (let inner-recur ((clist (vector-ref label-constant-vec (+ count -1)))
                                                      (result '())
                                                      )
                                      (if (pair? clist)
                                          (inner-recur (cdr clist)
                                                       (cons

                                                        (list "static see_object_t constant_" (car (car clist)) " = NULL;\n"
                                                              "if (constant_" (car (car clist)) " == NULL) { constant_" (car (car clist))
                                                              " = " (cdr (car clist)) ";}\n")

                                                        result))

                                          result))
                                    
                                    (vector-ref label-define-vec (- count 1)) ";\n}\n")
                              define-result)
                        )
           (serialize-to-string
            (list c2c-head-string "\n"
                  declare-result "\n" define-result "\n"
                  "int " main-name "(void) {
see_object_sys_init();
see_execution_sys_init();
see_execution_t execution = see_execution_new();
see_goto(execution,see_label_new(execution,label_" label-start ",0),NULL);
see_run(execution);
see_execution_free(execution);
return 0;
}"
                  (c2c-context-dump c2c-context)
                  c2c-tail-string
                  )))))
    ))

(define (unit-test-c2c)
  (let ((test (lambda (input)
                (let ((ctx system-ctx)
                      (c2c-ctx (make-c2c-context))
                      (exps (make-exp-pool))
                      (id #f)
                      )
                  (set! id (parse-eval ctx system-env exps input))
                  (display (exp-dump exps id)) (newline) (newline)
                  (set! id (cps-eval ctx exps id EXP_NULL))
                  (display (exp-dump exps id)) (newline) (newline)
                  (c2c ctx c2c-ctx "main" exps id)
                  (c2c-context-dump c2c-ctx)
                  ))))
    
    ;; (display (test '(@lambda (a) 1))) (newline) (newline)
    ;; (display (test '(@lambda (a) (@begin a 1)))) (newline) (newline)
    ;; (display (test '(@set! a b))) (newline) (newline)

    (display (test '(@with (a) (@begin (@set! a (@lambda () (a))) (a)))))
    ))
