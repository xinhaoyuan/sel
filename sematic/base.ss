;; Trival definition for sematic compiling

(define SYSTEM_RULE_SET        '@set!)
(define SYSTEM_RULE_LAMBDA     '@lambda)
(define SYSTEM_RULE_WITH       '@with)
(define SYSTEM_RULE_BEGIN      '@begin)
(define SYSTEM_RULE_IF         '@if)
(define SYSTEM_RULE_APPLY      '@apply)
(define SYSTEM_RULE_CALLCC     '@call/cc)

(define (rule? ctx env s)
  (and (symbol? s)
       (or (eq? s SYSTEM_RULE_SET)
           (eq? s SYSTEM_RULE_LAMBDA)
           (eq? s SYSTEM_RULE_WITH)
           (eq? s SYSTEM_RULE_BEGIN)
           (eq? s SYSTEM_RULE_IF)
           (eq? s SYSTEM_RULE_APPLY)
           (eq? s SYSTEM_RULE_CALLCC)
           (ctx 'rule? env s)
           )))

(define (make-fundamental-context)
  (lambda (op . args)
    (cond
     
     ((eq? op 'error)
      (display (car args)) (newline))

     ((eq? op 'rule?) #f)

     (else
      (begin
        (display "CTX: ") (display op) (display " ") (display args) (newline)
        #f))
     )))
