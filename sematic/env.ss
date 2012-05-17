;; Simple data structure for lexical bindings

(define (binding name meta value)
  (vector name meta value))
(define (binding-name binding)
  (vector-ref binding 0))
(define (binding-meta binding)
  (vector-ref binding 1))
(define (binding-value binding)
  (vector-ref binding 2))

(define (make-env) (list '()))

(define (env-level-add env)
  (cons '() env))

(define (env-binding-add! env name meta value)
  (set-car! env (cons (binding name meta value) (car env))))

(define (level-lookup level lookup)
  (and (pair? level)
       (or (lookup (car level)) (level-lookup (cdr level) lookup))))

(define (env-lookup env name)
  (and (symbol? name)
       (let lookup-recur ((current env))
         (and (pair? current)
              (or (level-lookup (car current) (lambda (b) (and (eq? (binding-name b) name) b)))
                  (lookup-recur (cdr current)))))))

(define (env-lookup-in-class env name meta)
  (and (symbol? name)
       (let lookup-recur ((current env))
         (and (pair? current)
              (or (level-lookup (car current) (lambda (b) (and (eq? (binding-meta b) meta) (eq? (binding-name b) name) b)))
                  (lookup-recur (cdr current)))))))

(define (unit-test-env)
  (let ((env (make-env)))

    (env-binding-add! env 'a 'variable (cons 0 1))
    (env-binding-add! env 'a 'rule 'exit)
    (env-binding-add! env 'b 'variable (cons 2 3))

    (display (env-lookup          env 'a)) (newline)
    (display (env-lookup-in-class env 'a 'variable)) (newline)
    (display (env-lookup-in-class env 'a 'rule)) (newline)
    (display (env-lookup          env 'b)) (newline)
    (display (env-lookup-in-class env 'b 'rule)) (newline)

    (set! env (env-level-add env))
    
    (env-binding-add! env 'a 'variable (cons 5 6))
    (env-binding-add! env 'b 'variable (cons 4 5))

    (display (env-lookup          env 'a)) (newline)
    (display (env-lookup-in-class env 'a 'variable)) (newline)
    (display (env-lookup-in-class env 'a 'rule)) (newline)
    (display (env-lookup          env 'b)) (newline)
    (display (env-lookup-in-class env 'b 'rule)) (newline)
    
    ))
