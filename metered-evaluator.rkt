#lang racket

(require racket/match)

(define (env-lookup env name)
  (match (assoc name env)
    ((cons key val)
     val)
    (_
     (error "Variable unbound:" name))))

(define (extend-env env names vals)
  (if (eq? names '())
      env
      (cons (cons (car names) (car vals))
            (extend-env env (cdr names) (cdr vals)))))


(define (evaluate expr env limit)
  (begin
    (if (< limit 1) (error "Gas limit exceeded") '())
  (match expr
    ;; Support builtin types
    ((or #t #f (? number?))
     expr)
    ;; Quoting
    (`(quote ,quoted-expr)
     quoted-expr)
    ;; Variable lookup
    ((? symbol? name)
     (env-lookup env name))
    ;; Conditionals
    (`(if ,test ,consequent ,alternate)
     (if (evaluate test env (- limit 1))
         (evaluate consequent env (- limit 1))
         (evaluate alternate env (- limit 1))))
    ;; Lambdas (Procedures)
    (`(lambda ,(cons arg args) ,body)
     (lambda (vals ...)
       (evaluate body (extend-env env (cons arg args) vals) (- limit 1))))
    ;; Procedure Invocation (Application)
    ((cons proc-expr arg-exprs)
     (apply (evaluate proc-expr env (- limit 1))
            (map (lambda (arg-expr)
                   (evaluate arg-expr env (- limit 1)))
                 arg-exprs))))))

(define (create-metered-evaluator limit)
  (lambda (expr env)
    (evaluate expr env limit)))
                     
(define math-env
  `((+ . ,+)
    (- . ,-)
    (* . ,*)
    (/ . ,/)))

(evaluate '(+ 1 2)
                math-env 10)
(evaluate '(- 2 3)
                math-env 10)
(evaluate '(* 2 3)
                math-env 10)
(evaluate '(/ 2 3)
                math-env 10)
(evaluate '(/ 2 3)
                math-env 10)
; Error: Exceeded gas limit