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


(define (evaluate expr env gas)
  (begin
    (gas 1)
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
       (if (evaluate test env gas)
           (evaluate consequent env gas)
           (evaluate alternate env gas)))
      ;; Lambdas (Procedures)
      (`(lambda ,(cons arg args) ,body)
       (lambda (vals ...)
         (evaluate body (extend-env env (cons arg args) vals) gas)))
      ;; Procedure Invocation (Application)
      ((cons proc-expr arg-exprs)
       (apply (evaluate proc-expr env gas)
              (map (lambda (arg-expr)
                     (evaluate arg-expr env gas))
                   arg-exprs))))))
                     
(define math-env
  `((+ . ,+)
    (- . ,-)
    (* . ,*)
    (/ . ,/)))

(define (create-gas-allowance limit)
  (begin
    (define remaining limit)
    (lambda (charge)
      (begin
        (if (> 1 remaining) (error "Gas limit exceeded") '())
        (set! remaining (- remaining charge))))))

(evaluate '(+ 1 2)
                math-env (create-gas-allowance 10))
(evaluate '(- 2 3)
                math-env (create-gas-allowance 10))
(evaluate '(* 2 3)
                math-env (create-gas-allowance 10))
(evaluate '(/ 2 3)
                math-env (create-gas-allowance 10))
(evaluate '(/ 2 3)
                math-env (create-gas-allowance 10))

(display "big evaluation")
(evaluate '(/ 2 3 (+ 4 5 (- 1 5)))
                math-env (create-gas-allowance 10))
; Error: Gas limit exceeded