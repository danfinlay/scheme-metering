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

(define (evaluate expr env)
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
     (if (evaluate test env)
         (evaluate consequent env)
         (evaluate alternate env)))
    ;; Lambdas (Procedures)
    (`(lambda ,(cons arg args) ,body)
     (lambda (vals ...)
       (evaluate body (extend-env env (cons arg args) vals))))
    ;; Procedure Invocation (Application)
    ((cons proc-expr arg-exprs)
     (apply (evaluate proc-expr env)
            (map (lambda (arg-expr)
                   (evaluate arg-expr env))
                 arg-exprs)))))

(define (create-meter limit)
  (begin
    (define remaining limit)

    ; Per method attenuator:
    (lambda (target cost)
      (begin
        (lambda (first . rest)
          (begin
            (set! remaining (- remaining cost))
            (display (string-append "remaining gas " (number->string (+ remaining 1)) "\n"))
            (if (> remaining -1)
              (apply target (cons first rest))
              (error "Exceeded gas limit"))))))))


(define meter (create-meter 8))
                     
(define math-env
  `((+ . ,(meter + 1))
    (- . ,(meter - 1))
    (* . ,(meter * 1))
    (/ . ,(meter / 4))))


(evaluate '(+ 1 2)
                math-env)
(evaluate '(- 2 3)
                math-env)
(evaluate '(* 2 3)
                math-env)
(evaluate '(/ 2 3)
                math-env)
(evaluate '(/ 2 3)
                math-env)