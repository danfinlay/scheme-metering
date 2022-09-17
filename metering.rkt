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

(define (create-metered-method-old target limit)
  (begin
    (define remaining limit)
    (display (string-append "created call limit of " (number->string remaining) "\n"))
    (lambda (x y)
      (begin
        (set! remaining (- remaining 1))
        (display (string-append "remaining calls " (number->string remaining) "\n"))
        (if (> remaining 0)
          (apply target x y)
          (error "Exceeded addition call limit"))))))


(define (create-metered-method target limit)
  (begin
    (define remaining limit)
    (lambda (first . rest)
      (begin
        (set! remaining (- remaining 1))
        (display (string-append "remaining calls " (number->string (+ remaining 1)) "\n"))
        (if (> remaining -1)
          (apply target (cons first rest))
          (error "Exceeded addition call limit"))))))

(define (create-meter limit)
  (begin
    (define remaining limit)

    ; Per method attenuator:
    (lambda (target)
      (begin
        (lambda (first . rest)
          (begin
            (set! remaining (- remaining 1))
            (display (string-append "remaining calls " (number->string (+ remaining 1)) "\n"))
            (if (> remaining -1)
              (apply target (cons first rest))
              (error "Exceeded addition call limit"))))))))


(define meter (create-meter 2))
(define metered-add (meter +))
                     

(define math-env
  `((+ . ,(meter +))
    (- . ,(meter -))
    (* . ,(meter *))
    (/ . ,(meter /))))


(evaluate '(+ 1 2)
                math-env)
(evaluate '(- 2 3)
                math-env)

(evaluate '(/ 2 3)
                math-env)