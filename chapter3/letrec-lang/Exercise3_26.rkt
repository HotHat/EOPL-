(module data-structures (lib "eopl.ss" "eopl")

  ;; expval := INT + BOOL + PROC

  (require "lang.rkt")

  
  ;; (define-datatype enviroment enviroment?
  ;;   (empty-env)
  ;;   (

  (define-datatype  enviroment enviroment?
    (empty-env)
    (extend-env
     (var symbol?)
     (val expval?)
     (env enviroment?)))


  (define apply-env
    (lambda (old-env x)
      ;; (begin
      ;;   (eopl:printf "apply-env : ~a => ~a~%" old-env x))
      (cases enviroment old-env
             (empty-env ()
                        (eopl:printf "empty enviroment for: ~a~%" x))
             (extend-env (var val env)

                         (if (equal? x var)
                             (begin
                              ; (eopl:printf "Get variable: ~a~%~%" x)
                               val)
                             (apply-env env x))))))


  
  (define-datatype proc proc?
    (procedure
     (var list?)   ;; change for Exercise 3.21   <== (var symbol?)
     (body expression?)
     (env enviroment?)))

  (define apply-procedure
    (lambda (proc1 val)
      ;; (begin
      ;;   (eopl:printf "apply-procedure proc: ~a~%" proc1)
      ;;   (eopl:printf "apply-procedure val: ~a~%" val))

      (cases proc proc1
             (procedure (var body env)
                        ;; change by Exercise 3.21
                        ;; (eopl:printf "var   : ~a  => " var)
                        ;; (eopl:printf "body: ~a   => " body)
                        ;; (eopl:printf "env: ~a => " env)
                        ;; (eopl:printf "val: ~a~%~%" val)
                        (letrec ((more-env (lambda (var-lst val-lst new-env)
                                             (cond
                                               [(and (null? var-lst) (null? val-lst))
                                                new-env]
                                               [(or (null? var-lst) (null? val-lst))
                                                (eopl:printf "parameters not match ~a <------> ~a~%" var val)]
                                               [else
                                                (let ((p-var (car var-lst))
                                                      (p-val (car val-lst)))
                                                  ;; (eopl:printf "var   : ~a~%~%" p-var)
                                                  ;; (eopl:printf "value: ~a~%~%" p-val)
                                                  ;; (eopl:printf "env: ~a~%~%" new-env)
                                                  ;; (eopl:printf "var   : ~a~%~%" p-var)
                                                     ;; (eopl:printf "value: ~a~%~%" v)
                                                     ;; (eopl:printf "env: ~a~%~%" new-env)
                                                    (more-env (cdr var-lst) (cdr val-lst) (extend-env p-var p-val new-env)))]))))
                          (let* ((new-env (more-env var val env))
                                (value (value-of body new-env)))
                           ;; (eopl:printf "var ~a =>  ~a~%~%" var val)
                           ;; (eopl:printf "apply-procedure code: ~a~% => ~a ~%" (struct-to-code body) val)

                           ;; (eopl:printf "apply-procedure value: ~a~%~%" value)
                            value))))))
  
  
  (define-datatype expval expval?
    (num-val
     (val number?))
    (bool-val
     (val boolean?))
    (proc-val
     (val proc?)))


  (define expval-extractor-error
    (lambda (val1 val2)
      (eopl:printf "Error occured expert ~a but get ~a~%" val1 val2)))
  
  (define expval->num
    (lambda (val)
      (cases expval val
          (num-val (val) val)
          (else
           (expval-extractor-error 'int val)))))

  (define expval->bool
    (lambda (val)
      (cases expval val
             (bool-val (val) val)
             (else
              (expval-extractor-error 'bool val)))))

  (define expval->proc
    (lambda (val)
      (cases expval val
             (proc-val (val) val)
             (else
              (expval-extractor-error 'proc val)))))


  ;;;;;;;;;;;; init enviroment ;;;;;;;;;;;;;;;;;;

  (define init-env
    (lambda ()
      (extend-env 'i (num-val 1)
                  (extend-env 'v (num-val 5)
                              (extend-env 'x (num-val 10)
                                          (empty-env))))))
  
;  (eopl:printf "~a~%" (init-env))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  

  

;  (eopl:printf "~a~%" (just-scan program-str))

  ;(eopl:printf "~a~%" (scan&parse program-str))

  (define value-of-program
    (lambda (pgm)
      (cases program pgm
             (a-program (exp)
                         (value-of exp (init-env))))))
  
  (define value-of
    (lambda (p env)
      (cases expression p
             (const-exp (val)
                        (num-val val))
             (diff-exp (exp1 exp2)
                       (let ((val1 (expval->num (value-of exp1 env)))
                             (val2 (expval->num (value-of exp2 env))))
                         (num-val (- val1 val2))))
             (zero?-exp (exp)
                        (let ((val1 (value-of exp env)))
                          (if (= (expval->num val1) 0)
                              (bool-val #t)
                              (bool-val #f))))
             (if-exp (exp1 exp2 exp3)
                     (let ((val1 (value-of exp1 env)))
                       (if (expval->bool val1)
                           (value-of exp2 env)
                           (value-of exp3 env))))
             (var-exp (x)
                      (let ((var (apply-env env x)))
                        ;(eopl:printf "Find var :~a => ~a  => ~a~%~%" x var env)
                        var))


             (let-exp (iden exp1 p-body)
                      (let ((val1 (value-of exp1 env)))
                        ;; (eopl:printf "let-exp exp1: ~a~%" val1)
                        (value-of p-body (extend-env iden val1 env))))

             ;; Exercise 3.19 letproc
             (letproc-exp (p-name p-var p-proc p-body)
                           (let ((proc (proc-val (procedure (list p-var) p-proc env))))  ;; chang for Exercise:  p-var ===> (list p-var)
                                 (value-of p-body (extend-env p-name proc env))))

             (proc-exp (iden exp1)
                       (proc-val (procedure iden exp1 env)))
             
             (call-exp (exp1 exp2)
                       (letrec ((proc (expval->proc (value-of exp1 env)))
                                (more-param (lambda (parms rsl)
                                             (if (null? parms)
                                                 rsl
                                                 (let ((val (value-of (car parms) env)))
                                                   (more-param (cdr parms) (cons val rsl)))))))
                         ;; (eopl:printf "call-exp: ~a~%" val1 )
                         (let ((param-lst (more-param (reverse exp2) '())))
                          ;; (eopl:printf "call-exp: ~a~%" param-lst )
                           (apply-procedure proc param-lst))))
             )))



  ;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;

  (define free-variables
    (lambda (pgm bounds)
      (cases expression pgm
             (const-exp (x)
                        '())
             (var-exp (v)
                      (if (member v bounds)
                          '()
                          (list v)))
             (diff-exp (exp1 exp2)
                       (append (free-variables exp1 bounds)
                               (free-variables exp2 bounds)))
             (zero?-exp (exp)
                        (free-variables exp bounds))
             (if-exp (iden exp1 exp2)
                     (append (free-variables iden bounds)
                             (free-variables exp1 bounds)
                             (free-variables exp2 bounds)))
             (let-exp (iden exp body)
                      (append (free-variables iden bounds)
                              (free-variables exp bounds)
                              (free-variables body (cons iden bounds))))
             (proc-exp (p-var p-body)
                       (free-variables p-body (cons p-var bounds)))
             (letproc-exp (iden p-var p-exp p-body)
                          (eopl:printf "letproc~%"))
             (call-exp (exp1 exp2)
                       (append (free-variables exp1 bounds)
                               (free-variables exp2 bounds))))))
                        
                       

  (define free-variables-test
    '(
    "3"
    "a"
    "zero?(x)"
    "if zero?(-(x,5)) then  y else p"
    "let f=proc(x) x in (f 5)"
    "proc(x) proc(y) -(x,y)"
    ))

  (letrec ((test (lambda (lst)
                   (if (null? lst)
                       (eopl:printf "Test over")
                       (let ((x (scan&parse (car lst))))
                         (eopl:printf "free variable: ~a~%" (free-variables x '()))
                         (test (cdr lst)))))))
    (test free-variables-test))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  

  (define program-str "let y = proc (x) if zero?(-(x,5)) then 1 else 2 in (y 18)")
  (define program-str-1 "letproc y(x) = if zero?(-(x,18)) then 1 else 2 in (y 18)")

  ;; Exercise 3.20  Currying test
  (define program-str-2 "let f = proc (x) proc (y) -(x, -(0, y))  in ((f 5) 4)")


  (define test-list
    '("let y = proc (x) if zero?(-(x,5)) then 1 else 2 in (y 18)"
      "letproc y(x) = if zero?(-(x,18)) then 1 else 2 in (y 18)"
      "let f = proc (x) proc (y) -(x, -(0, y))  in ((f 5) 4)"
      "let f = proc (x,y,z,w) -(x, -(0, -(y, -(0,-(z, -(0, w)))))) in (f 1 2 3 4)"
      "let makemult = proc (maker) proc (x)
                                        if zero?(x)
                                            then 0
                                        else -(((maker maker) -(x,1)), -4)
                      in let times4 = proc (x) ((makemult makemult) x) in (times4 3)"

      ;; Exercise 3.23
      "let makemult = proc (maker, x, y)
      if zero?(x)
      then 0
      else
      -((maker maker -(x,1) y), -(0,y))
      in let times = proc (x, y)
      (makemult makemult x y) in (times 3 5)"

      ;; Exercise 3.24
      "let odd= proc (make1, make2, x)
if zero?(x)
then
zero?(1)
else
(make2 make2 make1 -(x,1) )
in let even = proc (make1, make2, x)
if zero?(x)
then
zero?(0)
else
(make2 make2 make1 -(x,1))
in  let odd? = proc (x)
(odd odd even x)
in  let even? = proc (x)
(even even odd x)
in (even? 101)"        

;; λf (λx (λz ((f (x x)) z)))  => (Y F)

"let makerec = proc (f)
                     let d = proc (x)
                              proc (z)
                               proc (w)
                               (((f (x x)) z) w)
                     in proc (n) 
                         proc (m)
                            (((f (d d)) n) m)
in let maketimes4 = proc (f) proc (x)
                             proc (y)
                            if zero?(x)
                            then 0
                            else -(((f -(x,1)) y), -(0,y))
in let times4 = (makerec maketimes4) in ((times4 3) 4)"

      ))


;; (even even odd 3)
;; (odd odd even 2)
;; (
  
  (letrec ((test (lambda (lst)
                   (if (null? lst)
                       (eopl:printf "All test success!")
                       (begin
                         (eopl:printf "~a~%" (value-of-program (scan&parse (car lst))))
                         (test (cdr lst)))))))
    (test test-list))

  
;;   (define test-struct (scan&parse (list-ref test-list 6)))
;; ;;  (eopl:printf "~a~%" test-struct)
;;   (eopl:printf "~a~%" (value-of-program test-struct))

  

  )





