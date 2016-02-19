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
      (cases enviroment old-env
             (empty-env ()
                        (eopl:printf "empty enviroment"))
             (extend-env (var val env)
                         (if (eqv? x var)
                             val
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
                        (letrec ((more-env (lambda (var-lst val-lst env)
                                             (cond
                                               [(and (null? var-lst) (null? val-lst))
                                                env]
                                               [(or (null? var-lst) (null? val-lst))
                                                (eopl:printf "parameters not match ~a <------> ~a~%" var val)]
                                               [else
                                                (let ((p-var (car var-lst))
                                                      (p-val (car val-lst)))
                                                  (let ((v (value-of p-val env)))
                                                    (more-env (cdr var-lst) (cdr val-lst) (extend-env p-var v env))))]))))
                          (value-of body (more-env var val env)))))))
  
  
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
                      (apply-env env x))


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
                       (let ((proc (expval->proc (value-of exp1 env))))
                         ;; (eopl:printf "call-exp: ~a~%" val1 )
                         (apply-procedure proc exp2)))
             )))

  (define program-str "let y = proc (x) if zero?(-(x,5)) then 1 else 2 in (y 18)")
  (define program-str-1 "letproc y(x) = if zero?(-(x,18)) then 1 else 2 in (y 18)")

  ;; Exercise 3.20  Currying test
  (define program-str-2 "let f = proc (x) proc (y) -(x, -(0, y))  in ((f 5) 4)")


  (define test-list
    '("let y = proc (x) if zero?(-(x,5)) then 1 else 2 in (y 18)"
      "letproc y(x) = if zero?(-(x,18)) then 1 else 2 in (y 18)"
      "let f = proc (x) proc (y) -(x, -(0, y))  in ((f 5) 4)"
      "let f = proc (x,y,z,w) -(x, -(0, -(y, -(0,-(z, -(0, w)))))) in (f 1 2 3 4)"))

  
  (letrec ((test (lambda (lst)
                   (if (null? lst)
                       (eopl:printf "All test success!")
                       (begin
                         (eopl:printf "~a~%" (value-of-program (scan&parse (car lst))))
                         (test (cdr lst)))))))
    (test test-list))

;  (define test-str "let f = proc (x,y,z,w) -(x, -(0, -(y, -(0,-(z, -(0, w)))))) in (f 1 2 3 4)")

 ; (eopl:printf "~a~%" (value-of-program (scan&parse test-str)))

  

  )
