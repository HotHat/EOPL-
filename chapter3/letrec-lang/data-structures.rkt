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
     (env enviroment?))
    (extend-env-rec
     (name symbol?)
     (var symbol?)
     (body expression?)
     (env enviroment?)))


  (define apply-env
    (lambda (old-env x)
      (cases enviroment old-env
             (empty-env ()
                        (eopl:printf "empty enviroment"))
             (extend-env (var val env)
                         (if (eqv? x val)
                             val
                             (apply-env env x)))
             (extend-env-rec (name var body env)
                             (if (eqv? x name)
                                 (proc-val (procedure var body env))
                                 (apply-env env x))))))


  
  
  (define-datatype proc proc?
    (procedure
     (var symbol?)
     (body expression?)
     (env enviroment?)))

  (define apply-procedure
    (lambda (proc1 val)
      (cases proc proc1
             (procedure (var body env)
                        (value-of body (extend-env var val env))))))
  

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
  
  (define expval->int
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

  
  (define program-str "letr y = 5 in -(y 8)")
  

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
                       (let ((val1 (value-of exp1 env))
                             (val2 (value-of exp2 env)))
                         (num-val (- val1 val2))))
             (zero?-exp (exp)
                        (let ((val1 (value-of exp env)))
                          (if (expval->bool val1)
                              (bool-val #t)
                              (bool-val #f))))
             (if-exp (exp1 exp2 exp3)
                     (let ((val1 (value-of exp1 env)))
                       (if (expval->bool val1)
                           (let ((val1 (value-of exp2 env)))
                             (num-val val1))
                           (let ((val1 (value-of exp3 env)))
                             (num-val val1)))))
             (var-exp (x)
                      (let ((val ((apply-env env) x)))
                        (num-val val)))
             (let-exp (iden exp1 exp2)
                      (let ((val1 (value-of exp1)))
                        (value-of exp2 (extend-env iden val1))))
             (proc-exp (iden exp1)
                       (proc-val (procedure iden exp1 env)))
             
             (call-exp (exp1 exp2)
                       (let ((proc (expval->proc (value-of exp1 env)))
                             (val1 (value-of exp2 env)))
                         (apply-procedure proc exp2)))
             
             (letrec-exp (name var exp1 body)
                         (let ((exp (value-of exp1 env))
                               (proc (procedure var exp env)))
                           (value-of body (extend-env name  proc body)))))))

  ;(eopl:printf "~a~%" (value-of-program (scan&parse program-str)))
  

  )
