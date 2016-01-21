(module data-structures (lib  "eopl/eopl.rkt")
  
  
  (provide (all-defined-out))
  


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




  (define-datatype expval expval?
    (num-val
     (num number?))
    (bool-val
     (bool boolean?)))
  
  (define expval->num
    (lambda (val)
      (cases expval val
        (num-val (num) num)
        (else
         (eopl:error "error: ~a, ~a" 'num val)))))
  
  (define expval->bool
    (lambda (val)
      (cases expval val
        (bool-val (bool) bool)
        (else (eopl:error "error: ~a, ~a" 'num val)))))
  

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (define empty-env
    (lambda ()
      (lambda (var)
        (eopl:error "unbinded variable: " var))))

  (define extend-env
    (lambda (var val env)
      (lambda (f-var)
        (if (equal? f-var var)
            val
            (env f-var)))))

  (define apply-env
    (lambda (env var)
      (env var)))

  (define init-env
    (lambda ()
      (extend-env 'i (num-val 1)
           (extend-env 'v (num-val 5)
               (extend-env 'x (num-val 10)
                     (empty-env))))))
  
  ;(display (apply-env (init-env) 'x))
  
  
  
)  