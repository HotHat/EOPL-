(module data-structures (lib  "eopl/eopl.rkt")
  
  
  (provide (all-defined-out))
  


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;  ExpVal := Bool + Int + Pair + EmptyList
  ;  DesVal := Bool + Int + Pair + EmptyList


  (define-datatype expval expval?
    (num-val
     (num number?))
    (bool-val
     (bool boolean?))
    
    ;; Exercise 3.9
    (emptylist (emptylist null?))
    (pair-val
     (first expval?)
     (secend expval?))
    
    
   )
  
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
  
  
  ;; Exercise 3.9
  (define car-my
    (lambda (val)
      (cases expval val
          (pair-val (first secend)
               first)
          (else 
             (eopl:error "~a is a empty list" val)))))
  
  (define cdr-my
    (lambda (val)
      (cases expval val
        (pair-val (first secend)
            secend)
        (else  
            (eopl:error "~a is empty list!" val)))))
  
  (define null?-my
    (lambda (val)
      (cases expval val
        (emptylist (exp)
            (bool-val #t))
        (else
             (bool-val #f)))))

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