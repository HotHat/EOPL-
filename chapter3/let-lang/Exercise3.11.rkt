(module lang (lib "eopl.ss" "eopl")

  (require "data-structures.rkt")
  
 

  
  
  
   ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)))
     
  
  (define the-grammar
    '((program (expression) a-program)

      (expression (number) const-exp)
      (expression
        ("-" "(" expression "," expression ")")
        diff-exp)
      
      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)
    
      ;; Exercise 3.11
      ;(expression (identifier) var-exp)
      
      (expression
       (identifier function-expression) i-fun-exp)
      
      (function-expression
       ("(" list-operator ")") fun-exp)
      (function-expression () empty-fun-exp)
      
      (list-operator (expression list-operator) more-operator-exp)
      (list-operator ("," list-operator) comma-separator-exp)
      (list-operator () operator-end-exp)
      
      ;; End 3.11

      (expression
       ("let" identifier "=" expression "in" expression)
       let-exp)))   
 
    
 
  ;;;;;;;;;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))

 
  
  ;(display (scan&parse "let x = 10 in -(x, 8)"))



  


  (define run
    (lambda (string)
      (value-of-program (scan&parse string))))

  (define value-of-program
    (lambda (val)
      (cases program val
        (a-program (exp)
            (value-of exp (init-env))))))

  
  (define value-of-function
    (lambda (val env)
      (cases function-expression val
        (fun-exp (lst)
            ;(eopl:printf "fun-exp: ~a~%" lst)
            (value-of-list lst env))
        (empty-fun-exp ()
            (emptylist '())))))
  
  (define value-of-list
    (lambda (val env)
      (cases list-operator val
        (more-operator-exp (exp lst)
            (let ((val (value-of exp env)))
              ;(eopl:printf "more-operator-exp: exp: ~a, list: ~a~%" exp lst)
              (pair-val val (value-of-list lst env))))
        
        (comma-separator-exp (lst)
             (value-of-list lst env))
        (operator-end-exp ()
             (emptylist '())))))
         
  
  
  
  (define value-of
    (lambda (val env)
      (cases expression val
        (const-exp (num)
            (num-val num))
        (diff-exp (exp1 exp2)
            (let ((num1 (value-of exp1 env))
                  (num2 (value-of exp2 env)))
              (let ((val1 (expval->num num1))
                    (val2 (expval->num num2)))
                (num-val (- val1 val2)))))
       
        (i-fun-exp (var fun)
           (cond
             [(eqv? 'zero? var)    ;; zero?(expression)
              ;(eopl:printf "i-fun-exp: I: ~a, F: ~a~%" var fun)
              (let ((val (value-of-function fun env)))
                (eopl:printf "value: ~a~%" (car-my val))
                (if (zero? (expval->num (car-my val)))
                    (bool-val #t)
                    (bool-val #f)))]
             
             ;; Exercise 3.6
             [(eqv? 'minus var)
              (let ((val (value-of-function fun env)))
                (let ((v (expval->num (car-my val))))
                   (num-val (- v))))]
             
             ;; Exercise 3.7
             [(eqv? 'add var)
              (let ((val (value-of-function fun env)))
                (let ((v1 (expval->num (car-my val)))
                      (v2 (expval->num (car-my (cdr-my val)))))
                   (num-val (+ v1 v2))))]
             [(eqv? 'multiply var)
              (let ((val (value-of-function fun env)))
                (let ((v1 (expval->num (car-my val)))
                      (v2 (expval->num (car-my (cdr-my val)))))
                   (num-val (* v1 v2))))]
             [(eqv? 'quotient var)
              (let ((val (value-of-function fun env)))
                (let ((v1 (expval->num (car-my val)))
                      (v2 (expval->num (car-my (cdr-my val)))))
                   (num-val (+ v1 v2))))]
             
             ;; Exercise 3.8
             [(eqv? 'equal? var)
              (let ((val (value-of-function fun env)))
                (let ((v1 (expval->num (car-my val)))
                      (v2 (expval->num (car-my (cdr-my val)))))
                    (bool-val (zero? (- v1 v2)))))]
             [(eqv? 'greater? var)
              (let ((val (value-of-function fun env)))
                (let ((v1 (expval->num (car-my val)))
                      (v2 (expval->num (car-my (cdr-my val)))))
                    (bool-val  (> v1 v2))))]
             [(eqv? 'less? var)
              (let ((val (value-of-function fun env)))
                (let ((v1 (expval->num (car-my val)))
                      (v2 (expval->num (car-my (cdr-my val)))))
                    (bool-val  (< v1 v2))))]
             
             ;; Exercise 3.9
             [(eqv? 'cons var)
              (let ((val (value-of-function fun env)))
                (let ((v1 (car-my val))
                      (v2 (car-my (cdr-my val))))
                  (pair-val v1 v2)))]
             [(eqv? 'car var)
              (let ((val (value-of-function fun env)))
                (let ((v1 (car-my val)))
                  v1))]
             [(eqv? 'cdr var)
              (let ((val (value-of-function fun env)))
                (let ((v1 (cdr-my val)))
                  v1))]
             [(eqv? 'null? var)
              (let ((val (value-of-function fun env)))
                (let ((v1 (car-my val)))
                  (eopl:printf "null?-exp: ~a~%" val)
                  (cases expval v1
                    (emptylist (empty)
                       (bool-val #t))
                    (else
                     (bool-val #f)))))]
             
             [(eqv? 'emptylist var)
              (emptylist '())]
             
             ;; Exercise 3.10
             
             [(eqv? 'list var)
              (let ((val (value-of-function fun env)))
                val)]
             
             
             
             
             
             
             [else    ;; this is a variable
              (apply-env env var)]))
        
        (if-exp (exp1 exp2 exp3)
           (let ((val (value-of exp1 env)))
             (if (expval->bool val)
                 (value-of exp2 env)
                 (value-of exp3 env))))
        (let-exp (iden exp1 body)
            (let ((val (value-of exp1 env)))
              (value-of body (extend-env iden val env)))))))
         

        
                        


  ;(display (run "let x = 10 in cons( x, emptylist)"))
           
  
  ;(display (run "let x = 4 in cons( x,cons(-(x,1), emptylist))"))
  

  
  (eopl:printf "~a~%" (run "let x = minus(5) in zero?(x)") )
  
  (eopl:printf "~a~%" (run "cons(let x = minus(5) in  let y = 5 in cons(x,y), emptylist)"))
  (eopl:printf "~a~%" (run "null?(emptylist)"))
  (eopl:printf "~a~%" (run "let x = 5 in let y =6 in let z=8 in list(x,y,z)"))
  
)