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
       ("zero?" "(" expression ")")
       zero?-exp)

      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)
      
      ;; Exercise 3.6 [*] ;;;;;;;
      (expression
       ("minus" "(" expression ")") minus-exp)

      ;; End 3.6 ;;;;;;;;;;;;;;;

      
      ;; Exercise 3.7 [*]
      (expression
       ("add" "(" expression "," expression ")") add-exp)
      (expression
       ("mult" "(" expression "," expression ")") mult-exp)
      (expression
       ("quot" "(" expression "," expression ")") quot-exp)
      ;; End 3.7 ;;;;;;;;;;;;
      
      
      
      ;; Exercise 3.8 [*]
      (expression
       ("equal?" "(" expression "," expression ")") equal?-exp)
      (expression
       ("greater?" "(" expression "," expression ")") greater?-exp)
      (expression
       ("less?" "(" expression "," expression ")") less?-exp)
      
      
      ;; End 3.8 ;;;;;;;;;

      
      ;; Exercise 3.9 [**]
      
      (expression
       ("cons" "(" expression "," expression  ")") cons-st-exp)
      (expression
       ("null?" "(" expression ")") null?-exp)
      (expression
       ("car" "(" expression ")") car-st-exp)
      (expression
       ("cdr" "(" expression ")") cdr-st-exp)
      (expression 
       ("emptylist") emptylist-exp)
      
      
      ;; End 3.9
      
      
      ;; Exercise 3.10
      (expression (list) lst-list-exp)
      (list
       ("list" "(" list-items ")") list-exp)
      (list-items (expression list-items) list-item-exp)
      (list-items ("," list-items) list-items-exp)
      (list-items () list-empty-exp)
      
      
      ;; End 3.10
      
      
      
      (expression (identifier) var-exp)

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

  car-st-exp
  
  ;(display (scan&parse "let x = 10 in -(x, 8)"))



  


  (define run
    (lambda (string)
      (value-of-program (scan&parse string))))

  (define value-of-program
    (lambda (val)
      (cases program val
        (a-program (exp)
            (value-of exp (init-env))))))

  
  (define value-of-list
    (lambda (val env)
      (cases list val
        (list-exp (item)
            (value-of-list-item item env)))))
  
  (define value-of-list-item
    (lambda (val env)
      (cases list-items val 
        (list-item-exp (lst items)
              (let ((first (value-of lst env))
                    (second (value-of-list-item items env)))
                  (pair-val first second)))
        (list-items-exp (items)
              (value-of-list-item items env))
        (list-empty-exp ()
              (emptylist '())))))
  
  
  
  (define value-of
    (lambda (val env)
      (cases expression val
        (const-exp (num)
            (num-val num))
        (var-exp (var)
            (apply-env env var))
        (diff-exp (exp1 exp2)
            (let ((num1 (value-of exp1 env))
                  (num2 (value-of exp2 env)))
              (let ((val1 (expval->num num1))
                    (val2 (expval->num num2)))
                (num-val (- val1 val2)))))
        (zero?-exp (exp)
           (let ((val (value-of exp env)))
            (if (zero? (expval->num val))
                (bool-val #t)
                (bool-val #f))))

        ;; Exercise 3.6
        (minus-exp (exp)
           (num-val (- (expval->num (value-of exp env)))))
        ;; End 3.6

        ;; Exercise 3.7
        (add-exp (exp1 exp2)
           (let ((val1 (expval->num (value-of exp1 env)))
                 (val2 (expval->num (value-of exp2 env))))
             (num-val (+ val1 val2))))

         (mult-exp (exp1 exp2)
           (let ((val1 (expval->num (value-of exp1 env)))
                 (val2 (expval->num (value-of exp2 env))))
             (num-val (* val1 val2))))
         (quot-exp (exp1 exp2)
           (let ((val1 (expval->num (value-of exp1 env)))
                 (val2 (expval->num (value-of exp2 env))))
             (num-val (/ val1 val2))))
        ;; End 3.7
        
        ;; Exercise 3.8
         (equal?-exp (exp1 exp2)
           (let ((val1 (expval->num (value-of exp1 env)))
                 (val2 (expval->num (value-of exp2 env))))
             ;(eopl:printf "val1: ~a, val2: ~a~%" val1 val2)
             (bool-val (= val1 val2))))
        (greater?-exp (exp1 exp2)
           (let ((val1 (expval->num (value-of exp1 env)))
                 (val2 (expval->num (value-of exp2 env))))
             (bool-val (> val1 val2))))
        (less?-exp (exp1 exp2)
           (let ((val1 (expval->num (value-of exp1 env)))
                 (val2 (expval->num (value-of exp2 env))))
             (bool-val (< val1 val2))))
        ;; End 3.8
        
        
     
        ;; Exercise 3.9
        
        (cons-st-exp (first secend)
         (let ((val1 (value-of first env))
               (val2 (value-of secend env)))
           (pair-val val1 val2)))
        
        (null?-exp (exp)
           (let ((val (value-of exp env)))
            (null?-my val)))
        
        (car-st-exp (exp)
            (car-my exp))
        
        (cdr-st-exp (exp)
            (cdr-my exp))
        
        (emptylist-exp ()
            (emptylist '()))
        
        ;; End 3.9
        
        ;; Exercise 3.10
        (lst-list-exp (lst)
            (value-of-list lst env))
        
        ;; End 3.9
        
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
  
  (display (run "let x=4 in list(4,-(x, 1),-(x,3))"))
  
  
  
)