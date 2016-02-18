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

  
  
  ;(display (scan&parse "let x = 10 in -(x, 8)"))



  


  (define run
    (lambda (string)
      (value-of-program (scan&parse string))))

  (define value-of-program
    (lambda (val)
      (cases program val
        (a-program (exp)
            (value-of exp (init-env))))))

  
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
        
        
        
        (if-exp (exp1 exp2 exp3)
           (let ((val (value-of exp1 env)))
             (if (expval->bool val)
                 (value-of exp2 env)
                 (value-of exp3 env))))
        (let-exp (iden exp1 body)
            (let ((val (value-of exp1 env)))
              (value-of body (extend-env iden val env)))))))
         

        
                        


  (display (run "let x = 10 in -(x, 8)"))
           
  (display (run "let x = 7
                 in let y = mult(2,20)
                 in let y = let x = -(add(x,10), 1)
                    in -(x, y) in -(-(x,8),y)"))

  (display (run "minus(-(minus(5),9))"))

  (display (run "add(5,9)"))
  (display (run "mult(5 ,9)"))
  (display (run "quot( 5 ,9 )"))
  
  (eopl:printf "10 = 5: ~a~%" (run "equal?(10,5)"))
  (eopl:printf "10 = 10: ~a~%"(run "equal?(10,10)"))
  (eopl:printf "10 > 8: ~a~%" (run "greater?(10,8)"))
  (eopl:printf "10 < 8: ~a~%" (run "less?(10,8)"))
  
  
  
)