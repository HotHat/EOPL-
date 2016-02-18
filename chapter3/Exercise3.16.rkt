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
       ("let" (arbno identifier "=" expression) "in" expression)
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
                 (letrec ((more-env (lambda (lst1 lst2 env)
                                   (if (null? lst1)
                                       env
                                       (let ((vl (value-of (car lst2) env)))
                                         (more-env (cdr lst1) (cdr lst2) (extend-env (car lst1) vl env)))))))
                   ;; (value-of body (more-env (reverse iden) (reverse exp1) env)))))))    **** let
                   (value-of body (more-env iden exp1 env)))))))   ;;      *********  let*



  
  (eopl:printf "~a~%" (run "let x = 30
                 in let x = -(x,1)
                        y = -(x,2)
                     in  -(x, y)"))


 
  
  
)
