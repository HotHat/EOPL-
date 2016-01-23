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
    '((list 
       ("list" "(" items ")")   list-exp)
      (items (number items)    num-exp)
      (items (identifier items) var-exp)
      (items (list items)      list-item-exp)
      (items ("," items)        items-list)
      (items ()                 empty-item)
      
      ))   

    
 
       
  
    
 
  ;;;;;;;;;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))

  
  
 ; (display (just-scan "list(x,y,z)"))
 ; (display (scan&parse "list(5,y,z)"))
  
  
  
  
   (define value-of-list
    (lambda (val)
      (cases list val
        (list-exp (item)
            (value-of-items item)))))
  
  (define value-of-items
    (lambda (val)
      (cases items val
        (num-exp (num items)
            (let ((secend (value-of-items items)))
              (cons num secend)))
        (var-exp (var items)
           (let ((secend (value-of-items items)))
              (cons var secend)))
        (list-item-exp (lst item)
            (let ((first (value-of-list lst))
                  (secend (value-of-items item)))
              (cons first secend)))
        (items-list (items)
             (value-of-items items))
        (empty-item ()
            '()))))
  
  
  (define list-val (scan&parse "list(list(5,8,10),9,y,z)"))
 ; (display list-val) 
 ; (value-of-list list-val)
  (display (value-of-list list-val))
  
  
  
 
  

)