(module data-structures (lib  "eopl/eopl.rkt")
  
  
  (provide (all-defined-out))
  
  (define a-program
    (lambda (expression)
    `(a-program ,expression)))
  
  (define var-exp
    (lambda (identifier)
    `(var-exp ,identifier)))
  
  (define zero?-exp
    (lambda (expression)
    `(zero?-exp ,expression)))
  
  (define diff-exp
    (lambda (exp1 exp2)
    `(diff-exp ,exp1 ,exp2)))

  
  (define if-exp
    (lambda (exp1 exp2 exp3)
    `(if-exp ,exp1 ,exp2 ,exp3)))
  
  (define let-exp
    (lambda (ident exp1 exp2)
    `(let-exp ,ident ,exp1 ,exp2)))
  
  (define const-exp
    (lambda (ident)
      `(const-exp ,ident)))
  
  
)  