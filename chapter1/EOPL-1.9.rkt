#lang racket



(define remove
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? s (car los))
            (remove s (cdr los))
            (cons (car los)
                  (remove s (cdr los)))))))




(remove 'a '(a b a c))
(remove 'b '(e f g))
(remove 'a4 '(c1 a4 c1 a4))
(remove 'x '())