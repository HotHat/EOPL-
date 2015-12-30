#lang racket



;; Exercise 1.15 [*] (duple n x) returns a list containing a list containing n copies of x.

(define duple
  (lambda (n x)
    (if (zero? n)
        '()
        (cons x (duple (- n 1) x)))))


(duple 2 3)
(duple 4 '(ha ha))
(duple 0 '(blah))



;; Exercise 1.16 [*] (invert lst), where lst is a list of 2-lists (lists of length two),
;; returns a list with each 2-list reversed.

(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (let ((first (caar lst))
              (secend (cadar lst)))
          (cons (list secend first) (invert (cdr lst)))))))

(invert '((a 1) (a 2) (a 3) (1 b) (2 b)))


;; Exercise 1.17 [*] (down lst) wraps parentheses around each top-level element of lst.

(define down
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (list (car lst)) (down (cdr lst))))))


(down '(1 2 3))
(down '((a) (fine) (idea)))
(down '(a (more (complicated)) object))


;; Exercise 1.18 [*] (swapper s1 s2 slist) returns a list the same as slist,
;; but with all occurrences of s1 replaced by s2 and all occurrences of s2 replaced by s1.

(define swapper
  (lambda (s1 s2 slist)
    (cond
      [(null? slist) '()]
      [(symbol? (car slist))
       (cond
         [(eqv? s1 (car slist))
          (cons s2 (swapper s1 s2 (cdr slist)))]
         [(eqv? s2 (car slist))
          (cons s1 (swapper s1 s2 (cdr slist)))]
         [else
          (cons (car slist) (swapper s1 s2 (cdr slist)))])]
      [else
       (cons (swapper s1 s2 (car slist))
             (swapper s1 s2 (cdr slist)))])))


(swapper 'a 'd '(a b c d))
       
(swapper 'a 'd '(a d () c d))
(swapper 'x 'y '((x) y (z (x))))


;; Exercise 1.19 [**] (list-set lst n x) returns a list like ls,
;; except that the n-th element, using zero-based indexing, is x.

(define list-set
  (lambda (lst n x)
    (if (null? lst)
        '()
        (if (zero? n)
            (cons x (list-set (cdr lst) (- n 1) x))
            (cons (car lst)  (list-set (cdr lst) (- n 1) x))))))

(list-set '(a b c d) 2 '(1 2))
(list-ref (list-set '(a b c d) 3 '(1 5 10)) 3)    



;; Exercise 1.20 [*] (count-occurrences s slist) returns the number of occurrences of s in slist.

(define count-occurrences
  (lambda (s slist)
    (cond
      [(null? slist) 0]
      [(symbol? (car slist))
       (if (eqv? s (car slist))
           (+ 1 (count-occurrences s (cdr slist)))
           (count-occurrences s (cdr slist)))]
      [else
       (+ (count-occurrences s (car slist))
          (count-occurrences s (cdr slist)))])))

(count-occurrences 'x '((f x) y (((x z) x))))
(count-occurrences 'x '((f x) y (((x z) () x))))
(count-occurrences 'w '((f x) y (((x z) x))))


                

