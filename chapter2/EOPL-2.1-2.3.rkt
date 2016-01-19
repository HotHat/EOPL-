#lang racket



;; Exercise 2.1 [*] Implement the four required operations for bigits. Then use your 
;; implementation to calculate the factorial of 10. How does the execution time vary 
;; as this argument changes? How doess the execution time vary as the base changes? Explan why.

(define zero
  (lambda ()
    '()))

(define is-zero?
  (lambda (n)
    (if (null? n)
        #t
        #f)))

(is-zero? (zero))

(define successor
  (lambda (lst)
    (if (null? lst)
        
        (cons 1 '())
        
        (if (= (+  (car lst) 1) 16)
            (cons 0 (successor (cdr lst)))
            (cons (+ (car lst) 1) (cdr lst))))))

; (successor '(15))


;(define test-suc
;  (lambda (lst n last)
;    (if (> n 0)
;        (let ((nn (successor lst)))
;         (display (+ (- last n) 1)) (display ":  ") (display nn) (display #\return)
;          (test-suc nn (- n 1) last))
;        '())))
;(test-suc (zero) 44856 44856)





(define predecessor
  (lambda (n)
    (if (is-zero? n)
        (error "predecessor of zero undefined!")
        (if (> (- (car n) 1) 0)
            (cons (- (car n) 1) (cdr n))
            
            (if (= (- (car n) 1) 0)
                '()
                
                (cons (- 16 1) (predecessor (cdr n))))))))




(define number->bigNumber
  (lambda (n)
    (letrec ([ctx (lambda (begin number)
                    (if (> number 0)
                        (let ((n (successor begin)))
                          (ctx n (- number 1)))
                        begin))])
      (ctx (zero) n))))


(number->bigNumber 258)


;(define plus





;(predecessor '(0 0 1))


;; 8+3*16+15*16*16+10*16*16*16 
;; 8+48+3840+40960



;; Exercise 2.2 [**] Analyze each of these proposed representations critically. 
;; To what extent do they succeed or fail in satisfying the specification of the data type?


;; TODO:



;; Exercise 2.3 [**] Define a representation of all the integers (negative and nonnegative) as diff-trees,
;; where a diff-tree is a list defined by the grammar

;; Diff-tree ::= (one) | (diff diff-tree diff-tree)


;; 1)  every number n can be presentate by  (n-1) + 1  <==> (n-1) - (-1) <==> (n-1)-(0-1) <==> (n-1)-(diff 0 (one))
;; 0 has infinitely many preresentation, when left diff-tree equal right diff-tree then (n-1)-(diff 0 (one))
;; then 


;; 2)

(define one '(one))

(define diff-tree
  (lambda (left right)
    `(diff ,left ,right)))

(diff-tree one one)

(define zero-tree
  (lambda()
    (diff-tree one one)))




(define zero?-tree   ;; error it's a 
  (lambda (n)
    (letrec ((loop 
              (lambda (num)
                (if (and (pair? num) (equal? 'diff (car num)))
                    (let ((left (cadr num))
                          (right (caddr num)))
                      (cond
                        [(equal? (car left) 'one) 
                         (- 1 (loop right))]
             
                        [(equal? (car right) 'one)
                         (- (loop left) 1)]
            
                        [else 
                         (- (loop left) (loop right))]))
                    1))))
      (if (= 0 (loop n))
          #t
          #f))))


(display "-----****-----\n")
(zero?-tree one)
;
(zero?-tree (diff-tree  one (diff-tree one one)))
;
(zero?-tree (diff-tree (diff-tree one one) (diff-tree one one)))
;
(zero?-tree  (diff-tree (diff-tree (diff-tree one one) (diff-tree one one)) (diff-tree (diff-tree one one) (diff-tree one one))))
(zero?-tree  (diff-tree (diff-tree (diff-tree one one) one) (diff-tree (diff-tree one one) (diff-tree one one))))

(display "----end-----\n")


(define successor-tree
  (lambda (n)
    (let ((negative-one (diff-tree (diff-tree one one) one)))
      (diff-tree n negative-one))))


(define predecessor-tree
  (lambda (n)
    (diff-tree n one)))

(zero?-tree (predecessor-tree (predecessor-tree (successor-tree (successor-tree (zero-tree))))))

(successor-tree one)


(define diff-tree->number
  (lambda (n)
    (letrec ((loop 
              (lambda (num)
                (if (and (pair? num) (equal? 'diff (car num)))
                    (let ((left (cadr num))
                          (right (caddr num)))
                      (cond
                        [(equal? (car left) 'one) 
                         (- 1 (loop right))]
             
                        [(equal? (car right) 'one)
                         (- (loop left) 1)]
            
                        [else 
                         (- (loop left) (loop right))]))
                    1))))
      (loop n))))





;; 3)

(define diff-tree-plus
  (lambda (n1 n2)
    (let ((negative-one (diff-tree (diff-tree one one) one)))
      (diff-tree n1 (diff-tree one (successor-tree n2))))))



