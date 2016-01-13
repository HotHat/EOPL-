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