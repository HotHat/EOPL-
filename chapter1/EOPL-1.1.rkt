#lang racket


;; Exercise 1.1


;; 1. {3n + 2 | n in N}

(define isEx1?
  (lambda (n)
    (cond
      [(< n 2) #f]
      [(= n 2) #t]
      [else
       (isEx1? (- n 3))])))

;(isEx1? 1)
;(isEx1? 2)
;(isEx1? 3)
;(isEx1? 4)
;(isEx1? 5)
;(isEx1? 102983)
;(display "------------------\n")
;; 2. {2n + 3m + 1 | n, m in N}

(define isEx2?
  (lambda (n)
    (cond
      [(< n 1) #f]
      [(= n 1) #t]
      [else
       (or (isEx2? (- n 2)) (isEx2? (- n 3)))])))

;(isEx2? 0)
;(isEx2? 1)
;(isEx2? 2)
;(isEx2? 3)
;(isEx2? 4)
;(isEx2? 5)
;(isEx2? 1983713)

;(display "------------------\n")

;; 3. {(n, 2n+1) | n in N}


(define isEx3?
  (lambda (n)
    (let ([first (car n)]
          [secend (cadr n)])
      ;(display first) (display "----") (display secend)  (display "\n")
      (cond
        [(or (< first 0) (< secend 1)) #f]
        [(and (= first 0) (= secend 1)) #t]
        [else
         (isEx3? (list (- first 1) (- secend 2)))]))))

;(isEx3? '(0 1))
;(isEx3? '(1 2))
;(isEx3? '(2 3))
;(isEx3? '(4 5))
;(isEx3? '(5 8))
;(isEx3? '(182 82783))
;(isEx3? '(50 101))

;; 4. {(n, n*n) | n in N}

(define isEx4?
  (lambda (n)
      (let ([first (car n)]
            [secend (cadr n)])
        (cond
          [(or (< first 0) (< secend 0)) #f]
          [(and (= first 0) (= secend 0)) #t]
         ; [(and (= first 1) (= secend 1)) #t]
          [else
           (isEx4? (list (- first 1) (- (- secend (* 2 (- first 1))) 1)))]))))


;(isEx4? '(0 0))
;(isEx4? '(1 1))
;(isEx4? '(2 3))
;(isEx4? '(3 9))
;(isEx4? '(4 15))
;(isEx4? '(49 2401))

