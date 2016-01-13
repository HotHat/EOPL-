#lang racket


(struct node (type start end chars) #:transparent)

(define code "-(55, -(x,11))")
(define delimit '(#\- #\( #\, #\)))
(define space '(#\space #\return #\tab))



(define is-delimit?
  (lambda (ch)
    (if (memq ch delimit)
        #t
        #f)))
(define is-space?
  (lambda (ch)
    (if (memq ch space)
        #t
        #f)))

(is-delimit? "-")
(is-delimit? "a")
(is-space? " ")
(is-space? "a")




(define scan1
  (lambda (str start)
    (if (> (string-length str) start)
        (let ((ch (string-ref str start)))
          (cond
            [(is-space? ch)
             (scan1 str (add1 start))]
            [(is-delimit? ch)
             (values (node 'token start (add1 start) (substring str start (add1 start))) (add1 start))]
            [else
             (let loop ((ss (add1 start)))
               (printf "Test ~s~%" ss )
               (cond
                 [(> ss (string-length str))
                  (values (node 'token start ss (substring str start ss)) ss)]
                 [(or (is-space? (string-ref str ss)) (is-delimit? (string-ref str ss)))
                   (values (node 'token start ss (substring str start ss)) ss)]
                 [else
                  
                  (loop (add1 ss) )]))]))
        (values '() start))))

        
(define scan
  (lambda (str)
    (let loop ((stk-lst '()) (start 0))
      (let-values ([(tk val) (scan1 str start)])
        (printf "Token: ~s, value: ~s~%" tk val)
        (cond
          [(< val (string-length str))
           (loop (cons tk stk-lst) val)]
          [else
           (reverse (cons tk stk-lst))])))))

;; "-(55, -(x,11))"
(scan code )

(scan "zero?(5)")

                        
;; Designing an interface for a recursive data type
;; 1. Include one constructor for each kind of data in the data type.
;; 2. Include one predicate for each kind of data in the data type.
;; 3. Include one extractor for each piece of data passed to a constructor of the data type.


;;Program ::= Expression
;;Expression ::= Number
;;Expression ::= -(Expression , Expression)
;;Expression ::= zero? (Expression)
;;Expression ::= if Expression then Expression else Expression ::= Identifier
;;Expression ::= let Identifier = Expression in Expression



(define is-number-string?
  (lambda (str)
    (let ((length (string-length str))
          (num '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)))
      (let loop ((st str) (index 0))
        (cond
          [(<= length index) #t]
          [(member (string-ref str index) num)
           (loop st (add1 index))]
          [else
           #f])))))

(is-number-string? "123")


(define expression '())

(define const-exp?
  (lambda (node)
    (if (is-number-string? (node-chars))
        #t
        #f)))
(define const-exp
  (lambda (num)
    `(const ,num)))

;(const-exp 55)

;(define zero?-exp?
;  (lambda (lst)
;    (if 


