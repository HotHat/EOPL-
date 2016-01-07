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


;; Exercise 1.21 [**] (product sos1 sos2), where sos1 and sos2 are each a list of symbols without
;; repetitions, returns a list of 2-lists
;; than represents the Cartesian product of sos1 and sos2. The 2-lists may appear in any order.


(define product-l
  (lambda (sos1 sos2 sos3)
    (if (null? sos1)
        '()
        (if (null? sos2)
            (product-l (cdr sos1) sos3 sos3)
            (cons (list (car sos1) (car sos2))
                  (product-l sos1 (cdr sos2) sos3))))))


(product-l '(a b) '(x y z) '(x y z))

(define product
  (lambda (sos1 sos2)
    (product-l sos1 sos2 sos2)))

(product '(a b c) '(x y z))


;; Exercise 1.22 [**] (filter-in pre lst) returns the list of those elements in lst that satisfy the predicate pred.

(define filter-in
  (lambda (pre lst)
    (if (null? lst)
        '()
        (if (pre (car lst))
            (cons (car lst)
                  (filter-in pre (cdr lst)))
            (filter-in pre (cdr lst))))))

(filter-in number? '(a 2 (1 3) b 7))
(filter-in symbol? '(a (b c) 17 foo))
       

;; Exercise 1.23 [**] (list-index rped lst) returns the 0-based position of teh first element of lst that satisfies
;; the predicate pred. If no element of lst satisfies the predicate, then list-index returns #f.




(define list-index-n
  (lambda (pre lst n)
    (if (null? lst)
        #f
        (if (pre (car lst))
            n
            (list-index-n pre (cdr lst) (+ n 1))))))

(define list-index
  (lambda (pre lst)
    (list-index-n pre lst 0)))

(list-index number? '(a 2 (1 3) b 7))
(list-index symbol? '(a (b c) 17 foo))
(list-index symbol? '(1 2 (a b) 3))



;; Exercise1.24 [**] (every? pred lst) return #f if any element of lst fails to sitisyf pred, and returns #f otherwise.

(define every? 
  (lambda (pre lst)
    (if (null? lst)
        #t
        (if (pre (car lst))
            (every? pre (cdr lst))
            #f))))
(every? number? '(a b c 3 e))
(every? number? '(1 2 3 4 5))

;; Exercise1.25 [**] (exists? pred lst) returns #f if any element of lst satisfies pre, and returns #f otehrwise

(define exists?
  (lambda (pre lst)
    (if (null? lst)
        #f
        (if (pre (car lst))
            #t
            (exists? pre (cdr lst))))))

(exists? number? '(a b c 3 e))
(exists? number? '(a b c d e))


;; Exercise1.26 [**] (up lst) removes a pair of parentheses from each otp-level element of lst. If a top-level element
;; is not a lst, it is incluede in teh result, as is. The value of ( up (down lst)) is equivalent to lst, but (down (up lst)
;; is not necessarily lst. (See exercise 1.17.)


(define up-ctx
  (lambda (lst ctx)
    (cond 
      [(null? lst) 
       (cond
         [(null? ctx) '()]
         [else 
          (cons (car ctx) (up-ctx lst (cdr ctx)))])]
      
      [(null? ctx)
       (if (pair? (car lst))
           (up-ctx (cdr lst) (car lst))
           (cons (car lst) (up-ctx (cdr lst) ctx)))]
      
      [else
       (cons (car ctx) (up-ctx lst (cdr ctx)))])))

(define up
  (lambda (lst)
    (up-ctx lst '())))

;(up-ctx '((1 (3) 2) (3 4) ((5 8 (7)))) '())
;(up-ctx '((x (y)) z) '())

(up '((1 2) (3 4)))  ;; ==> (1 2 3 4)
(up '((x (y)) z))    ;; ==> (x y z)

;; Exercise 1.27 [**] (flatten slist) returns a list of the symbols contained in
;; slist in the order in which they occur rwhen slist is prined. Intuitively, flatten
;; removes all the inner parentheses from its argument.

(define flatten
  (lambda (lst)
    (if (null? lst)
        '()
        (if (pair? (car lst))
            (up (cons (flatten (car lst)) (flatten (cdr lst))))
            (if (not (null? (car lst)))
                (cons (car lst) (flatten (cdr lst)))
                (flatten (cdr lst)))))))

(flatten '(a b c))                 ;; ==> '(a b c)
(flatten '((a) () (b ()) () (c)))  ;; ==> '(a b c)
(flatten '((a b) c (((d)) e)))     ;; ==> '(a b c d e)
(flatten '(a () (((b))) (() (c)))) ;; ==> '(a b c)


;; Exercise1.28 [**](merge loi1 loi2) , where loi1 and loi2 are lists of integers that
;; are sorted in ascending order, returns sorted list of all the integers in loi1 and loi2.


(define merge-ctx
  (lambda (loi1 loi2 cmp)
    (cond
      [(null? loi1)
       (if (null? loi2) 
           (cons cmp loi2)
           (if (< cmp (car loi2))
               (cons cmp loi2)
               (cons (car loi2) (cons cmp (cdr loi2)))))]
       
      [(null? loi2)
       (if (null? loi1)
           (cons cmp loi1)
           (if (< cmp (car loi1))
               (cons cmp loi1)
               (cons (car loi1) (cons cmp (cdr loi1)))))]
      [else
       (let ((s1 (car loi1))
             (s2 (car loi2)))
         (if (< s1 s2)  ;;  s1 < s2
             (if (< cmp s1)  ;; cmp < s1 < s2  
                 (cons cmp (cons s1  (merge-ctx (cdr loi1) (cdr loi2) s2)))
                 (if (< cmp s2)               ;; s1 < cmp < s2
                     (cons  s1  (cons cmp (merge-ctx (cdr loi1) (cdr loi2) s2)))
                     ;; s1 < s2 < cmp
                     (cons s1 (cons s2 (merge-ctx (cdr loi1) (cdr loi2) cmp)))))
             ;; s2 < s1
             (if (< cmp s2) ;; cmp < s2 < s1
                 (cons  cmp (cons s2 (merge-ctx (cdr loi1) (cdr loi2) s1)))
                 (if (< cmp s1) ;; s2 < cmp < s1
                      (cons s2 (cons cmp (merge-ctx (cdr loi1) (cdr loi2) s1)))
                      ;; s2 < s1 < cmp
                       (cons s2 (cons s1 (merge-ctx (cdr loi1) (cdr loi2) cmp)))))))])))

(merge-ctx '(1 4) '(1 2 8) 1)   ;; ==> '(1 1 1 2 4 8)

(define merge
  (lambda (loi1 loi2)
    (merge-ctx (cdr loi1) loi2 (car loi1))))
(merge '(1 4) '(1 2 8))  ;; ==> '(1 1 2 4 8)
(merge '(35 62 81 90 91) '(3 83 85 90)) ;; ==> '(3 35 62 81 83 85 90 90 91)


;; Exercise 1.29 [**] (sort loi) retruns a list of the elements of loi in ascending oredr.

(define sort-insert   ;; loi is a list in ascending order
  (lambda (loi s)
    (if (null? loi)
        (cons s loi)
        (if (< s (car loi))
            (cons s loi)
            (cons (car loi) (sort-insert (cdr loi) s))))))

;(sort-insert '(2 5) 3) ;; ==> '(2 3 5)
;(sort-insert '(2 5) 8) ;; ==> '(2 5 8)

(define sort-ctx
  (lambda (loi ctx)
    (if (null? loi)
        ctx
        (sort-ctx (cdr loi) (sort-insert ctx (car loi))))))
;(sort-ctx '(8 2 5 2 3) '()) ;; ==>  '(2 2 3 5 8)

(define sort
  (lambda (loi)
    (sort-ctx loi '())))

(sort '(8 2 5 2 3)) ;; ==> '(2 2 3 5 8)


;; Exercise 1.30 [**] (sort/predicate pre loi) returns a list of elements sorted by the predicate.

(define sort-insert/predicate   ;; loi is a list in ascending order
  (lambda (pred loi s)
    (if (null? loi)
        (cons s loi)
        (if (pred s (car loi))
            (cons s loi)
            (cons (car loi) (sort-insert/predicate pred (cdr loi) s))))))    

(define sort/predicate
  (lambda (pred loi)
    (letrec ((sort/predicate-ctx
              (lambda (loi ctx)
                (if (null? loi)
                    ctx
                    (sort/predicate-ctx (cdr loi) (sort-insert/predicate pred ctx (car loi)))))))
             (sort/predicate-ctx loi '()))))

(sort/predicate < '(8 2 5 2 3)) ;; ==>'(2 2 3 5 8)
(sort/predicate > '(8 2 5 2 3)) ;; ==>'(8 5 3 2 2)

             
;; Exercise 1.31 [*] Write the following procedures for calculating on a bintree(definition 1.1.7):
;; leaf and interior-node, which build bintrees, leaf?, which tests whether a bintree is a leaf, and
;; lson, rson, and contents-of, which extract the components of a node. contents-of should work on both
;; leaves and interior nodes.

;; Defination 1.1.7 (binary tree)
;; binary-search-tree ::= Int | (Symbol Binary-search-tree Binary-search-tree)

(define leaf 
  (lambda (s) ;; s Int
    (if (number? s)
        s
        (error "Tree leaf must be numbere"))))

(define interior-node 
  (lambda (s lson rson)
    `(,s ,lson ,rson)))


(define leaf? 
  (lambda (tree)
    (number? tree)))

(define lson 
  (lambda (tree)
   (if (not (leaf? tree))
       (cadr tree)
       (error "Error is a leaf"))))

(define rson 
 (lambda (tree)
   (if (not (leaf? tree))
       (caddr tree)
       (error "Error is a leaf"))))

(define contents-of   ;;; ????
  (lambda (tree)
    (car tree)))

(leaf? (leaf 5))

(define tree (interior-node 'red
      (interior-node 'bar
        (leaf 26)
        (leaf 12))
      (interior-node 'red
        (leaf 11)
        (interior-node 'quux
          (leaf 117)
          (leaf 14)))))
tree       
(leaf? tree)
(lson tree)
(rson tree)
(contents-of tree)   ;; ??

;; Exercise [*] Write a procedure double-tree that kates a bintree, as prepresented
;; in defination 1.17, and produces another bintree like the otriginal, but with all the 
;; integers in the leaves doubles.

(define leaf->number 
  (lambda (leaf)
    leaf))
(define tree-sym
  (lambda (tree)
    (car tree)))


(define double-tree
  (lambda (tree)
    (if (leaf? tree)
        (leaf (+ (leaf->number tree) (leaf->number tree)))
        (let ((sym (tree-sym tree))
              (l (lson tree))
              (r (rson tree)))
          (interior-node sym (double-tree l) (double-tree r))))))

tree                ;; ==> '(red (bar 26 12) (red 11 (quux 117 14)))
(double-tree tree)  ;; ==> '(red (bar 52 24) (red 22 (quux 234 28)))



;; Exercise 1.33 [**]Write a procedure mark-leakes-with-red-depth that takes a bintree (definiton 1.1.7),
;; and produces a bintree of the same shape as the original, except that in the new tree, each leaf 
;; contains the integer of nodes between it and the root that contain the symbol red. For example, the expression

;;(mark-leaves-with-red-depth
;;    (interior-node ’red
;;      (interior-node ’bar
;;        (leaf 26)
;;        (leaf 12))
;;      (interior-node ’red
;;        (leaf 11)
;;        (interior-node ’quux
;;          (leaf 117)
;;          (leaf 14))
;;which is written using the procedures defined in exercise 1.31, should return the bin- tree
;;  (red
;;    (bar 1 1)
;;    (red 2 (quux 2 2)))


(define mark-leaves-with-red-depth-ctx
  (lambda (tree ctx)
    (if (leaf? tree)
        (leaf ctx)
         (let ((sym (tree-sym tree))   ;; tree-sym defined in 1.32
              (l (lson tree))
              (r (rson tree)))
           (if (eqv? sym 'red)
               (interior-node sym (mark-leaves-with-red-depth-ctx l (+ ctx 1)) (mark-leaves-with-red-depth-ctx r (+ ctx 1)))
               (interior-node sym (mark-leaves-with-red-depth-ctx l ctx) (mark-leaves-with-red-depth-ctx r ctx)))))))

;(mark-leaves-with-red-depth-ctx tree 0)  ;; ==> (red (bar 1 1) (red 2 (quux 2 2)))

(define mark-leaves-with-red-depth
  (lambda (tree)
    (mark-leaves-with-red-depth-ctx tree 0)))

(mark-leaves-with-red-depth
    (interior-node 'red
      (interior-node 'bar
        (leaf 26)
        (leaf 12))
      (interior-node 'red
        (leaf 11)
        (interior-node 'quux
          (leaf 117)
          (leaf 14)))))  ;; ==> (red (bar 1 1) (red 2 (quux 2 2)))


;; Exercise 1.34 [***]Write a procedure path that takes an integer n and a binary search tree bst (page 10) 
;; that contains the integer n, and returns a list of lefts and rights showing how to find the node containing n.
;; If n is found at the root, it returns the empty list.

;;> (path 17 ’(14 (7 () (12 () ())) (26 (20 (17 () ())
;;                          ())
;;                      (31 () ()))))
;;  (right left left


