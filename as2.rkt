#lang racket
;; level-list returns a list of all the nodes on a desired tree level
(define (level-list tree level)
  (if (null? tree)
      (list)
      (cond
        [(and (list? (car tree))(eq? level 1))(list* (level-list(cdr tree)level))]
        [(and (not(list? (car tree)))(eq? level 1))(list* (car tree)(level-list (cdr tree)level))]
        [(and (not(list? (car tree)))(not(eq? level 1)))(list* (level-list (cdr tree)level))]
        [(and (list? (car tree))(not(eq? level 1)))(list* (level-list(car tree)(- level 1))(level-list(cdr tree)level))])))
;;Not sure how to get rid of the extra brackets

(level-list '(1 (2 (3 () ()) (4 () ())) (15 (4()()) ()) (16 () ())) 1)
(level-list '(1 (2 (3 () ()) (4 () ())) (15 (5()()) ()) (16 () ())) 2)
(level-list '(1 (2 (3 () ()) (4 () ())) (15 (5(17()())()) ()) (16 () ())) 3)

;;height function returns the maximum hight of the tree
(define (height tree)
  (if(null? tree)
     0
     (cond
       [(list? (car tree))(max(+ 1 (height(car tree)))(height(cdr tree)))]
       [else (height(cdr tree))])))


(height '(1 (2 (3 () ()) (4 () ())) (15 (4(17(12(13()())())())()) ()) (16 () ())))
(height '())
(height '(1(4()())(13()())))
(height '(1 (2 (3 () ()) (4 () ())) (15 (4()()) ()) (16 () ())))

;;returns true if a node is a leaf
(define (isTerminalNode node)
  (and(null? (car(cdr node)))(null? (car(cdr(cdr node))))))

;;returns true if it can sum up a path to get desired sum
(define (sum tree target)
  (if(null? tree)
     #f
     (if(list? (car tree))
        (or(sum(car tree) target)(sum(cdr tree) target))
        (if (isTerminalNode tree)
            (eq? (- target (car tree)) 0)
            (sum(cdr tree)(- target (car tree)))))))

(sum '(1 (2 (3 () ()) (4 () ())) (15 () ()) (16 () ())) 6)
(sum '(1 (2 (3 () ()) (4 () ())) (15 () ()) (16 () ())) 7)
(sum '(1 (2 (3 () ()) (4 () ())) (15 () ()) (16 () ())) 16)
(sum '(1 (2 (3 () ()) (4 () ())) (15 () ()) (16 () ())) 17)
(sum '(1 (2 (3 () ()) (4 () ())) (15 () ()) (16 () ())) 0)
(sum '(1 (2 (3 () ()) (4 () ())) (15 () ()) (16 () ())) 20)
