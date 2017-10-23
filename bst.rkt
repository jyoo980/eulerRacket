;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname bst) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; bst.rkt - Binary Search Tree implementation in Racket

(define-struct node(key val left right))
;; BST is one of:
;; - false
;; - (make-node Integer String BST BST)
;; interp. key is value Integer
;;         value is the value stored in the node
;;         left is the left child as type BST
;;         right is right child as type BST
;; INVARIANT: left key < parent key, and right key > parent key

(define BSTF false)
(define BST1 (make-node 1 "one" false false))
(define BST3 (make-node 3 "three" false false))
(define BST2 (make-node 2 "two" false BST3))
(define BST0 (make-node 0 "zero" BST1 BST2))

#;
(define (fn-for-bst bst)
  (cond [(false? bst) (...)]
        [else
         (... (fn-for-bst (node-left bst))
              (fn-for-bst (node-right bst)))]))

;; BST -> Natural
;; count the number of nodes in the given bst
(check-expect (count BSTF) 0)
(check-expect (count BST1) 1)
(check-expect (count BST2) 2)
(check-expect (count BST0) 4)

; (define (count bst) 0) ; stub

;<template from Binary Search Tree>
(define (count bst)
  (cond [(false? bst) 0]
        [else
         (add1
          (+ (count (node-left bst))
             (count (node-right bst))))]))

;; BST -> Natural
;; produce the height of the given bst
(check-expect (height BSTF) -1)
(check-expect (height BST1) 0)
(check-expect (height BST2) 1)
(check-expect (height BST0) 2)

; (define (height bst) 0) ; stub

;<template from Binary Search Tree>
(define (height bst)
  (cond [(false? bst) -1]
        [else
         (add1 (max (height (node-left bst))
                    (height (node-right bst))))]))                      

;; BST Natural -> String | False
;; search for a node with the given key, if found, produce value, else false
(check-expect (search BSTF 0) false)
(check-expect (search BSTF 1) false)
(check-expect (search BST1 1) "one")
(check-expect (search BST0 3) "three")

; (define (search bst key) false) ; stub

;<template from Binary Search Tree>
(define (search bst key)
  (cond [(false? bst) false]
        [(= key (node-key bst)) (node-value bst)]
        [(< key (node-key bst)) (search (node-left bst) key)]
        [(> key (node-key bst)) (search (node-right bst) key)]))

