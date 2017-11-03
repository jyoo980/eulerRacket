;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname linked_list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; This is a file which implements a linked-list in Racket 
(define-struct node(val next))
;; LinkedList is one of:
;; - false
;; - (make-node Integer LinkedList)
;; interp. false represents a NULL node
;;         first field is the node's value 
;;         second field is the "next" pointer as LL

(define LL0 false)  
(define LL1 (make-node 1 LL0))  ;; 1->NULL
(define LL2 (make-node 2 LL1))  ;; 2->1->NULL
(define LL3 (make-node 3 LL2))  ;; 3->2->1->NULL

#;
(define (fn-for-linked-list ll)
  (cond [(false? ll) (...)]
        [else
         (... (node-val ll)
              (fn-for-linked-list (node-next ll)))]))