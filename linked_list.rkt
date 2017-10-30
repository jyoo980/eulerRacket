;; This is a file which implements a linked-list in Racket 
(define-struct node(val next))
;; LinkedList is one of:
;; - false
;; - (make-node Integer LinkedList)
