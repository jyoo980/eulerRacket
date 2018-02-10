;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname sorts) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; (listof Number) -> (listof Number)
;; quicksort
(check-expect (qsort empty) empty)
(check-expect (qsort (list 1)) (list 1))
(check-expect (qsort (list 1 2)) (list 1 2))
(check-expect (qsort (list 5 4 3 2 1)) (list 1 2 3 4 5))
(check-expect (qsort (list 1 1 2 2)) (list 1 1 2 2))

; (define (qsort empty) empty) ; stub

(define (qsort lon)
  (cond [(empty? lon) empty]
        [else
         (append (qsort (filter (λ(n)(<= n (first lon))) (rest lon)))
                 (list (first lon))
                 (qsort (filter (λ(n)(> n (first lon))) (rest lon))))]))

;; (listof Number) Natural -> (listof Number)
;; takes the first n elements of a list 
(check-expect (take empty 1) empty)
(check-expect (take (list 1 2) 1) (list 1))
(check-expect (take (list 1 2 3) 2) (list 1 2))

 ; (define (take lon n) empty) ; stub

(define (take lon0 n0)
  (local [(define (fn-for-lon lon result n)
            (cond [(zero? n) result]
                  [(empty? lon) result]
                  [else
                   (fn-for-lon (rest lon) (append result (list (first lon))) (sub1 n))]))]
    (fn-for-lon lon0 empty n0)))

;; (listof Number) -> (listof Number)
;; take the last n elements of a list
(check-expect (drop empty 1) empty)
(check-expect (drop (list 1 2) 1) (list 2))
(check-expect (drop (list 1 2 3) 2) (list 2 3))

; (define (drop lon n) empty) ; stub

(define (drop lon0 n0)
  (local [(define (fn-for-lon lon result n)
            (cond [(zero? n) result]
                  [(empty? lon)  result]
                  [else
                   (fn-for-lon (rest lon) (cons (first lon) result) (sub1 n))]))]
    (fn-for-lon (reverse lon0) empty n0)))

;; (listof Number) (listof Number) -> (listof Number)
;; merges two lists
(check-expect (merge empty empty) empty)
(check-expect (merge empty (list 1 2)) (list 1 2))
(check-expect (merge (list 1 2) empty) (list 1 2))
(check-expect (merge (list 1 2) (list 2 3)) (list 1 2 2 3))
(check-expect (merge (list 1 2) (list 3 4)) (list 1 2 3 4))
(check-expect (merge (list 3 4) (list 1 2)) (list 1 2 3 4))

; (define (merge lon1 lon2) empty) ; stub

(define (merge lon1 lon2)
  (cond [(and (empty? lon1) (not (empty? lon2))) lon2]
        [(and (not (empty? lon1)) (empty? lon2)) lon1]
        [(and (empty? lon1) (empty? lon2)) empty]
        [else
         (if (>= (first lon1) (first lon2))
             (cons (first lon2) (merge lon1 (rest lon2)))
             (cons (first lon1) (merge (rest lon1) lon2)))]))

;; (listof Number) -> (listof Number)
;; mergesort
(check-expect (mergesort empty) empty)
(check-expect (mergesort (list 1)) (list 1))
(check-expect (mergesort (list 1 2)) (list 1 2))
(check-expect (mergesort (list 5 4 3 2 1)) (list 1 2 3 4 5))
(check-expect (mergesort (list 1 1 2 2)) (list 1 1 2 2))

; (define (mergesort lon) empty) ; stub

(define (mergesort lon)
  (cond [(empty? lon) empty]
        [else
         (merge (mergesort (take lon (/ (length lon) 2)))
                (mergesort (drop lon (/ (length lon) 2))))]))