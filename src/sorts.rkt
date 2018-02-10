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
