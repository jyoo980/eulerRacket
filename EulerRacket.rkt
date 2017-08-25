;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname EulerRacket) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require racket/math)

;; =======================================================
;; PROBLEM:
;; Find the sum of all the multiples of 3 or 5 below 1000. 
;; =======================================================

;; Natural -> Natural
;; determines sum of the multiples of 3 or 5 below given n

(define (sum-3-5 n)
  (foldl + 0
         (filter (λ(n)(or (= (modulo n 3) 0)(= (modulo n 5) 0)))
                 (build-list (add1 n) identity))))

; (sum-3-5 1000)     ; answer

;; ===============================================================
;; PROBLEM:
;; Find the difference between the sum of the squares of the first  
;; one hundred natural numbers and the square of the sum.  
;; ===============================================================

;; Natural -> Natural
;; determines sum of the squares of the first 100 naturals and the square of the sum

(define (sum-sqr-diff n0)
  ;; sum is type: Natural, sum of numbers seen
  ;; sumsqr is type: Natural, sum of the squares of numbers seen
  (local [(define (fn-for-natural n sum sumsqr)
            (cond [(zero? n) (- (sqr sum) sumsqr)]
                  [else
                   (fn-for-natural (sub1 n) (+ n sum) (+ (sqr n) sumsqr))]))]
    (fn-for-natural n0 0 0)))

;; ===========================================================
;; PROBLEM:
;; Write a function that reverses a list, preferably in place. 
;; ===========================================================

;; (listof X) -> (listof X)
;; reverses list passed in

(define (reverse-lox lox0)
  ;; reversed is type: lox
  (local [(define (fn-for-lox lox reversed)
            (cond [(empty? lox) reversed]
                  [else
                   (fn-for-lox (rest lox) (cons (first lox) reversed))]))]
    (fn-for-lox lox0 empty)))


;; ===========================================================
;; PROBLEM:
;; write a function whcih interleaves 2 lists.
;; ===========================================================

;; (listof X) (listof X) -> (listof X)
;; creates a new list by taking elements interchangably from 2 lists

(define (interleave lox1_0 lox2_0)
  ;; select is type: boolean
  (local [(define (fn-for-lox lox1 lox2 select)
            (cond [(and (empty? lox1) (empty? lox2)) empty]
                  [(empty? lox1) lox2]
                  [(empty? lox2) lox1]
                  [else
                   (if (not (false? select))
                       (cons (first lox1) (fn-for-lox (rest lox1) lox2 false))
                       (cons (first lox2) (fn-for-lox lox1 (rest lox2) true)))]))]                      
    (fn-for-lox lox1_0 lox2_0 true)))

;; This is a naive, non tail-recursive approach.

;; ===========================================================================
;; PROBLEM:
;; Write a function: on_all that applies a function to every element of a list
;; ===========================================================================
;; (X -> Y) (listof X) -> (listof Y)
;; this is basically a map function, made it tail-recursive for runtime
(define (on_all fn lox0)
  ;; result is type: (listof Y)
  (local [(define (fn-for-lox fn lox result)
            (cond [(empty? lox) result]
                  [else
                   (fn-for-lox fn (rest lox) (append (list (fn (first lox))) result))]))]
    (fn-for-lox fn lox0 empty)))

;; ===========================================================================================
;; PROBLEM:
;; Write a function which returns the largest element of a list. Assume the list is non-empty
;; ===========================================================================================

(define (list-max lon0)
  ;; max is type: Number, interp. largest number seen in list so far
  (local [(define (fn-for-lon lon max)
            (cond [(empty? lon) max]
                  [else
                   (fn-for-lon (rest lon) (if (> (first lon) max) (first lon) max))]))]
    (fn-for-lon (rest lon0) (first lon0))))

;; ===========================================================================
;;  PROBLEM:
;;  Write a function which returns the list of the first 100 fibonacci numbers 
;; ===========================================================================

(define (fib n)
  (local [(define (fn-for-natural prev2 prev1 index)
            (cond [(= index 0) prev2]
                  [else
                   (fn-for-natural prev1 (+ prev2 prev1) (sub1 index))]))]
    (fn-for-natural 0 1 n)))

;; (build-list (add1 100) fib) ; tail-recursive answer

;; ===========================================================
;; PROBLEM:
;; Write a function consumes a number, returns a list of its digits. 
;; ===========================================================

(define (num->digits n0)
  (local [(define (inner-helper str len start end)
            (cond [(= (sub1 end) len) empty]
                  [else
                   (cons (string->number (substring str start end))
                         (inner-helper str len (add1 start) (add1 end)))]))]
    (inner-helper (number->string n0) (string-length (number->string n0)) 0 1)))

;; ===========================================================
;; PROBLEM:
;; Find the sum of the digits in the number 100!  
;; ===========================================================

(define (factorial num)
  ;; result is type: Natural, interp. the factorial
  (local [(define (fn-for-natural n result)
            (cond [(zero? n) result]
                  [else
                   (fn-for-natural (sub1 n) (* n result))]))]
    (fn-for-natural num 1)))

;; using num->digits function
;; (foldl + 0 (num->digits (factorial 100))) ; answer

;; ===========================================================
;; PROBLEM:
;; Design a function with returns the k-th element of a list
;; ===========================================================

;; (listof X) Natural -> X
(define (kth-from-list lox k)
  (cond [(empty? lox) (error "no items at that index")]
        [else
         (if (= 1 k)
             (first lox)
             (kth-from-list (rest lox) (sub1 k)))]))

;; ===========================================================
;; PROBLEM:
;; Remove duplicate elements in a list of Strings
;; ===========================================================

;; (listof String) -> (listof String)
(define (rm-duplicates los0)
  ;; result is type: (listof String), interp. the resulting list
  ;; prev is type: String, interp. the previous string
  (local [(define (fn-for-los los result prev)
            (cond [(empty? los) result]
                  [else
                   (fn-for-los (rest los) (if (not (string=? (first los) prev))
                                              (append result (list (first los)))
                                              result)
                               (first los))]))]
    (fn-for-los (rest los0) empty (first los0))))

;; =========================================================
;; PROBLEM:
;; Replicate the elements of a list a given number of times.
;; ===========================================================

(define (duplicate-n lox0 num)
  ;; count is type: Natural, interp, the number of times that each element will be duplicated
  ;; result is type; (listof X), the resulting list with duplicates
  (local [(define (fn-for-lox lox count result)
            (cond [(empty? lox) result]
                  [else
                   (if (zero? count)
                       (fn-for-lox (rest lox) num result)
                       (fn-for-lox lox (sub1 count) (append result (list (first lox)))))]))]
    (fn-for-lox lox0 num empty)))

;; ===============================================================
;; PROBLEM:
;; Write a function which consumes a list and returns true if it is
;; a palindrome, i.e. reads the same back and forth
;; ===============================================================

;; (listof X) -> Boolean
;; consume lox, return true if lox is a palindromic list, else false
(check-expect (pdrome? (list 1 2 3 2 1)) true)
(check-expect (pdrome? (list 1 2 2 1)) true)
(check-expect (pdrome? (list 1 3 3 1 4)) false)
(check-expect (pdrome? empty) true)

(define (pdrome? lox0)
  ;; reversed is type: (listof X), interp. the original list reversed
  (local [(define (fn-for-lox original reversed)
            (cond [(empty? original) true]
                  [else
                   (if (equal? (first original) (first reversed))
                       (fn-for-lox (rest original) (rest reversed))
                       false)]))]
    (fn-for-lox lox0 (reverse lox0))))
;
;A simpler implementation
;(define  (pdrome? lox)
;  (equal? lox (reverse lox)))

;; ===============================================================
;; PROBLEM:
;; Write a function which consumes a String and returns true if it
;; is a palindrome, i.e. reads the same back and forth
;; ===============================================================

;; String -> Boolean
;; consume str, return true if it is a palindrome, else false
(check-expect (string-pdrom? "bob") true)
(check-expect (string-pdrom? "hannah") true)
(check-expect (string-pdrom? "john") false)

(define (string-pdrom? str1)
  (local [(define (fn-for-string str)
            (pdrome? (string-array str)))
          
          (define (string-array str0)
            (local [(define (fn-for-str str start end)
                      (cond [(= end (add1 (string-length str))) empty]
                            [else
                             (cons (substring str start end)
                                   (fn-for-str str (add1 start) (add1 end)))]))]
              (fn-for-str str0 0 1)))]
    (fn-for-string str1)))


;; =========================================================================
;; PROBLEM:
;; Write a function which determines whether a given integer is prime or not
;; =========================================================================

;; Integer -> Boolean
;; consumes integer n, returns true iff the Integer is a prime
(check-expect (prime? 17) true)
(check-expect (prime? 1) false)
(check-expect (prime? 12) false)

(define (prime? integer0)
  (local [(define (trial-div integer factors)
            (cond [(= integer 1) false]
                  [(empty? factors) true]
                  [else
                   (if (= (modulo integer (first factors)) 0)
                       false
                       (trial-div integer (rest factors)))]))
          
          (define (factor-list num)
            (local [(define (fn-for-natural start end result)
                      (cond [(= start (add1 end)) result]
                            [else
                             (fn-for-natural (add1 start) end (append result (list start)))]))]
              (fn-for-natural 2 (floor (sqrt num)) empty)))]
    (trial-div integer0 (factor-list integer0))))  

