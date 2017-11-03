;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname EulerRacket) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require racket/math)
(require racket/base)

;; =======================================================
;; PROBLEM:
;; Find the sum of all the multiples of 3 or 5 below 1000. 
;; =======================================================

;; Natural -> Natural
;; determines sum of the multiples of 3 or 5 below given n
(check-expect (sum-3-5 0) 0)
(check-expect (sum-3-5 10) (+ 3 5 6 9))
(check-expect (sum-3-5 20) (+ 3 5 6 10 9 12 15 18))

(define (sum-3-5 n)
  (foldl + (- 0 n)
         (filter (λ(n)(or (= (modulo n 3) 0)(= (modulo n 5) 0)))
                 (build-list (add1 n) identity))))

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
(check-expect (reverse-lox empty) empty)
(check-expect (reverse-lox (list 1)) (list 1))
(check-expect (reverse-lox (list 1 2 3 4)) (list 4 3 2 1))

(define (reverse-lox lox0)
  ;; reversed is type: lox
  (local [(define (fn-for-lox lox reversed)
            (cond [(empty? lox) reversed]
                  [else
                   (fn-for-lox (rest lox) (cons (first lox) reversed))]))]
    (fn-for-lox lox0 empty)))


;; ===========================================================
;; PROBLEM:
;; write a function which interleaves 2 lists.
;; ===========================================================

;; (listof X) (listof X) -> (listof X)
;; creates a new list by taking elements interchangably from 2 lists
(check-expect (interleave empty empty) empty)
(check-expect (interleave (list 1) empty) (list 1))
(check-expect (interleave empty (list 2)) (list 2))
(check-expect (interleave (list 1 2) (list "a" "b" "c" "d")) (list 1 "a" 2 "b" "c" "d"))
(check-expect (interleave (list "a" "b" "c" "d") (list 1 2)) (list "a" 1 "b" 2 "c" "d"))

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



;; ==============================================================================
;; PROBLEM:
;; Write a function which consume a list and a number, n, skips every nth element
;; ==============================================================================

;; (listof X) Natural -> (listof X)
(check-expect (skip-n empty 4) empty)
(check-expect (skip-n (list 1 2) 4) (list 1 2))
(check-expect (skip-n (list 1 2 3 4 5) 2) (list 1 3 5))
(check-expect (skip-n (list 1 2 3 4 5) 3) (list 1 2 4 5))

(define (skip-n lon0 n0)
  (local [(define (fn-for-lon lon count result)
            (cond [(empty? lon) result]
                  [else
                   (if (= count 1)
                       (fn-for-lon (rest lon) n0 result)
                       (fn-for-lon (rest lon) (sub1 count) (append result (list (first lon)))))]))]
    (fn-for-lon lon0 n0 empty)))            


;; ===========================================================================
;; PROBLEM:
;; Write a function: on_all that applies a function to every element of a list
;; ===========================================================================
;; (X -> Y) (listof X) -> (listof Y)
;; this is basically a map function, made it tail-recursive for runtime
(check-expect (on_all add1 empty) empty)
(check-expect (on_all add1 (list 0 1 2)) (list 1 2 3))
(check-expect (on_all odd? (list 1 2 3 4)) (list true false true false))
(check-expect (on_all (λ(s)(string-append s "s")) (list "apple" "no" "yep"))
              (list "apples" "nos" "yeps"))

(define (on_all fn lox0)
  ;; result is type: (listof Y)
  (local [(define (fn-for-lox fn lox result)
            (cond [(empty? lox) result]
                  [else
                   (fn-for-lox fn (rest lox) (append result (list (fn (first lox)))))]))]
    (fn-for-lox fn lox0 empty)))

;; ===========================================================================================
;; PROBLEM:
;; Write a function which returns the largest element of a list. Assume the list is non-empty
;; ===========================================================================================

;; (listof Natural) -> Natural
(check-expect (list-max (list 1 2)) 2)
(check-expect (list-max (list 1 2 -4 -4 24 5 -3 0)) 24)

(define (list-max lon0)
  ;; max is type: Number, interp. largest number seen in list so far
  (local [(define (fn-for-lon lon max)
            (cond [(empty? lon) max]
                  [else
                   (fn-for-lon (rest lon) (if (> (first lon) max) (first lon) max))]))]
    (fn-for-lon (rest lon0) (first lon0))))

;; ===========================================================================================
;; PROBLEM:
;; Write a function which computes the average of a list
;; ===========================================================================================

;; (listof Integer) -> Number
(check-expect (average empty) 0)
(check-expect (average (list 1)) 1)
(check-expect (average (list 1 -3 0 1)) (/ (+ 1 -3 0 1) 4))

;; Tail-recursive solution
#;
(define (average lon0)
  (local [(define (fn-for-lon lon count sum)
            (cond [(empty? lon) (if (zero? sum) 0 (/ sum count))]
                  [else
                   (fn-for-lon (rest lon) (add1 count) (+ (first lon) sum))]))]
    (fn-for-lon lon0 0 0)))

(define (average lon)
  (local [(define count 0) (define sum 0)]
    (begin
      (for-each (λ(n)(begin (set! count (add1 count)) (set! sum (+ sum n))))
                lon)
      (if (zero? count) 0 (/ sum count)))))


;; ===========================================================================
;;  PROBLEM:
;;  Write a function which checks for list equality
;; ===========================================================================

;; (listof X) (listof X) -> Boolean
(check-expect (list-equal? empty empty) true)
(check-expect (list-equal? (list 1) empty) false)
(check-expect (list-equal? empty (list 1)) false)
(check-expect (list-equal? (list 1 2 4) (list 1 2 4)) true)
(check-expect (list-equal? (list 1 2 4) (list 1 2 3)) false)
(check-expect (list-equal? (list 4 2 3) (list 1 2 3)) false)

;; Method 1 - Tail-recursion

(define (list-equal? lon0 lon1)
  (cond [(and (empty? lon0) (empty? lon1)) true]
        [(or (empty? lon0) (empty? lon1)) false]
        [else
         (if (equal? (first lon0) (first lon1))
             (list-equal? (rest lon0) (rest lon1))
             false)]))

;;; Method 2 - Mutation
;(define (list-equal? lox0 lox1)
;  (local [(define isEqual false)]
;    (if (not (= (length lox0) (length lox1)))
;        false
;        (begin
;          (for-each (λ(a b)(set! isEqual (equal? a b))) lox0 lox1)
;          isEqual))))

;; ===========================================================================
;;  PROBLEM:
;;  Write a function which returns the list of the first 100 fibonacci numbers 
;; ===========================================================================

;; Natural -> (listof Natural)
(check-expect (fib 0) 0)
(check-expect (fib 3) 2)

(define (fib n)
  (local [(define (fn-for-natural prev2 prev1 index)
            (cond [(= index 0) prev2]
                  [else
                   (fn-for-natural prev1 (+ prev2 prev1) (sub1 index))]))]
    (fn-for-natural 0 1 n)))

;; (build-list (add1 100) fib) ; tail-recursive answer

;; =================================================================
;; PROBLEM:
;; Write a function consumes a number, returns a list of its digits. 
;; =================================================================

;; Natural -> (listof Natural)
(check-expect (num->digits 0) (list 0))
(check-expect (num->digits 123) (list 1 2 3))
(check-expect (num->digits 24423243) (list 2 4 4 2 3 2 4 3))

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
(check-expect (factorial 0) 1)
(check-expect (factorial 10) (* 10 9 8 7 6 5 4 3 2 1))
(check-expect (factorial 100) (foldl * 1 (remove 0 (build-list 101 identity))))

;; Tail-recursive version
#;
(define (factorial num)
  ;; result is type: Natural, interp. the factorial
  (local [(define (fn-for-natural n result)
            (cond [(zero? n) result]
                  [else
                   (fn-for-natural (sub1 n) (* n result))]))]
    (fn-for-natural num 1)))

;; General-recursive version
#;
(define (factorial num)
  (cond [(zero? num) 1]
        [else
         (* num (factorial (sub1 n)))]))

;; Function-pointer version
#;
(define (factorial num)
  (foldl * 1 (remove 0 (build-list (add1 num) identity))))

;; Version using mutation
(define (factorial n)
  (local [(define product 1)]
    (begin
      (for-each (λ(n)(set! product (* n product)))
                (remove 0 (build-list (add1 n) identity)))
      product)))  

;; using num->digits function
;; (foldl + 0 (num->digits (factorial 100))) ; answer
;; built-in methods
;; (foldl * 1 (remove 0 (build-list 101 identity))))

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
(check-expect (rm-duplicates (list "a" "a" "b" "c" "c" "c" "c" "cc" "d")) (list  "b" "c" "cc" "d"))

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

#;
(define (prime? integer0)
  (local [(define (trial-div integer factors)
            (cond [(or (= integer 1) (= integer 0)) false]
                  [(empty? factors) true]
                  [else
                   (if (= (modulo integer (first factors)) 0)
                       false
                       (trial-div integer (rest factors)))]))
          
          (define (factor-list num)
            (local [(define (fn-for-natural start end result)
                      (cond [(zero? num) empty]
                            [(= start (add1 end)) result]
                            [else
                             (fn-for-natural (add1 start) end (append result (list start)))]))]
              (fn-for-natural 2 (floor (sqrt num)) empty)))]
    (trial-div integer0 (factor-list integer0))))

;<Version using first-order functions, slower but more consise>
#;
(define (prime? n)
  (and (not (zero? n)) (not (= n 1))
       (andmap (λ(num)(not (zero? (modulo n num))))
               (remove 0 (remove 1 (build-list n identity))))))

;; =========================================================================
;; PROBLEM:
;; Write a function that generates a list of the first n prime numbers
;; =========================================================================

;; Natural -> (listof Natural)
;; generates a list of prime numbers up to a given n
(check-expect (get-primes 10) (list 2 3 5 7))
(check-expect (get-primes 20) (list 2 3 5 7 11 13 17 19))

(define (get-primes n)
  (filter prime? (build-list (add1 n) identity)))

;; =========================================================================
;; PROBLEM:
;; Write a function which determines whether two words are anagrams of each
;; other. No duplicate letters may be included
;; =========================================================================

;; String String -> Boolean
;; produce true if two strings are anagrams of one another, else false
(check-expect (anagram? "tommarvoloriddle" "iamlordvoldemort") true)
(check-expect (anagram? "john" "cena") false)

(define (anagram? str0 str1)
  (local [(define (check-all-los los0 str)
            (cond [(empty? los0) true]
                  [else
                   (if (string-contains? (first los0) str)
                       (check-all-los (rest los0) str)
                       false)]))
                                     
          (define (string->los str)
            (local [(define (fn-for-string str start end result)
                      (cond [(= end (add1 (string-length str))) result]
                            [else
                             (fn-for-string str (add1 start) (add1 end)
                                            (append result (list (substring str start end))))]))]
              (fn-for-string str 0 1 empty)))]
    (check-all-los (string->los str0) str1)))

;; ===============================================================
;; PROBLEM:
;; Provide implementations for the following first-order functions
;;     - map
;;     - filter
;;     - fold (tail and non tail-recursive)
;;     - andmap
;;     - ormap
;; ===============================================================

;; (X -> Y) (listof X) -> (listof Y)
;; the common map function, called "transform_all" here

(define (transform_all fn lox0)
  (local [(define (fn-for-lox lox fn result_list)
            (cond [(empty? lox) result_list]
                  [else
                   (fn-for-lox (rest lox) fn
                               (append result_list (list (fn (first lox)))))]))]
    (fn-for-lox lox0 fn empty)))

;; (X -> Boolean) (listof X) - > (listof X)
;; common filter function, called "satisfy" here

(define (satisfy pred? lox0)
  (local [(define (fn-for-lox pred lox result_list)
            (cond [(empty? lox) result_list]
                  [else
                   (fn-for-lox pred (rest lox) (update-result pred result_list (first lox)))]))
          (define (update-result pred lox x)
            (if (pred x) (append lox (list x)) lox))]
    (fn-for-lox pred? lox0 empty)))

;; (X Y -> Y) Y (listof X) -> (listof Y)
;; non tail-recursive fold, called "fold_right"

(define (fold_right fn b lox)
  (cond [(empty? lox) b]
        [else
         (fn (first lox)
             (fold_right fn b (rest lox)))]))

;; tail-recursive fold, called "fold_left"
(define (fold_left fn b lox0)
  (local [(define (fn-for-lox fn b lox result)
            (cond [(empty? lox) result]
                  [else
                   (fn-for-lox fn b (rest lox) (fn (first lox) result))]))]
    (fn-for-lox fn b lox0 b)))

;; (X -> Boolean) (listof X) -> Boolean
;; return true iff all elements satisfy the predicate

(define (all_true? pred? lox0)
  (local [(define (fn-for-lox lox pred result)
            (cond [(empty? lox) result]
                  [else
                   (fn-for-lox (rest lox) pred (and (first lox) result))]))]
    (fn-for-lox lox0 pred? true)))

;; (X -> Boolean) (listof X) -> Boolean
;; return true if at least one of the elements satisfy the predicate

(define (one_true? pred? lox0)
  (local [(define (fn-for-lox lox pred result)
            (cond [(empty? lox) result]
                  [else
                   (fn-for-lox (rest lox) pred (or (first lox) result))]))]
    (fn-for-lox lox0 pred? false)))

;; ================================================================
;; PROBLEM:
;; Provide an implementation of a function which consumes two lists
;; and produces their set intersection
;; ================================================================

(define (intersection lox0 lox1)
  (local [(define (fn-for-lox loxa loxb intersc)
            (cond [(or (empty? loxa) (empty? loxb)) empty]
                  [else
                   (if (member? (first loxa) loxb)
                       (fn-for-lox (rest loxa) loxb (cons (first loxa) intersc))
                       (fn-for-lox (rest loxa) loxb intersc))]))]
    (fn-for-lox lox0 lox1 empty)))
;; ===============================================
;; PROBLEM:
;; add up numbers in a list multiple ways
;; ===============================================

;; (listof Integer) -> Integer
;; computes sum of integers in list
(check-expect (sum empty) 0)
(check-expect (sum (list 1)) 1)
(check-expect (sum (list 1 2 3)) (+ 1 2 3))
(check-expect (sum (list -4 2 1 4199 -3)) (+ -4 2 1 4199 -3))

#;
(define (sum lon)
  (foldl + 0 lon))    ;; One liner
#;
(define (sum lon)
  (cond [(empty? lon) 0]
        [else
         (+ (first lon)
            (sum (rest lon)))]))    ;; classic
#;
(define (sum lon0)
  (local [(define (fn-for-lon lon result)
            (cond [(empty? lon) result]
                  [else
                   (fn-for-lon (rest lon) (+ result (first lon)))]))]  ;; making use of tail-recursion
    (fn-for-lon lon0 0)))
(define (sum lon0)
  (local [(define sum 0)]
    (begin
      (for-each  (λ(n)(set! sum (+ n sum))) lon0)
      sum)))


;; ================================================================
;; PROBLEM:
;; Design a function which consumes a list and produces a list of
;; their sublists. An example is shown below:
;; (list a b c d e f) -> (list (a b) (c d) (e f))
;; ================================================================

(define (pair-elements lox0)
  ;; second? is type: boolean, interp. true if at second element
  ;; sub_result is type: (listof X)
  (local [(define (fn-for-lox lox second? sub_result)
            (cond [(empty? lox) sub_result]
                  [else
                   (if (equal? second? true)
                       (fn-for-lox (rest lox) false (append (list (first lox)) (list sub_result)))
                       (fn-for-lox (rest lox) false sub_result))]))]
    (fn-for-lox lox0 false empty)))

;; ================================================================== 
;; PROBLEM:
;; Design a function which consumes a list, an element, and a posn.
;; and returns a new list with the element inserted at that position
;; ==================================================================

(define (insert e0 lox0 posn0)
  (local [(define (fn-for-lox e lox posn new)
            (cond [(or (empty? lox) (= posn 1)) (if (empty? lox)
                                                    (append new (list e))
                                                    (append (append new (list e)) lox))]
                  [else
                   (fn-for-lox e (rest lox) (sub1 posn) (append new (first lox)))]))]
    (fn-for-lox e0 lox0 posn0 empty)))

;; ======================================================================
;; PROBLEM:
;; Design a function which consumes two strings and returns the number of
;; identical characters starting from the first letter.
;; ======================================================================

;; String String -> Natural
;; returns number of identical letters starting from str1 and str2
(check-expect (same-char "apples" "") 0)
(check-expect (same-char "" "apples") 0)
(check-expect (same-char "apple" "apples") 5)
(check-expect (same-char "john" "jonathan") 2)
(check-expect (same-char "yes" "no") 0)
(check-expect (same-char "A" "a") 0)
(check-expect (same-char "hello" "hello") 5)
(check-expect (same-char "hello" "Hello") 0)

(define (same-char str0 str1)
  (local [(define (fn-for-los los0 los1 count)
            (cond [(or (empty? los0) (empty? los1)) count]
                  [else
                   (if (not (equal? (first los0) (first los1)))
                       count
                       (fn-for-los (rest los0) (rest los1) (add1 count)))]))
          
          (define (string->List str0)
            ;; result is type: (listof String), interp. the string as list
            (local [(define (fn-for-string str start end len result)
                      (if (= end (add1 len))
                          result
                          (fn-for-string str (add1 start) (add1 end) len
                                         (append result (list (substring str start end))))))]
              (fn-for-string str0 0 1 (string-length str0) empty)))]
    (fn-for-los (string->List str0) (string->List str1) 0)))

;; =======================================================
;; PROBLEM:
;; Do FizzBuzz in Racket - this is a fun and easy exercise
;; =======================================================

;; Natural -> (listof X)
;; FizzBuzz implemented in Racket
(check-expect (fizzbuzz 2) (list 1 2))
(check-expect (fizzbuzz 5) (list 1 2 "fizz" 4 "buzz"))
(check-expect (fizzbuzz 10) (list 1 2 "fizz" 4 "buzz" "fizz" 7 8 "fizz" "buzz"))

(define (fizzbuzz n0)
  (foldl
   (λ (n ror)(cond [(and (= (modulo n 3) 0) (= (modulo n 5) 0)) (append ror (list "fizzbuzz"))]
                   [(= (modulo n 3) 0) (append ror (list "fizz"))]
                   [(= (modulo n 5) 0) (append ror (list "buzz"))]
                   [else
                    (append ror (list n))]))
   empty
   (remove 0 (build-list (add1 n0) identity))))

#;
(define (fizzbuzz n0)
  (local [(define (fn-for-natural n result)
            (cond [(zero?) result]
                  [else
                   (if ((and (= (modulo n 3) 0) (= (modulo n 5) 0)))
                       (fn-for-natural (sub1 n) (append result (list "fizzbuzz")))
                       (if (= (modulo n 3) 0)
                           (fn-for-natural (sub1 n) (append result (list "fizz")))
                           (fn-for-natural (sub1 n) (append result (list "buzz")))))]))]
    (fn-for-natural n0 empty)))


;; ==============================================================
;; PROBLEM:
;; Design a function which computes the "difference" of two lists
;; ==============================================================

;; (listof X) (listof X) -> (listof X)
(check-expect (diff empty empty) empty)
(check-expect (diff (list 1 2) empty) (list 1 2))
(check-expect (diff empty (list 1 2)) empty)
(check-expect (diff (list 1 2) (list 2 3)) (list 1))
(check-expect (diff (list 1 2 3 4) (list 4 3 2 1)) empty)

(define (diff lox0 lox1)
  (cond [(empty? lox0) empty]
        [(empty? lox1) lox0]
        [else
         (if (member? (first lox1) lox0)
             (diff (remove (first lox1) lox0) (rest lox1))
             (diff lox0 (rest lox1)))]))

;; =================================================================
;; PROBLEM:
;; Design a function which computes the set compliment of two lists
;; =================================================================

;; (listof X) (listof X) -> (listof X)
(check-expect (comp empty empty) empty)
(check-expect (comp (list 1) (list 1)) empty)
(check-expect (comp (list 1 2) (list 2 1 4)) empty)
(check-expect (comp (list 2 1 4) (list 1 2)) (list 4))
(check-expect (comp empty (list 1 2)) empty)
(check-expect (comp (list 1 2) empty) (list 1 2))

(define (comp A B)
  (local [(define (fn-for-sets a b compliment)
            (cond [(empty? a) compliment]
                  [else
                   (if (not (member? (first a) b))
                       (fn-for-sets (rest a) b (append compliment (list (first a))))
                       (fn-for-sets (rest a) b compliment))]))]
    (fn-for-sets A B empty)))

;; ===================================================================
;; PROBLEM:
;; Design a function which computes the cartesian product of two lists
;; ===================================================================

;; (listof X) (listof X) -> (listof X)
;; computes cartesian product of two lists of equal size
(check-expect (cartesian empty empty) empty)
(check-expect (cartesian (list 1 2) (list 3 4)) (list (list 1 3) (list 2 4)))
(check-expect (cartesian (list "a" "b" "c") (list 1 2 3))
              (list (list "a" 1) (list "b" 2) (list "c" 3)))

;; General recursive solution
#;
(define (cartesian A B)
  (cond [(or (empty? A) (empty? B)) empty]
        [else
         (cons (list (first A) (first B))
               (cartesian (rest A) (rest B)))]))

(define (cartesian A B)
  ;; product is type: (listof (listof X)), interp. the cartesian product of A,B
  (local [(define (fn-for-sets a b product)
            (cond [(or (empty? a) (empty? b)) (reverse product)]
                  [else
                   (fn-for-sets (rest a) (rest b)
                                (cons (list (first a) (first b)) product))]))]
    (fn-for-sets A B empty)))

;; ===================================================================================
;; PROBLEM:
;; Design a function which consumes a list, and packs elements into sublists of size 2
;; you may assume that the list is at least of size 2 at the start
;; ===================================================================================

;; (listof X) -> (listof (listof X))
;; packs elements into a new sublist of size 2
(check-expect (pack! (list 1 2)) (list (list 1 2)))
(check-expect (pack! (list 1 2 3 4)) (list (list 1 2) (list 3 4)))
(check-expect (pack! (list 1 2 3 4 5 6)) (list (list 1 2) (list 3 4) (list 5 6)))
                    
(define (pack! lox)
  (local [(define (fn-for-lox lox prev)
            (cond [(empty? lox) empty]
                  [else
                   (cons (list prev (first lox))
                         (fn-for-lox (rest lox) (first lox)))]))

          (define (remove-duplicates lox2)
            (local [(define (fn-for-lox2 lox count original)
                      (cond [(empty? lox) original]
                            [else
                             (if (= (modulo count 2) 0)
                                 (fn-for-lox2 (rest lox) (add1 count) (remove (first lox) original))
                                 (fn-for-lox2 (rest lox) (add1 count) original))]))]
              (fn-for-lox2 lox2 1 lox2)))]
    (remove-duplicates (fn-for-lox (rest lox) (first lox)))))

;; ===================================================================================
;; PROBLEM:
;; Design a function which does the following
;; (list 1 2 3 4)     -> (list 2 1 4 3)
;; (list 1 2 3 4 5 6) -> (list 2 1 3 4 6 5)
;; ===================================================================================

(define (reverse-sublists lox)
  (local [(define (reverse-pack! lox)
            (cond [(empty? lox) empty]
                  [else
                   (cons (reverse (first lox))
                         (reverse-pack! (rest lox)))]))

          (define (list-all lox)
            (cond [(empty? lox) empty]
                  [else
                   (append (first lox)
                           (list-all (rest lox)))]))]
    (list-all (reverse-pack! (pack! lox)))))

;; ==================
;; PROBLEM:
;; Ackermann function
;; ==================

(define (ackm m n)
  (cond [(zero? m) (add1 n)]
        [(zero? n) (ackm (sub1 m) 1)]
        [else
         (ackm (sub1 m) (ackm m (sub1 n)))]))

;; ===================================================================
;; PROBLEM:
;; Design a function which takes in a range of values, and an interval
;; produces true iff all the values are in the interval, inclusive.
;; ==================================================================

;; (listof Number) Number Number -> Boolean
;; true iff list contains numbers in interval given, inclusive
(check-expect (in-range? (list 1) 0 5) true)
(check-expect (in-range? (list 1 2 3) 0 4) true)
(check-expect (in-range? (list -33 1 3) -32 4) false)

; (define (in-range? lov bt tp) false) ; stub

(define (in-range? lov bt tp)
  (andmap (lambda(val)(and (>= val bt)(<= val tp))) lov))

;; ====================================================================
;; PROBLEM:
;; Design a function which takes in a list of numbers (list 1 2 3), and 
;; produces its corresponding number value, e.g. (list 1 2 3) -> 321
;; ====================================================================

;; (listof Number) -> Number 
;; convert list to number
(check-expect (to->num (list 1)) 1)
(check-expect (to->num (list 1 2)) 21)
(check-expect (to->num (list 1 2 3)) 321)
(check-expect (to->num (list 1 2 1)) 121)

; (define (to->num lon) 0) ; stub

(define (to->num lon)
  (string->number (foldr string-append "" 
    (foldl (λ(s los) (append (list s) los)) empty 
      (map (λ(num)(number->string num)) lon)))))

;; (listof X) -> (listof (listof X))
;; consume lox, produce list of suffixes
(check-expect (suffixes (list "a" "b" "c" "d"))
              (list (list "a" "b" "c" "d") (list "b" "c" "d")
                    (list "c" "d") (list "d")))

; (define (suffixes lox) empty) ; stub

(define (suffixes lox)
  (cond [(empty? lox) empty]
        [else
         (cons lox (suffixes (rest lox)))]))


;; ====================================================================
;; PROBLEM:
;; Design a function which consumes a list of a list, then 
;; produces the list "flattened", e.g. (list (list 1 2) (list 3 4)) ->
;; (list 1 2 3 4)
;; ====================================================================

;; (listof (listof X)) -> (listof X)
;; consume a list with sublists, produce a "flattened" list
(check-expect (flatten empty) empty)
(check-expect (flatten (list (list 1 2) (list 3 4) (list 5) (list 6))) (list 1 2 3 4 5 6))
(check-expect (flatten (list (list 1) (list 2))) (list 1 2))
(check-expect (flatten (list (list 1))) (list 1))

; define (flatten lox) empty) ; stub

;<Not tail-recursive>
#;
(define (flatten lox)
  (cond [(empty? lox) empty]
        [else
         (append (first lox)
                 (flatten (rest lox)))]))

;<Tail-recursive solution>
#;
(define (flatten lox)
  (local [(define (fn-for-lox lox flattened)
            (cond [(empty? lox) flattened]
                  [else
                   (fn-for-lox (rest lox) (append flattened (first lox)))]))]
    (fn-for-lox lox empty)))

;<Using mutation>
#;
(define (flatten lox)
  (local [(define flattened empty)]
    (begin
      (for-each (λ(x)(set! flattened (append flattened x))) lox)
      flattened)))

;<Using higher-order functions>

(define (flatten lox)
  (foldl (λ(x flattened)(append flattened x)) empty lox))
