;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname primenumbergenerator) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; data definitions

; A Natural is a non-negative integer
(define (natural? n)
  (and (number? n) (>= n 0) (or (= 0 n) (natural? (- n 1)))))
; checks
(check-expect (natural? 0) #t)
(check-expect (natural? 2) #t)
(check-expect (natural? -2) #f)
(check-expect (natural? pi) #f)
(check-expect (natural? "zero") #f)
#;
(define (fn-on-natural n)
  (cond
    [(not (natural? n)) (error "not a non-negative integer")]
    [(= n 0) ...]
    [else ... n ... (fn-on-natural (- n 1))]))
    

; ListOfNatural is one of:
; - '()
; - (cons Natural ListOfNaturals)
(define (list-of-naturals? list)
  (or (empty? list) (and (natural? (first list)) (list-of-naturals? (rest list)))))
; checks
(check-expect (list-of-naturals? '()) #t)
(check-expect (list-of-naturals? (cons 3 (cons 2 '()))) #t)
(check-expect (list-of-naturals? (cons 3 (cons -2 '()))) #f)
(check-expect (list-of-naturals? (cons pi (cons 2 '()))) #f)
#;
(define (fn-on-list-of-naturals list)
  (cond
    [(not (list-of-naturals? list)) (error "not a list of naturals")]
    [(empty? list) ...]
    [(= (first list) 0) ...]
    [else (... (first list) ... (fn-on-list-of-naturals (rest list)))]))



; functions

(define (prime-number-generator n)
  ; Natural -> [ListOf Natural]
  ; given a number n, finds all primes less than or equal to that number
  (local (
          (define primes
            ; primes--the function output--is defined as a local variable.
            ; Note that the value of primes is utilized twice every time this
            ; function is called. The local definition really helps simplify the
            ; recursion
            (cond
              [(= n 2) '()]
              [else (prime-number-generator (- n 1))])))
    ; - IN -
    (cond
      [(is-prime? n primes) (snoc n primes)]
      [else primes])))
; checks
(check-expect (prime-number-generator 2) (list 2))
(check-expect (prime-number-generator 3) (cons 2 (cons 3 '())))
(check-expect (prime-number-generator 4) (cons 2 (cons 3 '())))
(check-expect (prime-number-generator 6) (cons 2 (cons 3 (cons 5 '()))))


(define (is-prime? n primes)
  ; Natural [ListOf Natural] ->  Boolean
  ; given a number and a list of known primes,
  ;     determines if that number is itself a prime
  (or
   (empty? primes)
   (< n (sqr (first primes)))
   (and
    (not (= 0 (modulo n (first primes))))
    (is-prime? n (rest primes)))))
; checks
(check-expect (is-prime? 3 (cons 2 '())) #t)
(check-expect (is-prime? 5 (cons 2 (cons 3 '()))) #t)
(check-expect (is-prime? 6 (cons 2 (cons 3 '()))) #f)


(define (snoc n list)
  ; Natural [ListOf Natural] -> [ListOf Natural]
  ;; appends a new element to a list, but from the inside out
  (cond
    [(empty? list) (cons n '())]
    [else (cons (first list) (snoc n (rest list)))]))
;checks
(check-expect (snoc 3 (cons 2 '())) (cons 2 (cons 3 '())))
(check-expect (snoc 5 (cons 2 (cons 3 '()))) (cons 2 (cons 3 (cons 5 '()))))



; actions

(prime-number-generator 100000)