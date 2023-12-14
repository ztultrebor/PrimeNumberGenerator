;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname primenumbergenerator) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


; data definitions

; A Natural is a non-negative integer
(define (natural? n)
  (and (number? n) (>= n 0) (or (= 0 n) (natural? (- n 1)))))
; checks
(check-expect (natural? 0) #t)
(check-expect (natural? 2) #t)
(check-expect (natural? -2) #f)
(check-expect (natural? 3.14) #f)
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
(check-expect (list-of-naturals? (cons 3.14 (cons 2 '()))) #f)
#;
(define (fn-on-list-of-naturals list)
  (cond
    [(not (list-of-naturals? list)) (error "not a list of naturals")]
    [(empty? list) ...]
    [(= (first list) 0) ...]
    [else (... (first list) ... (fn-on-list-of-naturals (rest list)))]))
    

; constants

(define KNOWNPRIMES '())



; functions

(define (prime-number-generator n primes)
  ; Natural ListOfNaturals -> ListOfNaturals
  ; given a number and a list of known primes,
  ;     find all primes less than or equal to that number
  (cond
    ;[(not (natural? n)) (error "not a non-negative integer")]
    ;[(not (list-of-naturals? primes)) (error "not a list of naturals")]
    [(= n 2) (cons 2 primes)]   ; base case
    [(is-prime? n (prime-number-generator (- n 1) primes))
     (cons n (prime-number-generator (- n 1) primes))]  ; dumb
    [else (prime-number-generator (- n 1) primes)])) ; really dumb
; checks
(check-expect (prime-number-generator 2 '()) (cons 2 '()))
(check-expect (prime-number-generator 3 '()) (cons 3 (cons 2 '())))
(check-expect (prime-number-generator 4 '()) (cons 3 (cons 2 '())))
(check-expect (prime-number-generator 6 '()) (cons 5 (cons 3 (cons 2 '()))))


(define (is-prime? n primes)
  ; Natural ListOfNaturals ->  Boolean
  ; given a number and a list of known primes,
  ;     determines if that number is itself a prime
  (cond
    [(empty? primes) #t]
    [(= 0 (modulo n (first primes))) #f]
    [else (is-prime? n (rest primes))]))
; checks
(check-expect (is-prime? 2 '()) #t)
(check-expect (is-prime? 3 (cons 2 '())) #t)
(check-expect (is-prime? 5 (cons 3 (cons 2 '()))) #t)
(check-expect (is-prime? 6 (cons 3 (cons 2 '()))) #f)

; actions

(prime-number-generator 25 KNOWNPRIMES)