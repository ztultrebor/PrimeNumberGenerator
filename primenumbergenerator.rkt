;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname primenumbergenerator) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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



; functions

(define (prime-number-generator n)
  ; Natural -> ListOfNaturals
  ; given a number n, finds all primes less than or equal to that number
  (cond
    [(= n 2) (cons 2 '())]   ; base case
    [else (prime-sequence-constructor n (prime-number-generator (- n 1)))]))
; checks
(check-expect (prime-number-generator 2) (cons 2 '()))
(check-expect (prime-number-generator 3) (cons 3 (cons 2 '())))
(check-expect (prime-number-generator 4) (cons 3 (cons 2 '())))
(check-expect (prime-number-generator 6) (cons 5 (cons 3 (cons 2 '()))))


(define (prime-sequence-constructor n primes)
  ; Natural ListOfNaturals -> ListOfNaturals
  ; if the given n is prime, it prepends n to the list of known primes,
  ;    otherwise the list of known primes is unchanged
  (if (is-prime? n primes) (cons n primes) primes))
; checks
(check-expect (prime-sequence-constructor
               2 '()) (cons 2 '()))
(check-expect (prime-sequence-constructor
               3 (cons 2 '())) (cons 3 (cons 2 '())))
(check-expect (prime-sequence-constructor
               4 (cons 3 (cons 2 '()))) (cons 3 (cons 2 '())))

(define (is-prime? n primes)
  ; Natural ListOfNaturals ->  Boolean
  ; given a number and a list of known primes,
  ;     determines if that number is itself a prime
  (or
   (empty? primes)
   (and
    (not (= 0 (modulo n (first primes))))
    (is-prime? n (rest primes)))))
; checks
(check-expect (is-prime? 2 '()) #t)
(check-expect (is-prime? 3 (cons 2 '())) #t)
(check-expect (is-prime? 5 (cons 3 (cons 2 '()))) #t)
(check-expect (is-prime? 6 (cons 3 (cons 2 '()))) #f)

; actions

(prime-number-generator 1000)