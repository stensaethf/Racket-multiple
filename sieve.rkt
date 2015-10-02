#lang racket
;;; Frederik Roenn Stensaeth
;;; 04.11.2015

;;; seq generates a lazy list of consecutive numbers from start to stop.
(define seq
  (lambda (first last)
    (if (or (> first last) (not (integer? first)) (not (integer? last)))
        #f
        (cons first (lambda () (seq (+ first 1) last))))))

;;; inf-seq generates an infinite lazy list of consective numbers.
(define inf-seq
  (lambda (first)
    ;; Return a lazy-list containing first and a call to inf-seq with (+ first 1)
    (cons first (lambda () (inf-seq (+ first 1))))))

;;; first-n generates a list of the first n numbers in the lazy-list.
(define first-n
  (lambda (lazy-list n)
    ;; Return an empty list if the end of the list is reached.
    (cond ([equal? #f lazy-list] '())
          ([= 1 n] (list (car lazy-list)))
          (else (cons (car lazy-list) (first-n ((cdr lazy-list)) (- n 1)))))))

;;; nth returns the nth element in a lazy list. Returns #f if n is too large or negative.
(define nth
  (lambda (lazy-list n)
    ;; Return #f is the end of list is reached or invalid n is provided.
    (cond ([equal? #f lazy-list] #f)
          ([< n 1] #f)
          ([= 1 n] (car lazy-list))
          (else (nth ((cdr lazy-list)) (- n 1))))))

;;; filter-multiples generates a lazy-list containing the elements that were not multiples of n
;;; in the lazy-list it was given.
(define filter-multiples
  (lambda (lazy-list n)
    ;; Return #f if the end of the list is reached.
    (cond ([equal? #f lazy-list] #f)
          ;; Check to see whether the item in the list is a multiple of n.
          ([= 0 (modulo (car lazy-list) n)] (filter-multiples ((cdr lazy-list)) n))
          (else (cons (car lazy-list) (lambda () (filter-multiples ((cdr lazy-list)) n)))))))

;;; primes computes a lazy-list containing all prime numbers.
(define primes
  (lambda ()
    (sieve (inf-seq 2))))

;;; sieve is a helper function for (primes). It generates a lazy-list containing all prime numbers.
(define sieve
  (lambda (lazy-list)
    ;; Return a list containing the first item in the lazy-list and a function.
    (cons (car lazy-list) (lambda () (sieve (filter-multiples lazy-list (car lazy-list)))))))
