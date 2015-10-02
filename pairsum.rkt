#lang racket
;;; Frederik Roenn Stensaeth
;;; 04.04.2015
;;; A Racket program that defines gen-list, pair-sum?, and pair-sum-lazy?

;;; gen-list generates a list of consecutive numbers from start to stop. Returns #f if invalid values.
(define gen-list
  (lambda (start stop)
    (if (not (and (integer? start) (integer? stop)))
        #f
        (cond ([> start stop] #f)
              ([= start stop] (list start))
              (else (append (list start) (gen-list (+ start 1) stop)))))))

;;; pair-sum? tests whether any two adjacent values in numlist sum to val.
(define pair-sum?
  (lambda (numlist val)
    (if (or (>= 1 (length numlist)) (not (integer? val)))
        #f
        (if (= (+ (car numlist) (car (cdr numlist))) val)
               #t
               (pair-sum? (cdr numlist) val)))))

;;; pair-sum-lazy? tests whether any two adjacent values in numlist sum to val. numlist is a lazy list.
(define pair-sum-lazy?
  (lambda (numlist val)
    (cond ([equal? #f numlist] #f)
          ([equal? #f (car numlist)] #f)
          ([equal? #f ((cdr numlist))] #f)
          ([not (integer? val)] #f)
          (else 
              (if (= (+ (car numlist) (car ((cdr numlist)))) val)
                  #t
                  (pair-sum-lazy? ((cdr numlist)) val))))))

;;; gen-lazy-list generates a lazy list of consecutive numbers from start to stop.
(define gen-lazy-list
  (lambda (start stop)
    (if (or (> start stop) (not (integer? start)) (not (integer? stop)))
        #f
        (cons start
              (lambda () (gen-lazy-list (+ start 1) stop))))))