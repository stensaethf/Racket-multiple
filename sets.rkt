#lang racket
;;; Frederik Roenn Stensaeth
;;; 04.09.2015
;;; Note: I could have put elementOf? inside the individual functions themselves, but that would have been
;;; redundant, so I decided to keep it separate.

;;; <helper function> elementOf? checks whether a value - val - is in a set. Returns #t if it is in the set.
(define elementOf?
  (lambda (val set)
    (cond ([null? set] #f)
          ([equal? val (car set)] #t)
          ;; If the val is a set and the (car set) is a set, then check whether the sets are equal.
          ;; If not, check whether the val is in the remainder of the set.
          ([and (list? (car set)) (list? val)]
           (if (set-equal? val (car set))
               #t
               (elementOf? val (cdr set))))
          (else (elementOf? val (cdr set))))))

;;; union finds and returns the union of two sets.
(define union
  (lambda (S1 S2) 
    (cond ([null? S2] S1)
          ;; If the first element in S2 is in S1, return the unuon of S1 and the rest of S2.
          ([elementOf? (car S2) S1] (union S1 (cdr S2)))
          ;; Else return the union of the set S1 + the first element in S2 and the remainder of S2.
          (else (union (append S1 (list (car S2))) (cdr S2))))))

;;; intersection finds and returns the intersection of two sets.
(define intersect
  (lambda (S1 S2)
    (cond ([null? S1] '())
          ;; If the first element in S1 is in S2, return the set containing the first item in S1 and the intersection of
          ;; the remainder of S1 and S2.
          ([elementOf? (car S1) S2] (cons (car S1) (intersect (cdr S1) S2)))
          ;; Else, return the intersection of the last elements of S1 and S2.
          (else (intersect (cdr S1) S2)))))

;;; set-equal? checks whether two sets are equal by calling sub-set-equal?. Returns #t if the two sets are equal.
(define set-equal?
  (lambda (S1 S2)
    (cond ([and (null? S2) (null? S1)] #t)
          ([null? S1] #f)
          ([null? S2] #f)
          ([not (= (length S1) (length S2))] #f)
          ;; Checks whether every element in S1 is in S2 and every element in S2 is in S1.
          (else (and (sub-set-equal? S1 S2) (sub-set-equal? S2 S1))))))

;;; sub-set-equal? checks whether two sets are equal. Returns true if the two sets are equal.
(define sub-set-equal?
  (lambda (S1 S2)
    (cond ([and (null? S1) (null? S2)] #t)
          ([null? S1] #t)
          ;; If the first element in S1 is in S2, check whether the remainder of S1 and S2 are equal.
          ([elementOf? (car S1) S2] (sub-set-equal? (cdr S1) S2))
          (else #f))))


