#lang racket
;;; Frederik Roenn Stensaeth
;;; 04.04.2015
;;; A Racket implementation of binary search tree (bst).

;;; null-bst returns an empty bst
(define null-bst
  (lambda ()
    '()))

;;; null-bst? checks if a bst is empty or not. Returns #t if it is empty.
(define null-bst?
  (lambda (bst)
    (if (equal? '() bst)
        #t
        #f)))

;;; entry returns the root of the bst. Returns #f if the bst is empty or not correct format.
(define entry
  (lambda (bst)
    (if (= 0 (length bst))
        #f
        (if (not (= 3 (length bst)))
            #f
            ;; Checks if both the left and the right subtrees are lists.
            (if (or (not (list? (car (cdr bst)))) (not (list? (car (cdr (cdr bst))))))
                #f
                (car bst))))))

;;; left returns the left subtree of the bst. Returns #f if the bst is empty or not correct format.
(define left
  (lambda (bst)
    (if (= 0 (length bst))
        #f
        (if (not (= 3 (length bst)))
            #f
            ;; Checks if both the left and the right subtrees are lists.
            (if (or (not (list? (car (cdr bst)))) (not (list? (car (cdr (cdr bst))))))
                #f
                (car (cdr bst)))))))

;;; right returns the right subtree of the bst. Returns #f is the bst is empty or not correct format.
(define right
  (lambda (bst)
    (if (= 0 (length bst))
        #f
        (if (not (= 3 (length bst)))
            #f
            ;; Checks if both the left and the rightsubtrees are lists.
            (if (or (not (list? (car (cdr bst)))) (not (list? (car (cdr (cdr bst))))))
                #f
                (car (cdr (cdr bst))))))))

;;; make-bst creates a bst and returns it. Returns #f if the root is not a number or either of the subtrees are not lists.
(define make-bst
  (lambda (elt left right)
    ;; Checks if elt is a number and if the subtrees are lists.
    (if (or (not (number? elt)) (not (list? right)) (not (list? right)))
        #f
        ;; Checks if both subtrees are empty.
        (cond ([and (= 0 (length left)) (= 0 (length right))] 
                  (list elt '() '()))
              ;; Checks if the left subtree is not empty and the right subtree is empty.
              ([and (= 3 (length left)) (= 0 (length right))]
                  ;; Checks if the subtrees of the left subtree are proper format.
                  (if (and (list? (car (cdr left))) (list? (car (cdr (cdr left)))) (number? (car left)))
                      (list elt left '())
                      #f))
              ;; Checks if the left subtree is empty and the right subtree is not empty.
              ([and (= 0 (length left)) (= 3 (length right))]
                  ;; Checks if hte subtrees of the right subtree are proper format.
                  (if (and (list? (car (cdr right))) (list? (car (cdr (cdr right)))) (number? (car right)))
                      (list elt '() right)
                      #f))
              ;; Checks if the left and right subtrees are non-empty.
              ([and (= 3 (length left)) (= 3 (length right))]
                  ;; Checks if the subtrees of the right and left subtrees are proper format.
                  (if (and (and (list? (car (cdr left))) (list? (car (cdr (cdr left)))) (number? (car left))) (and (list? (car (cdr right))) (list? (car (cdr (cdr right)))) (number? (car right))))
                      (list elt left right)
                      #f))
              ;; If either subtree contains are of unusual size, return #f.
              (else #f)))))

;;; preorder performs a preorder traversal of the bst and returns a list of the values in the bst.
(define preorder
  (lambda (bst)
    (if (null-bst? bst)
        #f
        (if (not (= 3 (length bst)))
            #f
            ;; Checks if both subtrees are empty.
            (cond ([and (= 0 (length (car (cdr bst)))) (= 0 (length (car (cdr (cdr bst)))))] 
                      (list (car bst)))
                  ;; Checks if the right subtree is non-empty and the left subtree is empty.
                  ([and (= 0 (length (car (cdr bst)))) (= 3 (length (car (cdr (cdr bst)))))] 
                      (append (list (car bst)) (preorder (car (cdr (cdr bst))))))
                  ;; Checks if the right subtree is empty and the left subtree is non-empty.
                  ([and (= 3 (length (car (cdr bst)))) (= 0 (length (car (cdr (cdr bst)))))] 
                      (append (list (car bst)) (preorder (car (cdr bst)))))
                  ;; Checks if both subtrees are non-empty.
                  ([and (= 3 (length (car (cdr bst)))) (= 3 (length (car (cdr (cdr bst)))))] 
                      (append (list (car bst)) (append (preorder (car (cdr bst))) (preorder (car (cdr (cdr bst)))))))
                  (else #f))))))

;;; inorder performs an inorder traversal of the bst and returns a list of the values in the bst.
(define inorder
  (lambda (bst)
    (if (null-bst? bst)
        #f
        (if (not (= 3 (length bst)))
            #f
            ;; Checks if both subtrees are empty.
            (cond ([and (= 0 (length (car (cdr bst)))) (= 0 (length (car (cdr (cdr bst)))))] 
                      (list (car bst)))
                  ;; Checks if the right subtree is non-empty and the left subtree is empty.
                  ([and (= 0 (length (car (cdr bst)))) (= 3 (length (car (cdr (cdr bst)))))] 
                      (append (list (car bst)) (inorder (car (cdr (cdr bst))))))
                  ;; Checks if the right subtree is empty and the left subtree is non-empty.
                  ([and (= 3 (length (car (cdr bst)))) (= 0 (length (car (cdr (cdr bst)))))] 
                      (append (inorder (car (cdr bst))) (list (car bst))))
                  ;; Checks if both subtrees are non-empty.
                  ([and (= 3 (length (car (cdr bst)))) (= 3 (length (car (cdr (cdr bst)))))] 
                      (append (inorder (car (cdr bst))) (append (list (car bst)) (inorder (car (cdr (cdr bst)))))))
                  (else #f))))))

;;; postorder performs a postorder traversal of the bst and returns list of the values in the bst.
(define postorder
  (lambda (bst)
    (if (null-bst? bst)
        #f
        (if (not (= 3 (length bst)))
            #f
            ;; Checks if both subtrees are empty.
            (cond ([and (= 0 (length (car (cdr bst)))) (= 0 (length (car (cdr (cdr bst)))))] 
                      (list (car bst)))
                  ;; Checks if the right subtree is non-empty and the left subtree is empty.
                  ([and (= 0 (length (car (cdr bst)))) (= 3 (length (car (cdr (cdr bst)))))] 
                      (append (postorder (car (cdr (cdr bst)))) (list (car bst))))
                  ;; Checks if the right subtree is empty and the left subtree is non-empty.
                  ([and (= 3 (length (car (cdr bst)))) (= 0 (length (car (cdr (cdr bst)))))] 
                      (append (postorder (car (cdr bst))) (list (car bst))))
                  ;; Checks if both subtrees are non-empty.
                  ([and (= 3 (length (car (cdr bst)))) (= 3 (length (car (cdr (cdr bst)))))] 
                      (append (postorder (car (cdr bst))) (append (postorder (car (cdr (cdr bst)))) (list (car bst)))))
                  (else #f))))))

;;; insert inserts v at the correct location in the bst. If v was in the bst the same bst is returned, but if it was not in the bst a new bst is returned.
(define insert
  (lambda (v bst)
    (if (not (integer? v))
        #f
        (if (null-bst? bst)
            (make-bst v '() '())
            ;; Checks if v is the same value as the root.
            (cond ([= v (car bst)] bst)
                  ;; Checks if v is larger than the value of the root.
                  ([> v (car bst)] (make-bst (car bst) (car (cdr bst)) (insert v (car (cdr (cdr bst))))))
                  ;; Checks if v is smaller than the value of the root.
                  ([< v (car bst)] (make-bst (car bst) (insert v (car (cdr bst))) (car (cdr (cdr bst))))))))))