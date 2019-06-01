;;; Primitive functions for interpreter.scm


(define cadr (lambda (l) (car (cdr l))))
(define caar (lambda (l) (car (car l))))
(define cddr (lambda (l) (cdr (cdr l))))
(define cdar (lambda (l) (cdr (car l))))
(define caddr (lambda (l) (car (cddr l))))
(define cdddr (lambda (l) (cdr (cddr l))))
(define cadddr (lambda (l) (car (cdddr l))))

(define not (lambda (b) (if b #f #t)))

(define else #t)

(define foldl
  (lambda (op init l)
    (if (pair? l)
        (foldl op (op (car l) init) (cdr l))
        init)))

(define reverse
  (lambda (l)
    (foldl cons '() l)))

(define foldr
  (lambda (op init l)
    (foldl op init (reverse l))))

(define map
  (lambda (f l)
    (foldr (lambda (x acc) (cons (f x) acc)) '() l)))

(define length
  (lambda (l)
    (foldl (lambda (x acc) (+ 1 acc)) 0 l)))

(define append
  (lambda (l1 l2)
    (foldr cons l2 l1)))

(define assoc-gen
  (lambda (key alist cmp)
    (if (pair? alist)
        (let ((entry (car alist)))
          (if (and (pair? entry) (cmp key (car entry)))
              entry
              (assoc-gen key (cdr alist) cmp)))
        #f)))

(define assoc (lambda (key alist) (assoc-gen key alist equal?)))
(define assq (lambda (key alist) (assoc-gen key alist eq?)))

(define <= (lambda (x y) (or (< x y) (= x y))))
(define >= (lambda (x y) (or (> x y) (= x y))))

(define newline (lambda () (display "\n")))
