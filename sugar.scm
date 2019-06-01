;;; Syntax sugar for interpreter.scm


(define (defun->define exp)
  (if (and (pair? exp) (equal? (car exp) 'define)
           (pair? (cdr exp)) (pair? (cadr exp)))
      (let ((var (car (cadr exp)))
            (params (cdr (cadr exp)))
            (body (cddr exp)))
        (list 'define var
              (cons 'lambda (cons params body))))
      exp))

(define (let->app exp)
  (if (and (pair? exp) (equal? (car exp) 'let))
      (let ((decl (cadr exp))
            (body (cddr exp)))
        (cons (cons 'lambda (cons (map car decl) body))
              (map cadr decl)))
      exp))

(define (letrec->define exp)
  (if (and (pair? exp) (equal? (car exp) 'letrec))
      (let ((decl (cadr exp))
            (body (cddr exp)))
        (cons 'let
              (cons '()
                    (append (map (lambda (x) (cons 'define x)) decl)
                            body))))
      exp))

(define (let*->let exp)
  (if (and (pair? exp) (equal? (car exp) 'let*))
      (foldr (lambda (x exp) (list 'let (list x) exp))
             (cons 'begin (cddr exp))
             (cadr exp))
      exp))

(define (cond->if exp)
  (if (and (pair? exp) (equal? (car exp) 'cond))
      (foldr (lambda (x exp)
               (list 'if (car x) (cons 'begin (cdr x)) exp))
             '#f
             (cdr exp))
      exp))

(define (or->if exp)
  (if (and (pair? exp) (equal? (car exp) 'or))
      (foldr (lambda (x exp)
               (list 'let (list (list 'v x)) (list 'if 'v 'v exp)))
             '#f
             (cdr exp))
      exp))

(define (and->if exp)
  (if (and (pair? exp) (equal? (car exp) 'and))
      (let ((l (reverse (cdr exp))))
        (if (pair? l)
            (foldl (lambda (x exp) (list 'if x exp #f))
                   (car l)
                   (cdr l))
            '#t))
      exp))
