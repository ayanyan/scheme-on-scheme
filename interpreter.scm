;;; A self-interpretable interpreter of (a subset of) Scheme


;;; fold may not be a primitive function in a Scheme implementation

(define (foldl op init l)
  (if (pair? l)
      (foldl op (op (car l) init) (cdr l))
      init))

(define (foldr op init l)
  (foldl op init (reverse l)))

;;; environment

(load "environment.scm")

;;; data structures

(define (make-closure env params body)
  (cons '*lambda* (cons env (cons params body))))

(define (data-closure? data)
  (and (pair? data) (equal? (car data) '*lambda*)))

(define closure-env cadr)

(define closure-params caddr)

(define closure-body cdddr)

(define (make-primitive arity fun)
  (list '*primitive* arity fun))

(define (data-primitive? data)
  (and (pair? data) (equal? (car data) '*primitive*)))

(define primitive-arity cadr)

(define primitive-fun caddr)

;;; base-eval :: ENV -> EXP -> ENV x VAL

(define (base-eval env exp)
  (define (constant? exp)
    (or (boolean? exp) (number? exp) (string? exp)))
  (cond ((eof-object? exp) (cons env '*exit*))
        ((constant? exp) (cons env exp))
        ((symbol? exp) (var-eval env exp))
        ((null? exp) (eval-error env 'syntax-error exp))
        ((data-closure? exp) (cons env exp))
        ((data-primitive? exp) (cons env exp))
        ((not (pair? exp)) (eval-error env 'unknown-data exp))
        ((equal? (car exp) 'exit) (cons env '*exit*))
        ((equal? (car exp) 'define) (def-eval env exp))
        ((equal? (car exp) 'let) (let-eval env exp))
        ((equal? (car exp) 'letrec) (letrec-eval env exp))
        ((equal? (car exp) 'let*) (let*-eval env exp))
        ((equal? (car exp) 'lambda) (lambda-eval env exp))
        ((equal? (car exp) 'if) (if-eval env exp))
        ((equal? (car exp) 'cond) (cond-eval env exp))
        ((equal? (car exp) 'and) (and-eval env exp))
        ((equal? (car exp) 'or) (or-eval env exp))
        ((equal? (car exp) 'begin) (begin-eval env exp))
        ((equal? (car exp) 'quote) (quote-eval env exp))
        (else (app-eval env exp))))

(define (correct-syntax? type exp)
  (cond ((equal? type 'var)
         (symbol? exp))
        ((equal? type 'define)
         (and (list? exp) (pair? exp) (pair? (cdr exp)) (pair? (cddr exp))))
        ((equal? type 'let)
         (and (list? exp) (pair? exp) (pair? (cdr exp)) (list? (cadr exp))))
        ((equal? type 'lambda)
         (and (list? exp) (pair? exp) (pair? (cdr exp))))
        ((equal? type 'app)
         (and (list? exp) (pair? exp)))
        ((equal? type 'if)
         (and (list? exp) (pair? exp) (pair? (cdr exp)) (pair? (cddr exp))))
        ((equal? type 'begin)
         (and (list? exp) (pair? exp)))
        ((equal? type 'quote)
         (and (pair? exp) (pair? (cdr exp)) (null? (cddr exp))))
        (#t #t)))

(define (repeat-base-eval env el)
  (foldl
   (lambda (exp acc) (base-eval (car acc) exp))
   (cons env '())
   el))

(define (map-base-eval env el)
  (foldr
   (lambda (exp acc)
     (let ((res (base-eval (car acc) exp)))
       (cons (car res) (cons (cdr res) (cdr acc)))))
   (cons env '())
   el))

(define (begin-eval env exp)
  (if (correct-syntax? 'begin exp)
      (repeat-base-eval env (cdr exp))
      (eval-error env 'syntax-error exp)))

(define (var-eval env exp)
  (if (correct-syntax? 'var exp)
      (let ((found (lookup-var exp env)))
        (if (pair? found)
            (cons env (cdr found))
            (eval-error env 'unbound-variable exp)))
      (eval-error env 'syntax-error exp)))

(define (def-eval env exp)
  (if (correct-syntax? 'define exp)
      (let ((var (cadr exp)))
        (if (pair? var)
            (base-eval env (defun->define exp))
            (let* ((res (base-eval env (caddr exp)))
                   (env (car res))
                   (val (cdr res)))
              (cons (define-var! env var val) var))))
      (eval-error env 'syntax-error exp)))

(define (let-eval env exp)
  (if (correct-syntax? 'let exp)
      (base-eval env (let->app exp))
      (eval-error env 'syntax-error exp)))

(define (letrec-eval env exp)
  (if (correct-syntax? 'let exp)
      (base-eval env (letrec->define exp))
      (eval-error env 'syntax-error exp)))

(define (let*-eval env exp)
  (if (correct-syntax? 'let exp)
      (base-eval env (let*->let exp))
      (eval-error env 'syntax-error exp)))

(define (lambda-eval env exp)
  (if (correct-syntax? 'lambda exp)
      (cons env (make-closure env (cadr exp) (cddr exp)))
      (eval-error env 'syntax-error exp)))

(define (app-eval env exp)
  (if (correct-syntax? 'app exp)
      (let* ((l (map-base-eval env exp))
             (env (car l))
             (fun (cadr l))
             (args (cddr l)))
        (base-apply env fun args))
      (eval-error env 'synatx-error exp)))

(define (base-apply env fun args)
  (define (make-alist vars vals)
    (if (and (pair? vars) (pair? vals))
        (cons (cons (car vars) (car vals)) (make-alist (cdr vars) (cdr vals)))
        (if (null? vars) (list) (list (cons vars vals)))))
  (cond ((data-closure? fun)
         (if (= (length (closure-params fun)) (length args))
             (let* ((new-env (foldl
                              (lambda (pair env) (define-var! env (car pair) (cdr pair)))
                              (extend-env (closure-env fun))
                              (make-alist (closure-params fun) args)))
                    (res (repeat-base-eval new-env (closure-body fun)))
                    (val (cdr res)))
               (cons env val))
             (eval-error env 'wrong-number-of-args fun)))
        ((data-primitive? fun)
         (if (or (not (number? (primitive-arity fun)))
                 (= (primitive-arity fun) (length args)))
             ((primitive-fun fun) env args)
             (eval-error env 'wrong-number-of-args fun)))
        (else
         (eval-error env 'non-function fun))))

(define (if-eval env exp)
  (if (correct-syntax? 'if exp)
      (let* ((cond-res (base-eval env (cadr exp)))
             (env (car cond-res))
             (cond-val (cdr cond-res)))
        (if (equal? cond-val #f)
            (if (pair? (cdddr exp))
                (base-eval env (cadddr exp))
                #f)
            (base-eval env (caddr exp))))
      (eval-error env 'syntax-error exp)))

(define (cond-eval env exp)
  (if (correct-syntax? 'app exp)
      (base-eval env (cond->if exp))
      (eval-error env 'syntax-error exp)))

(define (or-eval env exp)
  (if (correct-syntax? 'app exp)
      (base-eval env (or->if exp))
      (eval-error env 'syntax-error exp)))

(define (and-eval env exp)
  (if (correct-syntax? 'app exp)
      (base-eval env (and->if exp))
      (eval-error env 'syntax-error exp)))

(define (quote-eval env exp)
  (if (correct-syntax? 'quote exp)
      (cons env (cadr exp))
      (eval-error env 'syntax-error exp)))

;;; syntax sugar

(load "sugar.scm")

;;; error processing (not enough useful)

(define (eval-error env type exp)
  (display "ERROR: ")
  (write type)
  (display ": ")
  (print-data exp)
  (newline)
  (cons env '*error*))

;;; print

(define (print-data data)
  (cond ((data-closure? data) (display "#<closure ")
                              (write (cons (closure-params data) (closure-body data)))
                              (display ">"))
        ((data-primitive? data) (display "#<primitive>"))
        ((equal? data '*unspecified*) (display "#<unspecified>"))
        ((equal? data '*error*) (display "#<error>"))
        ((equal? data '*exit*) (display "bye."))
        (else (write data))))

;;; top-env

(define (make-top-env)
  (let* ((env (make-env))
         (env
          (define-var! env 'apply
            (make-primitive 2 (lambda (env args)
                                (base-apply env (car args) (cadr args))))))
         (env
          (define-var! env 'list
            (make-primitive #f (lambda (env args)
                                 (cons env args)))))
         (env
          (define-var! env 'cons
            (make-primitive 2 (lambda (env args)
                                (cons env (cons (car args) (cadr args)))))))
         (env
          (define-var! env 'car
            (make-primitive 1 (lambda (env args)
                                (cons env (car (car args)))))))
         (env
          (define-var! env 'cdr
            (make-primitive 1 (lambda (env args)
                                (cons env (cdr (car args)))))))
         (env
          (define-var! env 'null?
            (make-primitive 1 (lambda (env args)
                                (cons env (null? (car args)))))))
         (env
          (define-var! env 'pair?
            (make-primitive 1 (lambda (env args)
                                (cons env (pair? (car args)))))))
         (env
          (define-var! env 'list?
            (make-primitive 1 (lambda (env args)
                                (cons env (list? (car args)))))))
         (env
          (define-var! env 'equal?
            (make-primitive 2 (lambda (env args)
                                (cons env (equal? (car args) (cadr args)))))))
         (env
          (define-var! env 'eq?
            (make-primitive 2 (lambda (env args)
                                (cons env (eq? (car args) (cadr args)))))))
         (env
          (define-var! env 'set-car!
            (make-primitive 2 (lambda (env args)
                                (cons env (set-car! (car args) (cadr args)))))))
         (env
          (define-var! env 'set-cdr!
            (make-primitive 2 (lambda (env args)
                                (cons env (set-cdr! (car args) (cadr args)))))))
         (env
          (define-var! env 'symbol?
            (make-primitive 1 (lambda (env args)
                                (cons env (symbol? (car args)))))))
         (env
          (define-var! env 'boolean?
            (make-primitive 1 (lambda (env args)
                                (cons env (boolean? (car args)))))))
         (env
          (define-var! env 'number?
            (make-primitive 1 (lambda (env args)
                                (cons env (number? (car args)))))))
         (env
          (define-var! env 'string?
            (make-primitive 1 (lambda (env args)
                                (cons env (string? (car args)))))))
         (env
          (define-var! env '=
            (make-primitive 2 (lambda (env args)
                                (cons env (= (car args) (cadr args)))))))
         (env
          (define-var! env '<
            (make-primitive 2 (lambda (env args)
                                (cons env (< (car args) (cadr args)))))))
         (env
          (define-var! env '>
            (make-primitive 2 (lambda (env args)
                                (cons env (> (car args) (cadr args)))))))
         (env
          (define-var! env '+
            (make-primitive 2 (lambda (env args)
                                (cons env (+ (car args) (cadr args)))))))
         (env
          (define-var! env '-
            (make-primitive 2 (lambda (env args)
                                (cons env (- (car args) (cadr args)))))))
         (env
          (define-var! env '*
            (make-primitive 2 (lambda (env args)
                                (cons env (* (car args) (cadr args)))))))
         (env
          (define-var! env '/
            (make-primitive 2 (lambda (env args)
                                (cons env (/ (car args) (cadr args)))))))
         (env
          (define-var! env 'quotient
            (make-primitive 2 (lambda (env args)
                                (cons env (quotient (car args) (cadr args)))))))
         (env
          (define-var! env 'modulo
            (make-primitive 2 (lambda (env args)
                                (cons env (modulo (car args) (cadr args)))))))
         (env
          (define-var! env 'display
            (make-primitive 1 (lambda (env args)
                                (display (car args))
                                (cons env '*unspecified*)))))
         (env
          (define-var! env 'write
            (make-primitive 1 (lambda (env args)
                                (write (car args))
                                (cons env '*unspecified*)))))
         (env
          (define-var! env 'flush
            (make-primitive 0 (lambda (env args)
                                (flush)
                                (cons env '*unspecified*)))))
         (env
          (define-var! env 'read
            (make-primitive 0 (lambda (env args)
                                (cons env (read))))))
         (env
          (define-var! env 'eof-object?
            (make-primitive 1 (lambda (env args)
                                (cons env (eof-object? (car args)))))))
         (env
          (define-var! env 'with-input-from-file
            (make-primitive 2 (lambda (env args)
                                (with-input-from-file (car args)
                                  (lambda ()
                                    (base-apply env (cadr args) '())))))))
         (env
          (define-var! env 'load
            (make-primitive 1 (lambda (env args)
                                (with-input-from-file (car args)
                                  (lambda ()
                                    (define (re-loop env)
                                      (let* ((res (base-eval env (read)))
                                             (env (car res))
                                             (val (cdr res)))
                                        (if (equal? val '*exit*)
                                            (cons env '*unspecified*)
                                            (re-loop env))))
                                    (re-loop env))))))))
    env))

;;; interface

(define (scheme)
  (let ((top-env (make-top-env)))
    (define (rep-loop env)
      (display "sister> ")
      (let* ((res (base-eval env (read)))
             (env (car res))
             (val (cdr res)))
        (print-data val)
        (newline)
        (if (equal? val '*exit*)
            #t
            (rep-loop env))))
    (let ((top-env (car (base-eval top-env '(load "preamble.scm")))))
      (rep-loop top-env))))
