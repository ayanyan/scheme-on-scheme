;;; Environmental functions for interpreter.scm


(define (empty-frame)
  (list))

(define (update frame var val)
  (cons (cons var val) frame))

(define (lookup var frame)
  (assoc var frame))

(define (make-env)
  (list (empty-frame)))

(define (extend-env env)
  (cons (empty-frame) env))

(define (define-var env var val)
  (if (null? env)
      env
      (cons (update (car env) var val) (cdr env))))

(define (define-var! env var val)
  (if (null? env)
      #f
      (set-car! env (update (car env) var val)))
  env)

(define (set-var! env var val)
  (define (frame-set-var! frame var val)
    (cond ((null? frame)
           #f)
          ((equal? var (car (car frame)))
           (set-cdr! (car frame) val)
           #t)
          (else
           (frame-set-var! (cdr frame) var val))))
  (if (null? env)
      #f
      (or (frame-set-var! (car env) var val)
          (set-var! (cdr env) var val))))

(define (lookup-var var env)
  (if (null? env)
      #f
      (let ((found (lookup var (car env))))
        (if (pair? found)
            found
            (lookup-var var (cdr env))))))
