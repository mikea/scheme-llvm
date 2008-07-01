;; A module which lowers R5RS scheme into the scheme, which
;; is acceptable by compiler

(define (lower-define e t)
  (if (pair? (cadr e))
      ; (define (<var> <formals>) body)
      (let ((var (caadr e))
	    (formals (cdadr e))
	    (body (caddr e)))
	(lower-define 
	 `(define ,var (lambda (,@formals) ,body))
	 t))
      ; (define <var> <expr>)
      (let ((var (cadr e))
	    (expr (caddr e)))
	`((let ((,var ,expr))
	    ,@(lower t))))))

(define (lower-expr e t)
  (define (loop e t)
    (if t
	(cons e (lower t))
	e))
  (if (and (pair? e) (symbol? (car e)))
      (case (car e)
	((define) (lower-define e t))
	(else (loop e t)))
      (loop e t)))

(define (lower e)
  (if (null? e)
      e
      (if (pair? e)
	  (lower-expr (car e) (cdr e))
	  (lower-expr e #f))))