(load "scheme-parser.scm")

(define literal-var-num 0)
(define globals "")
(define global-init "")

(define (next-literal-var)
  (set! literal-var-num (+ literal-var-num 1))
  (string-append "@L." (number->string literal-var-num)))

(define (add-global s)
  (set! globals (string-append globals s "\n")))

(define (add-global-init s)
  (set! global-init (string-append global-init s "\n")))

(define (compile-number-literal e)
  (let ((var-name (next-literal-var)))
    (add-global 
     (format 
      "~a = internal constant DATA {i8* inttoptr (i32 ~a to i8*), i8 T_INT};" 
	      var-name
	      e))
    var-name))

(define (compile-call e)
  (let ((args (map (lambda (e1) (compile e1)) (cdr e))))
    (display (format "CAN'T COMPILE CALL: ~a\n" args))))

(define (compile-literal e)
  (cond 
   ((number? e) (compile-number-literal e))
   ((pair? e) (let ((var-name (next-literal-var))
		    (h (compile-literal (car e)))
		    (t (compile-literal (cdr e)))
		    (cons-var (next-literal-var)))
		(add-global
		 (format "~a = internal constant DATA\n" var-name))
		(add-global
		 (format "~a = internal constant CONS\n" cons-var))
		(add-global-init
		 (format 
		  "INIT_DATA(~a, ~a, CONS*, T_CONS)" var-name cons-var))
		(add-global-init
		 (format 
		  "SET_CAR(~a, ~a)" cons-var h))
		(add-global-init
		 (format 
		  "SET_CDR(~a, ~a)" cons-var t))
		var-name))
   (else (display "CAN'T COMPILE LITERAL: ")
	 (display e)
	 (newline))))

(define (compile e)
  (display (format "; Compile ~a\n" e))
  (let ((r (cond
	    ((number? e) (compile-literal e))
	    ((eq? 'quote (car e)) (compile-literal (cadr e)))
	    (#t (compile-call e))
	    (else (display (format
			    "ERROR: can't compile ~a\n" e))))))
    r))

(define (output-header)
  (display "#include \"runtime.ll.h\"\n")
  (display "define void @scheme_main() {\n"))

(define (output-footer)
  (display "ret void\n")
  (display "}\n")
  (display "define void @scheme_init() {\n")
  (display global-init)
  (display "ret void\n}\n")
  (display globals))

(define (read-and-compile)
  (output-header)
  (let loop ()
    (let ((e (read)))
      (if (not (eof-object? e))
	  (let ((r (compile e)))
	    (display (format 
		      "call %struct.Data* @display( %struct.Data* ~a ) \n"
		      r))
	    (loop))
	  (output-footer)))))

(read-and-compile)
