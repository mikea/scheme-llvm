(load "scheme-parser.scm")
(require-extension srfi-13)

(define literal-var-num 0)
(define local-var-num 0)
(define globals "")
(define global-init "")
(define environment '((car builtin) (cdr builtin)))
(define symbols ())

(define (next-local-var)
  (set! local-var-num (+ local-var-num 1))
  (string-append "%t." (number->string local-var-num)))
  
(define (next-literal-var)
  (set! literal-var-num (+ literal-var-num 1))
  (string-append "@L." (number->string literal-var-num)))

(define (add-global s)
  (set! globals (string-append globals s "\n")))

(define (add-global-init s)
  (set! global-init (string-append global-init s "\n")))

(define (gen-global-init . params)
  (add-global-init (apply format params)))

(define (gen-global . params)
  (add-global (apply format params)))

(define (gen . params)
  (display (apply format params))
  (newline))

(define (error . params)
  (display (apply format params))
  (newline))

(define (compile-number-literal e)
  (let ((var-name (next-literal-var)))
    (add-global 
     (format 
      "~a = internal constant DATA {i8* inttoptr (i32 ~a to i8*), i8 T_INT};" 
	      var-name
	      e))
    var-name))

(define (compile-symbol e)
  (let ((p (assoc e symbols)))
  (if p 
      (cdr p)
      (let* ((var-name (next-literal-var))
	     (var-str (next-literal-var))
	     (str (symbol->string e))
	     (str-ptr-var (next-local-var))
	     (res-ptr-var (next-local-var))
	     (call-res-var (next-local-var))
	     (result-var (next-local-var))
	     (array-len (+ (string-length str) 1)))
	(set! symbols (cons (list e var-name) symbols))
	(gen-global "~a = internal constant DATA* zeroinitializer; ~a" var-name e)
	(gen-global "~a = internal constant [~a x i8] c\"~a\\00\""
		    var-str array-len str)
	(gen-global-init "; init ~a" e)
	(gen-global-init "~A = getelementptr [~a x i8]* ~a, i64 0, i64 0" 
			 str-ptr-var array-len var-str)
	(gen-global-init "~a = call DATA* @string_to_symbol(i8* ~a)" 
			 call-res-var str-ptr-var)
	(gen-global-init "~a = getelementptr DATA** ~a, i64 0"
			 res-ptr-var var-name)
	(gen-global-init "store DATA* ~a, DATA** ~a"
			 call-res-var res-ptr-var)
	(gen "~a = getelementptr DATA** ~a, i64 0; ~a"
	     res-ptr-var var-name e)
	(gen "~a = load DATA** ~a"
	     result-var res-ptr-var)
	result-var))))

(define (compile-call e)
  (let* ((args (map (lambda (e1) (compile e1)) (cdr e)))
	 (symbol (car e))
	 (binding (assoc symbol environment)))
    (if binding
	(cond 
	 ((eq? (cadr binding) 'builtin) 
	  (let* ((var (next-local-var))
		 (args-with-types (map (lambda (s) (string-append "DATA* " s)) args))
		 (arglist (string-join args-with-types ", ")))
	    (gen "~a = call DATA* @~a(~a); call ~a" var symbol arglist symbol)
	    var))
	 (else (error "Can't compile ~a :  ~a" symbol binding)))
	(error "ERROR: ~a symbol not found" symbol))))

(define (compile-pair-literal e)
  (let ((var-name (next-literal-var))
	(h (compile-literal (car e)))
	(t (compile-literal (cdr e)))
	(cons-var (next-literal-var))
	(data-addr-var (next-local-var))
	(type-addr-var (next-local-var))
	(casted-value-var (next-local-var))
	(car-addr-var (next-local-var))
	(cdr-addr-var (next-local-var)))
    (gen-global "~a = internal constant DATA zeroinitializer; ~a" var-name e)
    (gen-global "~a = internal constant CONS zeroinitializer" cons-var)

    ;; Init DATA cell with pointer to CONS
    (gen-global-init "; init ~a" e)
    (gen-global-init  "~a = getelementptr DATA* ~a, i32 0, i32 0"
		      data-addr-var var-name)
    (gen-global-init "~a = bitcast CONS* ~a to i8*"
		     casted-value-var cons-var);
    (gen-global-init "store i8* ~a, i8* * ~a"
		     casted-value-var data-addr-var)
    (gen-global-init "~a = getelementptr DATA* ~a, i32 0, i32 1"
		     type-addr-var var-name)
    (gen-global-init "store i8 T_CONS, i8* ~a"
		     type-addr-var)

    ;; Setup CAR & CDR in CONS cell
    (gen-global-init "~a = getelementptr CONS* ~a, i32 0, i32 0"
		     car-addr-var cons-var)
    (gen-global-init "store DATA* ~a, DATA* * ~a"
		     h car-addr-var)
    (gen-global-init "~a = getelementptr CONS* ~a, i32 0, i32 1"
		     cdr-addr-var cons-var)
    (gen-global-init "store DATA* ~a, DATA* * ~a"
		     t cdr-addr-var)
    var-name))

(define (compile-literal e)
  (cond 
   ((number? e) (compile-number-literal e))
   ((pair? e) (compile-pair-literal e))
   ((symbol? e) (compile-symbol e))
   ((null? e) "inttoptr(i64 0 to DATA*)")
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
