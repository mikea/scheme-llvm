(load "scheme-parser.scm")
(require-extension srfi-13)

(define literal-var-num 0)
(define local-var-num 0)
(define globals "")
(define global-init "")
(define initial-environment '((car "@car") (cdr "@cdr") (+ "@add")))
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

(define (gen-to-list instruction-list . params)
  (instruction-list (string-append (apply format params))))

(define main-list
  (lambda (s)
    (display s)
    (newline)))

(define global-list
  (lambda (s)
    (add-global s)))

(define global-init-list
  (lambda (s)
    (add-global-init s)))

(define (error i . params)
  (i (string-append "ERROR: " (apply format params) "\n")))

(define (compile-number-literal e i)
  (let ((var-name (next-literal-var)))
    (add-global 
     (format 
      "~a = internal constant DATA {i8* inttoptr (i32 ~a to i8*), i8 T_INT};" 
	      var-name
	      e))
    var-name))

(define (compile-symbol e i)
  (let ((p (assoc e symbols)))
  (if p 
      (let* ((var-name (cadr p))
	     (res-ptr-var-load (next-local-var))
	     (result-var (next-local-var)))
	(gen-to-list i "; load ~a" e)
	(gen-to-list i "~a = getelementptr DATA** ~a, i64 0; ~a"
		     res-ptr-var-load var-name e)
	(gen-to-list i "~a = load DATA** ~a"
		     result-var res-ptr-var-load)
	result-var)
      (let* ((var-name (next-literal-var))
	     (var-str (next-literal-var))
	     (str (symbol->string e))
	     (str-ptr-var (next-local-var))
	     (res-ptr-var (next-local-var))
	     (res-ptr-var-load (next-local-var))
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
	(gen-to-list i "; load ~a" e)
	(gen-to-list i "~a = getelementptr DATA** ~a, i64 0; ~a"
	     res-ptr-var-load var-name e)
	(gen-to-list i "~a = load DATA** ~a"
	     result-var res-ptr-var-load)
	result-var))))

(define (compile-lambda e i env)
  (let* ((formals (cadr e))
	 (body (caddr e))
	 (proc-name (next-literal-var))
	 (formals-definition (map (lambda (x) (format "DATA* %~a" x)) formals))
	 (add-env (map (lambda (x) (list x (format "%~a" x))) formals)))
    (gen-global "define DATA* ~a(~a) {; ~a" 
		proc-name (string-join formals-definition ",") e)
    (let ((ret (compile body global-list (append add-env env))))
      (gen-global "ret DATA* ~a" ret)
      (gen-global "}"))
    proc-name))

(define (compile-call e i env)
  (let* ((args (map (lambda (e1) (compile e1 i env)) (cdr e)))
	(args-with-types 
	 (map (lambda (s) (string-append "DATA* " s)) args))
	(var (next-local-var))
	(arglist (string-join args-with-types ", ")))
    (if (symbol? (car e))
	;; compile call
	(let* ((symbol (car e))
	      (binding (assoc symbol env)))
	  (if binding
	      (begin
      		(gen-to-list i "~a = call DATA* ~a(~a); call ~a" 
			     var (cadr binding) arglist symbol)
		var)
	      (error i "~a symbol not found" symbol)))
	(if (eq? 'lambda (caar e))
	    ;; compile lambda
	    (let ((lambda-var (compile-lambda (car e) i env)))
	      (gen-to-list i "~a = call DATA* ~a(~a)" 
			   var lambda-var arglist)
	      var)
	    (error i "Can't compile call: ~a" e)))))

(define (compile-pair-literal e i)
  (let ((var-name (next-literal-var))
	(h (compile-literal (car e) global-init-list))
	(t (compile-literal (cdr e) global-init-list))
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
    (gen-global-init "~a = getelementptr CONS* ~a, i32 0, i32 0; car"
		     car-addr-var cons-var)
    (gen-global-init "store DATA* ~a, DATA* * ~a"
		     h car-addr-var)
    (gen-global-init "~a = getelementptr CONS* ~a, i32 0, i32 1; cdr"
		     cdr-addr-var cons-var)
    (gen-global-init "store DATA* ~a, DATA* * ~a"
		     t cdr-addr-var)
    var-name))

(define (compile-literal e i)
  (cond 
   ((number? e) (compile-number-literal e i))
   ((pair? e) (compile-pair-literal e i))
   ((symbol? e) (compile-symbol e i))
   ((null? e) "inttoptr(i64 0 to DATA*)")
   (else (i "CAN'T COMPILE LITERAL: ")
	 (i e)
	 (i "\n"))))

(define (compile-define e i env)
  (error i "can't compile define: ~a" e))


(define (gen-sizeof type i)
  (let ((sz (next-local-var))
	(szi (next-local-var)))
    (gen-to-list i "; sizeof ~a" type)
    (gen-to-list i "~a = getelementptr ~a* null, i32 1" sz type)
    (gen-to-list i "~a = ptrtoint ~a* ~a to i32" szi type sz)
    szi))

;; compiles let definitions
;; returns new environment
(define (compile-let-defs defs i env)
  (let loop ((d defs))
    (if (null? d)
	env
	(let* ((def (car d))
	       (def-name (car def))
	       (var (next-local-var))
	       (val (compile (cadr def) i env))
	       (val-addr (next-local-var))
	       (var-addr (next-local-var))
	       (data-sizeof (gen-sizeof "DATA" i)))
	  (gen-to-list i "; init ~a" def-name)
	  (gen-to-list i "~a = malloc DATA" var)
	  (gen-to-list i "~a = bitcast DATA* ~a to i8*" val-addr val)
	  (gen-to-list i "~a = bitcast DATA* ~a to i8*" var-addr var)
	  (gen-to-list i "call void @llvm.memcpy.i32(i8* ~a, i8* ~a, i32 ~a, i32 0)"
		       var-addr val-addr data-sizeof)
	  (cons (list def-name var) (loop (cdr d)))))))
	

(define (compile-let e i env)
  (let* ((defs (cadr e))
	 (body (cddr e))
	 (new-env (compile-let-defs defs i env)))
    (compile-list body i new-env)))

(define (compile e i env)
  (gen-to-list i "; compile ~a in ~a" e env)
  (let ((r (cond
	    ((number? e) (compile-literal e main-list))
	    ((pair? e)
	     (cond
	      ((eq? 'quote (car e)) (compile-literal (cadr e) main-list))
	      ((eq? 'let (car e)) (compile-let e main-list env))
	      (#t (compile-call e i env))))
	    (else 
	     (let ((binding (assoc e env)))
	       (if binding
		   (cadr binding)
		   (error i "can't compile ~a in env: ~a" e env)))))))
    r))

(define (compile-list exprs i env)
  (let loop ((ee exprs))
    (if (not (null? (cdr ee)))
	(begin
	  (compile (car ee) i env)
	  (loop (cdr ee)))
	(compile (car ee) i env))))

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

(define (read-list)
  (let loop ()
    (let ((e (read)))
      (if (not (eof-object? e))
	  (cons e (loop))
	  ()))))

(define (read-and-compile)
  (output-header)
    (let ((ee (read-list)))
      (let ((r (compile-list ee main-list initial-environment)))
	(display (format 
		  "call %struct.Data* @display( %struct.Data* ~a ) \n"
		  r))))
    (output-footer))

(read-and-compile)
