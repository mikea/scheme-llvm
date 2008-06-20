(require-extension syntax-case)
(require-extension srfi-1)

(load "parser-lib.scm")
(import parser-lib)

(define (special-initial? c)
  (or (char=? #\! c)
      (char=? #\$ c)
      (char=? #\% c)
      (char=? #\& c)
      (char=? #\* c)
      (char=? #\/ c)
      (char=? #\: c)
      (char=? #\< c)
      (char=? #\= c)
      (char=? #\> c)
      (char=? #\? c)
      (char=? #\^ c)
      (char=? #\_ c)
      (char=? #\~ c)))

(define (initial? c)
  (or (letter? c) (special-initial? c)))

(define (special-subsequent? c)
  (or (char=? #\+ c)
      (char=? #\- c)
      (char=? #\. c)
      (char=? #\@ c)))

(define (subsequent? c)
  (or (initial? c)
      (digit? c)
      (special-subsequent? c)))

(define (whitespace? c)
  (or (char=? #\space c)
      (char=? #\newline c)))

(define (!whitespace? c)
  (not (whitespace? c)))

(define (digitr? r)
  (lambda (c)
    (case r
      ((2) (and (char<=? #\0 c) (char>=? #\1 c)))
      ((8) (and (char<=? #\0 c) (char>=? #\7 c)))
      ((10) (and (char<=? #\0 c) (char>=? #\9 c)))
      ((16) (or (and (char<=? #\0 c) (char>=? #\9 c))  
		(and (char<=? #\a c) (char>=? #\f c))))
      (else #f))))

(define (hash? c)
  (char=? #\# c))

(define (token s)
  (parser t <- (matches s)
	  (while-char whitespace?)
	  return t))

(define peculiar-identifier
  (choice (token "+")
	  (token "-")
	  (token "...")))

(define identifier
  (parser i <- (choice (str-seq (if-char initial?) 
				(while-char subsequent?))
		       peculiar-identifier)
	  return (cons 'id i)))

(define string-element
  (choice (if-char (lambda (c) 
		     (not (or (char=? #\" c) (char=? #\\ c)))))
	  (parser (matches "\\\"")
		  return "\"")
	  (parser (matches "\\\\")
		  return "\\")))

(define string
  (parser (matches "\"")
	  s <- (while string-element)
	  (token "\"")
	  return (apply string-append s)))

(define boolean fail) ;; TODO
(define character fail) ;; TODO

(define (digit r)
  (parser c <- (if-char (digitr? r))
	  return (- (char->integer (string-ref c 0)) (char->integer #\0))))

(define sign
  (choice (parser (token "+")
		  return 1)
	  (parser (token "-")
		  return -1)
	  (parser nop
		  return 1)))

(define exponent-marker
  (choice (token "e")
	  (token "s")
	  (token "f")
	  (token "d")
	  (token "l")))

(define suffix
  (choice (seq exponent-marker sign (while (digit 10)))
	  nop))

(define (decimal r)
  (case r
    ((10) (choice (seq (uinteger 10) suffix)
		  (seq (matches ".") (while1 (digit 10)) (while-char hash?) suffix)
		  (seq (while1 (digit 10)) (matches ".") (while (digit 10)) 
		       (while-char hash?) suffix)
		  (seq (while1 (digit 10)) (while1-char hash?) (matches ".")
		       (while-char hash?) suffix)))
    (else fail)))


(define (uinteger r)
  (parser d <- (while1 (digit r)) 
	  h <- (while-char hash?)
	  return (let ((i1 (fold (lambda (d1 d2) (+ (* d2 r) d1)) 0 d))
		       (h (expt r (string-length h))))
		   (* i1 h))))

(define (ureal r)
  (choice (seq (uinteger r) (token "/") (uinteger r))
	  (uinteger r)
	  (decimal r)))

(define (real r)
  (parser s <- sign 
	  u <- (ureal r)
	  return (* s u)))

(define (complex r)
  (choice (seq (real r) (token "@") (real r))
	  (seq (real r) (token "+") (ureal r) (token "i"))
	  (seq (real r) (token "-") (ureal r) (token "i"))
	  (seq (real r) (token "+") (token "i"))
	  (seq (real r) (token "-") (token "i"))
	  (seq (token "+") (ureal r) (token "i"))
	  (seq (token "-") (ureal r) (token "i"))
	  (seq (token "+") (token "i"))
	  (seq (token "-") (token "i"))
	  (real r)))

(define exactness
  (choice (parser (matches "#i")
		  return (lambda (n) (exact->inexact n)))
	  (parser (matches "#e")
		  return (lambda (n) (inexact->exact n)))
	  (parser nop
		  return (lambda (n) n))))

(define (radix r)
  (case r
    ((2) (matches "#b"))
    ((8) (matches "#o"))
    ((16) (matches "#x"))
    ((10) (choice (matches "#d")
		  nop))
    (else fail)))

(define (prefix r)
  (choice (parser r <- (radix r) 
		  e <- exactness
		  return (cons e r))
	  (parser e <- exactness 
		  r <- (radix r)
		  return (cons e r))))

(define (num r) 
  (parser p <- (prefix r)
	  n <- (complex r)
	  return ((car p) n)))

(define number 
  (choice (num 2)
	  (num 8)
	  (num 10)
	  (num 16)))

;; External representations
(define symbol identifier)

(define simple-datum
  (parser r <- (choice boolean
		       number
		       character
		       string
		       symbol)
	  (while-char whitespace?)
	  return r))

(define vector fail) ;; TODO
(define abbreviation fail) ;; TODO

(define datum 
  (letrec ((_datum (lambda (s i) ((choice simple-datum compound-datum) s i)))
	   (_list (lambda (s i) 
		    ((choice (parser (token "(") 
				     l <- (while _datum) 
				     (token ")")
				     return l)
			     (parser (token "(") 
				     l <- (while1 _datum) 
				     (token ".") 
				     t <- _datum 
				     (token ")")
				     return (let ((tt (last-pair l)))
					      (begin
						(set-cdr! tt t)
						l)))
			     abbreviation) s i)))
	   (compound-datum (lambda (s i) ((choice _list vector) s i))))
    _datum))

;; Expressions

(define quotation
  (choice (parser (matches "'")
		  d <- datum
		  return (cons 'quote d))
	  (parser (token "(quote")
	       d <- datum
	       (token ")")
	       return (cons 'quote d))))
(define self-evaluating
  (choice boolean
	  number
	  character
	  string))

(define literal
  (parser l <- (choice quotation self-evaluating)
	  return (cons 'literal l)))

(define variable fail) ;; TODO
(define procedure-call fail) ;; TODO
(define lambda-expression fail) ;; TODO
(define conditional fail) ;; TODO
(define assignment fail) ;; TODO
(define derived-expression fail) ;; TODO
(define macro-use fail) ;; TODO
(define macro-block fail) ;; TODO

(define expression
  (choice variable
	  literal
	  procedure-call
	  lambda-expression
	  conditional
	  assignment
	  derived-expression
	  macro-use
	  macro-block))

(define command expression)

;; Programs and definitions
(define definition fail) ;; TODO
(define syntax-definition fail) ;; TODO

;; (define command-or-definition
;;   (choice command
;; 	  definition
;; 	  syntax-definition
;; 	  (seq (matches "(begin")
;; 	       (while1 command-or-definition)
;; 	       (matches ")"))))

;; (define program
;;   (while command-or-definition))
