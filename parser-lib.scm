(require-extension syntax-case)

(module parser-lib
	(any-char char seq parse-one parser nop
		  while-char digit? choice matches)

;; The module defines haskell-style parser combinators.
;;
;; A parser is a function of two arguments:
;;   - string which is parsed
;;   - offset to start parsing from
;; Parser function returns either:
;;   - #f if parsing was not successful
;;   - a pair (parsing_result . new_starting_offset)

(define (parse-one f s p) ((f) s p))

(define (nop)
  (lambda (s p)
    (cons #t p)))

;; Create a parser which accepts any char
(define (any-char) 
  (lambda (s p)
    (if (< p (string-length s))
	(cons (substring s p (+ p 1)) 
	      (+ p 1))
	#f)))

;; Create a parser which accepts a given char
(define (char c)
  (lambda (s p)
    (let ((r ((any-char) s p)))
      (if (and r (equal? (car r) c))
	  r
	  #f)
      )
    ))

;; Create a parser which accepts all chars while predicate is true
(define (while-char predicate)
  (lambda (s p)
    (let ((len (string-length s)))
      (let loop ((i p))
	(if (and (< i len) (predicate (string-ref s i)))
	    (loop (+ i 1))
	    (if (eq? p i)
		#f
		(cons (substring s p i) i)))))))

(define (seq p1 p2)
  (lambda (s p)
    (let ((r1 (p1 s p)))
      (if r1
	  (let ((r2 (p2 s (cdr r1))))
	    (if r2
		(cons (list (car r1) (car r2)) (cdr r2))
		#f))
	  #f)
      )))

(define-syntax parser
  (syntax-rules ()
    ((parser ()) (lambda (s p) (cons #t p)))
    ((parser ((v <- head-parser) . tail)) 
     (lambda (s p) 
       (let* ((head-result (head-parser s p)))
	 (if head-result
	     (let* ((v (car head-result))
		    (tail-parser (parser tail))
		    (head-pos (cdr head-result))
		    (tail-result (tail-parser s head-pos)))
	       (cons (car tail-result) (cdr tail-result)))
	     #f))
     ))
    ((parser ((return e))) (lambda (s p) (cons e p)))
))

;; TBD: (matches "token")

;; Tries to parse by each of the parsers consequently. 
;; Returns the result of first successful one.
(define (choice . parsers)
  (lambda (s i)
    (let loop ((p parsers))
      (if (pair? p)
	  (let ((r ((car p) s i)))
	    (if r
		r
		(loop (cdr p))))
	  #f))))

;; Tries to match a specified string
(define (matches m)
  (lambda (s i)
    (let ((n (string-length m)))
      (if (and (<= (+ i n) (string-length s))
               (string=? m (substring s i (+ i n))))
          (cons m (+ i n))
          #f))))



;; Some useful predicates

(define (digit? c)
  (and (char<=? #\0 c) (char>=? #\9 c)))

)