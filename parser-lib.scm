;; The module defines haskell-style parser combinators.
;;
;; A parser is a function of two arguments:
;;   - string which is parsed
;;   - offset to start parsing from
;; Parser function returns either:
;;   - #f if parsing was not successful
;;   - a pair (parsing_result . new_starting_offset)

(require-extension syntax-case)

(module parser-lib
	(any-char char seq parser nop
		  while-char while1-char digit? letter? choice matches
		  if-char str-seq while while1)

(define nop
  (lambda (s p)
    (cons #t p)))

;; Create a parser which accepts any char
(define any-char 
  (lambda (s p)
    (if (< p (string-length s))
	(cons (substring s p (+ p 1)) 
	      (+ p 1))
	#f)))

;; Create a parser which accepts a given char
(define (char c)
  (lambda (s p)
    (let ((r (any-char s p)))
      (if (and r (equal? (car r) c))
	  r
	  #f)
      )
    ))

;; Create a parser which accepts a char if predicate is true on it
(define (if-char predicate)
  (lambda (s i)
    (let ((r (any-char s i)))
	(if (and r (predicate (string-ref (car r) 0)))
	    r
	    #f))))

(define (while parser)
  (lambda (s i)
    (let ((len (string-length s)))
      (let loop ((j i)  (result ()))
	(if (< j len)
	    (let ((r (parser s j)))
	      (if r
		  (loop (+ j 1) (append result (list (car r))))
		  (cons result j)))
	    (cons result j))))))
		  
;; The same as while, but succeeds only if parser matched at least 1 times
(define (while1 parser)
  (lambda (s i)
    (let ((r ((while parser) s i)))
      (if (and r (> (length (car r)) 0))
	  r
	  #f))))

;; Create a parser which accepts all chars while predicate is true. 
;; The result is the substring which matched.
(define (while-char predicate)
  (parser ((r <- (while (if-char predicate)))
	   (return (apply string-append r)))))

;; Create a parser which accepts all chars while predicate is true. 
;; The result is the substring which matched.
(define (while1-char predicate)
  (parser ((r <- (while1 (if-char predicate)))
	   (return (apply string-append r)))))


;; A parser, which calls all passed parsers consequently. The result of
;; the successful parse is the list of parsers results.
(define (seq . parsers)
  (lambda (s i)
    (let loop ((pos i) (pp parsers) (result ()))
      (if (pair? pp)
	  (let ((r ((car pp) s pos)))
	    (if r
		(loop (cdr r) (cdr pp) (append result (list (car r))))
		#f))
	  (cons result pos)))))

;; Calls parsers consequently, requires all parsers to return strings.
;; The result of this parser is concatenated string of all parsers results.
(define (str-seq . parsers)
  (parser ((r <- (apply seq parsers))
	   (return (apply string-append r)))))

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

(define (letter? c)
  (and (char<=? #\a c) (char>=? #\z c)))
)