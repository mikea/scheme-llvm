;; The module defines haskell-style parser combinators.
;;
;; A parser is a function of two arguments:
;;   - string which is parsed
;;   - offset to start parsing from
;; Parser function returns either:
;;   - #f if parsing was not successful
;;   - a pair (parsing_result . new_starting_offset)
;; (require-extension syntax-case)
(require-extension r7rs)

(define-library parser-lib 
  (import (scheme base))
  (export any-char char choice if-char fail nop parser while while1 str-seq seq matches while-char while1-char)

	(begin
	
	(define (nop s p)
          (cons #t p))

	(define (fail s p)
          #f)

 ;; Accepts any char
 ;; TODO: should return a character not string
 (define (any-char s p)
         (if (< p (string-length s))
             (cons (substring s p (+ p 1)) (+ p 1))
             #f))

 ;; Accepts a char if predicate is true on it
 (define (if-char predicate)
  (lambda (s i)
   (let ((r (any-char s i)))
    (if (and r
             (predicate (string-ref (car r) 0)))
     r
     #f))))

 ;; Accepts a given char
 (define (char c)
 				 (if (char? c) 
					   (if-char (lambda (c1) (equal? c c1)))
						 (error "argument shoul be char" c)))

;; Accepts specified string
 (define (matches m)
  (lambda (s i)
   (let ((n (string-length m)))
    (if (and (<= (+ i n) (string-length s))
             (string=? m (substring s i (+ i n))))
     (cons m (+ i n))
     #f))))

 (define (while parser)
  (lambda (s i)
   (let ((len (string-length s)))
    (let loop ((j i)
               (result '()))
     (if (< j len)
      (let ((r (parser s j)))
       (if r
        (loop (cdr r) (append result (list (car r))))
        (cons result j)))
      (cons result j))))))

 ;; The same as while, but succeeds only if parser matched at least 1 times
 (define (while1 parser)
  (lambda (s i)
   (let ((r ((while parser) s i)))
    (if (and r
             (> (length (car r)) 0))
     r
     #f))))

 ;; A parser, which calls all passed parsers consequently. The result of
 ;; the successful parse is the list of parsers results.
 (define (seq . parsers)
	(lambda (s i)
  	(let loop ((pos i)
               (pp parsers)
               (result '()))
    		 (if (pair? pp)
     				 (let ((r ((car pp) s pos)))
     				 			(if r
       					 			(loop (cdr r) (cdr pp) (append result (list (car r))))
       					   #f))
     				 (cons result pos)))))

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



 (define-syntax parser
  (syntax-rules (<-)
   ((parser)
    (lambda (s p)
     (cons #t p)))
   ((parser v <- head-parser . tail)
    (lambda (s p)
     (let* ((head-result (head-parser s p)))
      (if head-result
       (let* ((v (car head-result))
              (tail-parser (parser . tail))
              (head-pos (cdr head-result))
              (tail-result (tail-parser s head-pos)))
        (if tail-result
         (cons (car tail-result) (cdr tail-result))
         #f))
       #f))))
   ((parser return e)
    (lambda (s p)
     (cons e p)))
   ((parser head-parser . tail)
    (lambda (s p)
     (let* ((head-result (head-parser s p)))
      (if head-result
       (let* ((v (car head-result))
              (tail-parser (parser . tail))
              (head-pos (cdr head-result))
              (tail-result (tail-parser s head-pos)))
        (if tail-result
         (cons (car tail-result) (cdr tail-result))
         #f))
       #f))))))


 ;; Calls parsers consequently, requires all parsers to return strings.
 ;; The result of this parser is concatenated string of all parsers results.
 (define (str-seq . parsers)
  (parser r <- (apply seq parsers) return (apply string-append r)))

 ;; Create a parser which accepts all chars while predicate is true. 
 ;; The result is the substring which matched.
 (define (while-char predicate)
  (parser r <- (while (if-char predicate)) return (apply string-append r)))

 ;; Create a parser which accepts all chars while predicate is true. 
 ;; The result is the substring which matched.
 (define (while1-char predicate)
  (parser r <- (while1 (if-char predicate)) return (apply string-append r)))

))