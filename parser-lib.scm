(require-extension syntax-case)

(module parser-lib
	(any-char char seq parse-one parser nop)

(define (parse-one f s p) ((f) s p))

(define (nop)
  (lambda (s p)
    (list #t p)))

(define (any-char) 
  (lambda (s p)
    (if (< p (string-length s))
	(list (substring s p (+ p 1)) 
	      (+ p 1))
	#f)))

(define (char c)
  (lambda (s p)
    (let ((r ((any-char) s p)))
      (if (and r (equal? (car r) c))
	  r
	  #f)
      )
    ))

(define (seq p1 p2)
  (lambda (s p)
    (let ((r1 (p1 s p)))
      (if r1
	  (let ((r2 (p2 s (cadr r1))))
	    (if r2
		(list (list (car r1) (car r2)) (cadr r2))
		#f))
	  #f)
      )))

(define-syntax parser
  (syntax-rules ()
    ((parser ()) (lambda (s p) (list #t p)))
    ((parser ((v <- head-parser) . tail)) 
     (lambda (s p) 
       (let* ((head-result (head-parser s p)))
	 (if head-result
	     (let* ((v (car head-result))
		    (tail-parser (parser tail))
		    (tail-pos (cadr head-result))
		    (tail-result (tail-parser s tail-pos)))
	       (list (car tail-result) (cadr tail-result)))
	     #f))
     ))
    ((parser ((return e))) (lambda (s p) (list e p)))
))

;; TBD: (matches "token"),  (while), (while-char space?), (choice)


)