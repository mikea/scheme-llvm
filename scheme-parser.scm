(require-extension syntax-case)

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

(define peculiar-identifier
  (choice (matches "+")
	  (matches "-")
	  (matches "...")))

(define identifier
  (parser ((i <- (choice 
		  (str-seq (if-char initial?) (while-char subsequent?))
		  peculiar-identifier))
	   (return (cons 'id i)))))

(define string-element
  (choice (if-char (lambda (c) 
		     (not (or (char=? #\" c) (char=? #\\ c)))))
	  (parser (((matches "\\\""))
		   (return "\"")))
	  (parser (((matches "\\\\"))
		   (return "\\")))))

(define string
  (parser (((matches "\""))
	   (s <- (while string-element))
	   ((matches "\""))
	   (return (cons 'string (apply string-append s))))))
	   