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
  (parser i <- (choice (str-seq (if-char initial?) (while-char subsequent?))
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
	  (matches "\"")
	  return (cons 'string (apply string-append s))))

(define boolean fail) ;; TODO
(define number fail) ;; TODO
(define character fail) ;; TODO

;; External representations
(define symbol identifier)

(define simple-datum
  (choice boolean
	  number
	  character
	  string
	  symbol))

(define compound-datum fail) ;; TODO

(define datum 
  (choice simple-datum
	  compound-datum))

;; Expressions

(define quotation
  (choice (parser (matches "'")
		  d <- datum
		  return (cons 'quote d))
	  (seq (matches "(quote")
	       datum
	       (matches ")"))))
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
