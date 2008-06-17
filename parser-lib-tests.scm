(require-extension syntax-case)

(use test)
(load "parser-lib.scm")
(import parser-lib)

(test-group "nop"
	    (test '(#t . 0) (parse-one nop "abc" 0)))

(test-group "any-char"
	    (test '("a" . 1) (parse-one any-char "abc" 0))
	    (test '("b" . 2) (parse-one any-char "abc" 1))
	    (test '("c" . 3) (parse-one any-char "abc" 2))
	    (test #f (parse-one any-char "abc" 3))
	    )

(test-group "char"
	    (test '("a" . 1) ((char "a") "abc" 0))
	    (test #f ((char "b") "abc" 0))
	    (test #f ((char "b") "abc" 3))
	    )

(test-group "seq"
	    (test '(("a" "b") . 2) ((seq (any-char) (any-char)) "abc" 0))
	    (test #f ((seq (any-char) (any-char)) "abc" 2))
	    (test '(("a" "b") . 2) ((seq (char "a") (char "b")) "abc" 0))
	    (test #f ((seq (char "b") (char "b")) "abc" 0))
	    (test #f ((seq (char "a") (char "c")) "abc" 0)) 
	    )

(test-group "parser"
	    (test '(#t . 1) (let ((p (parser ((c1 <- (any-char))))))
			    (p "abc" 0)))
	    (test '(#t . 2) (let ((p (parser ((c1 <- (any-char)) 
					    (c2 <- (any-char)))
					   )))
			    (p "abc" 0)))
	    (test '(#t . 2) (let ((p (parser ((c1 <- (char "a")) 
					    (c2 <- (any-char)))
					   )))
			    (p "abc" 0)))
	    (test #f (let ((p (parser ((c1 <- (char "a")) 
				       (c2 <- (any-char)))
				      )))
		       (p "dabc" 0)))
	    (test '("ab" . 2) (let ((p (parser ((c1 <- (any-char)) 
					      (c2 <- (any-char))
					      (return (string-append c1 c2)))
					     )))
			    (p "abc" 0)))
)

(test-group "while-char"
	    (test '("123" . 3) ((while-char digit?) "123abc" 0))
	    (test '("123" . 3) ((while-char digit?) "123" 0))
	    (test #f ((while-char digit?) "abc" 0))
)
