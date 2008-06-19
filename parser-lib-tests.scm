(require-extension syntax-case)

(use test)
(load "parser-lib.scm")
(import parser-lib)

(test-group "nop"
	    (test '(#t . 0) (nop "abc" 0)))

(test-group "any-char"
	    (test '("a" . 1) (any-char "abc" 0))
	    (test '("b" . 2) (any-char "abc" 1))
	    (test '("c" . 3) (any-char "abc" 2))
	    (test #f (any-char "abc" 3))
	    )

(test-group "char"
	    (test '("a" . 1) ((char "a") "abc" 0))
	    (test #f ((char "b") "abc" 0))
	    (test #f ((char "b") "abc" 3))
	    )

(test-group "if-char"
	    (test '("a" . 1) ((if-char letter?) "abc" 0))
	    (test #f ((if-char digit?) "abc" 0))
	    (test #f ((if-char letter?) "abc" 3))
	    )

(test-group "seq"
	    (test '(("a" "b") . 2) ((seq any-char any-char) "abc" 0))
	    (test #f ((seq any-char any-char) "abc" 2))
	    (test '(("a" "b") . 2) ((seq (char "a") 
					 (char "b")) "abc" 0))
	    (test #f ((seq (char "b") 
			   (char "b")) "abc" 0))
	    (test #f ((seq (char "a") 
			   (char "c")) "abc" 0)) 
	    )

(test-group "str-seq"
	    (test '("ab" . 2) ((str-seq any-char any-char) "abc" 0))
	    (test #f ((str-seq any-char any-char) "abc" 2))
	    (test '("ab" . 2) ((str-seq (char "a") (char "b")) "abc" 0))
	    (test #f ((str-seq (char "b") (char "b")) "abc" 0))
	    (test #f ((str-seq (char "a") (char "c")) "abc" 0)) 
	    )

(test-group "while"
	    (test '(("1" "2" "3") . 3)  ((while (if-char digit?)) "123abc" 0))
	    (test '(("1" "2" "3") . 3)  ((while (if-char digit?)) "123" 0))
	    (test '(("12" "12") . 4)  ((while (matches "12")) "12123" 0))
	    (test '(() . 0) ((while (if-char digit?)) "abc123abc" 0))
)

(test-group "while1"
	    (test '(("1" "2" "3") . 3)  ((while1 (if-char digit?)) "123abc" 0))
	    (test #f ((while1 (if-char digit?)) "abc123abc" 0))
)

(test-group "while-char"
	    (test '("123" . 3) ((while-char digit?) "123abc" 0))
	    (test '("123" . 3) ((while-char digit?) "123" 0))
	    (test '("" . 0) ((while-char digit?) "abc" 0))
)

(test-group "while1-char"
	    (test '("123" . 3) ((while1-char digit?) "123abc" 0))
	    (test '("123" . 3) ((while1-char digit?) "123" 0))
	    (test #f ((while1-char digit?) "abc" 0))
)

(test-group "choice"
	    (test '("123" . 3) ((choice (while1-char digit?)
			      (char "a")) "123abc" 0))
	    (test '("a" . 1) ((choice (while1-char digit?)
			      (char "a")) "abc123" 0))
	    (test #f ((choice (while1-char digit?)
			      (char "a")) "xabc123" 0))
)

(test-group "matches"
	    (test '("123" . 3) ((matches "123") "123abc" 0))
	    (test #f ((matches "123") "abc123" 0))
	    (test #f ((matches "1234") "123" 0))
)

(test-group "parser-macro"
	    (test '(#t . 1) (let ((p (parser ((c1 <- any-char)))))
			    (p "abc" 0)))
	    (test '(#t . 2) (let ((p (parser ((c1 <- any-char) 
					      (c2 <- any-char)))))
			      (p "abc" 0)))
	    (test '(#t . 2) (let ((p (parser ((c1 <- (char "a")) 
					      (c2 <- any-char)))))
			      (p "abc" 0)))
	    (test #f (let ((p (parser ((c1 <- (char "a")) 
				       (c2 <- any-char)))))
		       (p "dabc" 0)))
	    (test '("ab" . 2) (let ((p (parser ((c1 <- any-char) 
						(c2 <- any-char)
						(return (string-append c1 c2))))))
				(p "abc" 0)))
	    (test '("b" . 2) (let ((p (parser ((any-char) 
					       (c <- any-char)
					       (return c)))))
				(p "abc" 0)))
	    (test '("c" . 3) (let ((p (parser (((matches "ab")) 
					       (c <- any-char)
					       (return c)))))
				(p "abc" 0)))
)

(test-exit)