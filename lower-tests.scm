(use test)
(load "lower.scm")

(test-group "primitve-expr"
	    (test "abc" (lower "abc"))
	    (test '(12) (lower '(12))))

(test-group "define"
	    (test '((let ((x 5)) x))
		  (lower '((define x 5) 
			   x))))
	     

(test-exit)