(require-extension syntax-case)

(use test)
(load "scheme-parser.scm")


(test-group "identifier"
	    (test '((id . "abc") . 3) (identifier "abc" 0))
	    (test '((id . "...") . 3) (identifier "..." 0))
	    (test '((id . "ab@cd") . 5) (identifier "ab@cd" 0))
	    (test '((id . "$ab@cd") . 6) (identifier "$ab@cd" 0))
	    (test #f (identifier "@ab@cd" 0))
	    (test '((id . "+") . 1) (identifier "+34abc" 0))
	    (test #f (identifier "34abc" 0))
)

(test-exit)