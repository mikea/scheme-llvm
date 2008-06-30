((lambda (x) x) '(1 2))
;; (1 2)

((lambda (x) (car x)) '(1 2))
;; 1

((lambda (x y) (+ x y)) 5 6)
;; 11