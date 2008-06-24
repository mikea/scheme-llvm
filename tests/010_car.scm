(car '(1 2))
;; 1
(car '(2 3 4))
;; 2
(car '(5 . 6))
;; 5
(car '(a b c))
;; a
(car '((a) b c d))
;; (a)
(car '())
;; <failure>
