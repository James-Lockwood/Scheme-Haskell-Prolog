;(a) Finger Exercises
(define set-cardinality 
    (lambda (set)
        (if (null? set) 0 (+ 1 (set-cardinality (cdr set))))))

(define set-union
	(lambda (set1 s2)
		(cond ((null? set1) set2)
			  ((member (car set1) set2)
			  (set-union (cdr set1) set2))
			  (else (cons (car set1) (set-union (cdr set1) set2))))))

(define set-intersection
	(lambda (set1 set2)
		(cond ((null? set1) '())
			  ((member (car set1) set2)
			  (cons (car set1) (set-intersection (cdr set1) set2)))
		      (else (set-intersection (cdr set1) set2)))))

(define set-difference
	(lambda (set1 set2)
		(cond ((null? set1) '())
			  ((member (car set1) set2)
			  (set-difference (cdr set1) set2))
		      (else (cons (car set1) (set-difference (cdr set1) set2))))))

(define set-equal? 
	(lambda (set1 set2)
		(and (subset? set1 set2)
			 (subset? set2 set1))))

(define subset?
	(lambda (set1 set2)
		(or (null? set1)
			(and (member (car set1) set2) 
				 (subset? (cdr set1) set2)))))

(define set-map-join
	(lambda (f s)
		(foldr set-union '() (map f s))))


;(b) Lambda Calculus Manipulation
(define flat
	(lambda (y)
    		(cond ((null? y) '())
          	      ((not (pair? y)) (list y))
          	(else (append (flat (car y))
                        (flat (cdr y)))))))

(define n_a_l
	(lambda (x)
		(cond ((= (set-cardinality x) 1) x)
		      ((equal? (car x) 'lambda) (n_a_l (cddr x)))
		(else (cons (car x) (n_a_l (cdr x)))))))

(define free-variables
	(lambda (z)
		(n_a_l(remove-duplicates (flatten z)))))



(define test-it
	(lambda (p q r s u)
		(display "Cardinality of a: ")
		(display (set-cardinality p)) (newline)
		(display "Union of b and c: ")
		(display (set-union q r)) (newline)
		(display "Intersection of b and c: ")
		(display (set-intersection q r)) (newline)
		(display "(A/B) Difference of b and c: ")
		(display (set-difference q r))(newline)
		(display "Are b and c equal?: ")
		(display (set-equal? q r)) (newline)
		(display "Are c and c equal?: ")
 		(display (set-equal? q q)) (newline)
		(display "Mapping of function d onto set e: ")
		(display (set-map-join r s)) (newline)
		(display "Free variables in lambda function f: ")
		(display (free-variables u))))



(test-it '(1 2 5 7 13 19) '(a b c d e) '(c d e f g) (lambda (e) (list (+ e 1) (* e 10))) '(1 2 3 4) '((a b) (lambda c ((d c) (e b)))))
