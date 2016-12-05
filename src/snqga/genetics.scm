(define-module (snqga genetics))

(use-modules (srfi srfi-1))

(define (make-chromosome n)
  (let make-gene ((genes '()))
    (if (= (length genes) n)
	genes
	(let ((gene (cons (random n) (random n))))
	  (make-gene (if (member gene genes)
			 genes
			 (cons gene genes)))))))

(define (fitness chromosome)
  (let* ((can-take (λ (g1 g2)
		    (and (not (eq? g1 g2))
			 (or (eq? (car g1) (car g2))
			     (eq? (cdr g1) (cdr g2))
			     (eq? (abs (- (car g1) (car g2)))
				  (abs (- (cdr g1) (cdr g2))))))))
	 (n (- (length chromosome) 1))
	 (max-fitness (/ (+ (* n n) n) 2)))
    (let fit-cdr ((takes 0) (genes chromosome))
      (if (= (length genes) 1)
	  (- max-fitness takes)
	  (fit-cdr (+ takes
		      (fold + 0 (map
				 (λ (g)
				   (if (can-take g (car genes))
				       1
				       0))
				     genes)))
		   (cdr genes))))))

(define (select chromosomes)
  (let* ((fitness-pairs (map (λ (chromosome)
			       (cons (fitness chromosome) chromosome))
			     chromosomes))
	 (total-fitness (fold (λ (pair total)
				(+ total (car pair)))
			      0
			      fitness-pairs))
	 (normalised-pairs
	  (sort (map (λ (pair)
		       (cons (/ (car pair) total-fitness) (cdr pair)))
		     fitness-pairs)
		(λ (pair1 pair2)
		  (> (car pair1) (car pair2)))))
	 (r (random:uniform)))
    (let take-more ((taken '())
		    (remaining normalised-pairs)
		    (total 0))
      (if (> total r)
	  taken
	  (take-more (cons (cdar remaining) taken)
		     (cdr remaining)
		     (+ total (caar remaining)))))))

(define (chromosome->board chromosome)
  (let* ((n (length chromosome))
	 (board-strs (map (λ (chr) (make-string n chr))
			  (make-list n #\.))))
    (for-each (λ (gene)
		(let ((x (car gene))
		      (y (cdr gene)))
		  (string-set! (list-ref board-strs y) x #\q)))
	      chromosome)
    (string-join board-strs "\n")))
			       
(export make-chromosome fitness chromosome->board)
