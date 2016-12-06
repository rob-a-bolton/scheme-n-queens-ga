(define-module (snqga genetics))

(use-modules (srfi srfi-1))

(define (make-chromosome n)
  "Creates a random n-length chromosome. Genes are (x . y) pairs"
  (let make-gene ((genes '()))
    (if (= (length genes) n)
	genes
	(let ((gene (cons (random n) (random n))))
	  (make-gene (if (member gene genes)
			 genes
			 (cons gene genes)))))))

(define (fitness chromosome)
  "Calculates the fitness of a chromosome. Fitness is [0,t], where
   t is the maximum possible takes a piece can have (n-triangle of n)"
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
  "Selects some of the fittest chromosomes."
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

(define (mutate-gene gene n)
  "Mutates either the X or Y of a gene."
  (if (< (random:uniform) 0.5)
      (cons (random n) (cdr gene))
      (cons (car gene) (random n))))

(define (mutate! chromosome)
  "Mutates a chromosome."
  (let* ((index (random (length chromosome)))
	 (gene (list-ref chromosome index))
	 (new-gene (mutate-gene gene (length chromosome))))
    (if (member new-gene chromosome)
	(mutate chromosome)
	(begin
	  (list-set! chromosome index new-gene)
	  chromosome))))

(define (crossover c1 c2)
  "Combines two chromosomes."
  (let* ((position (random (- (length c1) 1)))
	 (g1 (append (list-head c1 position)
		     (list-tail c2 position)))
	 (g2 (append (list-head c2 position)
		     (list-tail c1 position))))
    (values g1 g2)))

(define (chromosome->board chromosome)
  "Creates a printable string representation of a chromosome as a chess board."
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
