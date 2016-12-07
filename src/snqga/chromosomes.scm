(define-module (snqga chromosomes))

(use-modules (srfi srfi-1)
	     (snqga util))

(define (make-chromosome n)
  "Creates a random n-length chromosome. Genes are (x . y) pairs"
  (let make-gene ((genes '()))
    (if (= (length genes) n)
	genes
	(let ((gene (cons (random n) (random n))))
	  (make-gene (if (member gene genes)
			 genes
			 (cons gene genes)))))))

(define (is-valid? chromosome)
  "Checks if a chromosome is a valid board state (no queens in same tile)"
  (let check-cdr ((this (car chromosome))
		  (rest (cdr chromosome)))
    (cond
     ((null? rest)
      #t)
     ((member this rest)
      #f)
     (else
      (check-cdr (car rest)
		 (cdr rest))))))

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

(define (max-fitness n)
  "Calculates the maximum fitness value for a chromosome of size n"
  (n-triangle (- n 1)))

(define (fitness chromosome)
  "Calculates the fitness of a chromosome. Fitness is [0,t], where
   t is the maximum possible takes a piece can have (n-triangle of n)"
  (let* ((can-take (λ (g1 g2)
		    (and (not (eq? g1 g2))
			 (or (eq? (car g1) (car g2))
			     (eq? (cdr g1) (cdr g2))
			     (eq? (abs (- (car g1) (car g2)))
				  (abs (- (cdr g1) (cdr g2))))))))
	 (max-f (max-fitness (length chromosome))))
    (let fit-cdr ((takes 0) (genes chromosome))
      (if (= (length genes) 1)
	  (- max-f takes)
	  (fit-cdr (+ takes
		      (fold + 0 (map
				 (λ (g)
				   (if (can-take g (car genes))
				       1
				       0))
				     genes)))
		   (cdr genes))))))

(define (map-sort-fit chromosomes)
  (sort (map (λ (chromosome)
	       (cons (fitness chromosome) chromosome))
	     chromosomes)
	(λ (pair-1 pair-2)
	  (> (car pair-1) (car pair-2)))))

(export make-chromosome
	is-valid?
	chromosome->board
	max-fitness
	fitness
	map-sort-fit)
