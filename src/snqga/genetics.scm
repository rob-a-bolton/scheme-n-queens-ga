(define-module (snqga genetics))

(use-modules (snqga chromosomes)
	     (snqga mutation)
	     (snqga crossover))

(define (select chromosomes)
  "Splits a space of chromosomes in two."
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
	  (values taken (map cdr remaining))
	  (take-more (cons (cdar remaining) taken)
		     (cdr remaining)
		     (+ total (caar remaining)))))))

(define (run-generation generation
			select-function
			crossover-chance
			mutation-chance)
  "Runs over a single generation. Performs selection, crossover, and mutation."
  (let*-values (((selected rest) (select generation))
		((crossed-over) (crossover-generation selected crossover-chance))
		((combined) (append crossed-over rest)))
      (map (λ (chromosome)
	     (if (< (random:uniform) mutation-chance)
		 (mutate chromosome)
		 chromosome))
	   combined)))

(define (solve-problem n
		       generation-size
		       max-generations
		       selection-func
		       crossover-func)
  "Attempts to solve n-queens. Gives up after max-generations is exceeded."
  (let solve ((generation (make-generation generation-size n))
	      (generations 1))
    (let ((winner (get-winner generation)))
      (cond
       (winner
	winner)
       ((= generations max-generations)
	#f)
       (else
	(solve (run-generation generation selection-func crossover-func 0.1)
	       (1+ generations)))))))

(define (make-generation size n)
  "Makes an n-sized generation of random chromosomes."
  (map make-chromosome (make-list size n)))

(define (get-winner generation)
  "Returns the winning chromosome (or false if none) from a generation."
  (let* ((fitness-pairs (map (λ (c) (cons (fitness c) c))
			     generation))
	 (max-f (max-fitness (length (car generation)))))
    (find (λ (f-pair)
	    (= max-f (car f-pair)))
	  fitness-pairs)))

(export make-chromosome fitness chromosome->board)
