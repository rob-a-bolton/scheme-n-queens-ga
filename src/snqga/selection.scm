(define-module (snqga selection))

(use-modules (snqga util)
	     (snqga chromosomes)
	     (srfi srfi-1))

(define (roulette config)
  "Roulette selection of chromosomes"
  (λ (chromosomes selection-size)    
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
		    (> (car pair1) (car pair2))))))
      (let take-more ((survivors '())
		      (num-survivors 1))
	(let* ((r (random:uniform))
	       (individual (choose-normalised normalised-pairs)))
	  (if (= num-survivors selection-size)
	      (cons individual survivors)
	      (take-more (cons individual survivors)
			 (1+ num-survivors))))))))

(define (tournament config)
  "Tournament selection of chromosomes."
  (let ((tournament-size (or (alist-get config 'tournament-size)
			     2)))
    (define (take-candidates chromosomes candidates num-candidates)
      (if (= num-candidates tournament-size)
	  candidates
	  (take-candidates chromosomes
			   (cons (list-ref chromosomes
					   (random (length chromosomes)))
				 candidates)
			   (1+ num-candidates))))
    (define (run-tournament chromosomes)
      (cdr (reduce (λ (c1 c2)
		     (if (> (car c1) (car c2))
			 c1
			 c2))
		   #f
		   (map (λ (c)
			  (cons (fitness c) c))
			(take-candidates chromosomes '() 0)))))
    (λ (chromosomes selection-size)
      (let choose-more ((chosen '())
			(num-chosen 0))
	(if (= num-chosen selection-size)
	    chosen
	    (choose-more (cons (run-tournament chromosomes)
			       chosen)
			 (1+ num-chosen)))))))

(define (rank config)
  "Ranked generator. Sorts by fitness, assignes a sequential rank to each, then
   uses the rank as the weight rather than the fitness."
  (λ (chromosomes selection-size)
    (let* ((fitness-pairs (reverse (map-sort-fit chromosomes)))
	   (total-rank (n-triangle (length fitness-pairs)))
	   (ranked (let rank-cdr ((ranked '())
				  (rest fitness-pairs)
				  (count 1))
		     (if (null? rest)
			 ranked
			 (rank-cdr (cons (cons (/ count total-rank) (cdar rest)) ranked)
				   (cdr rest)
				   (1+ count))))))
      (let choose-more ((chosen '())
			(num-chosen 0))
	(if (= num-chosen selection-size)
	    chosen
	    (cons (choose-normalised ranked) chosen))))))

(define (truncate config)
  "Truncation generator. Sorts by fitness and takes the most fit."
  (λ (chromosomes selection-size)
    (let ((fitness-pairs (map-sort-fit chromosomes)))
      (map cdr (list-head fitness-pairs selection-size)))))

(define selection-funcs
  `((roulette . ,roulette)
    (tournament . ,tournament)
    (rank . ,rank)
    (truncate . ,truncate)))

(define (generate-selector name config)
  "Looks up and returns a selection function by name."
  ((alist-get selection-funcs name) config))
    

(export selection-funcs generate-selector)
