(define-module (snqga selection))

(use-modules (snqga util)
	     (snqga chromosomes)
	     (srfi srfi-1))

(define (roulette config)
  "Roulette selection of chromosomes"
  (lambda (chromosomes selection-size)    
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
	       (individual
		(let select-individual ((rest normalised-pairs)
					(total (caar normalised-pairs)))
		  (if (> total r)
		      (cdar rest)
		      (select-individual (cdr rest)
					 (+ total (caadr normalised-pairs)))))))
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
    (lambda (chromosomes selection-size)
      (let choose-more ((chosen '())
			(num-chosen 0))
	(if (= num-chosen selection-size)
	    chosen
	    (choose-more (cons (run-tournament chromosomes)
			       chosen)
			 (1+ num-chosen)))))))

(define selection-funcs
  `((roulette . ,roulette)
    (tournament . ,tournament)))

(define (generate-selector name config)
  "Looks up and returns a selection function by name."
  ((alist-get selection-funcs name) config))
    

(export selection-funcs generate-selector)
