(define-module (snqga genetics))

(use-modules (snqga chromosomes)
	     (snqga mutation)
	     (snqga crossover)
	     (srfi srfi-1)
	     (srfi srfi-11)
	     (ice-9 pretty-print))

(define (run-generation generation
			selection-function
			crossover-function
			parent-ratio
			mutation-chance)
  "Runs over a single generation. Performs selection, crossover, and mutation."
  (let* ((parents (selection-function generation (floor (/ (length generation) parent-ratio))))
	 (crossed-over (map-crossover parents crossover-function))
	 (combined (append crossed-over generation)))
    (selection-function
     (map (λ (chromosome)
	    (if (< (random:uniform) mutation-chance)
		(mutate chromosome)
		chromosome))
	  combined)
     (length generation))))

(define (solve-problem n
		       generation-size
		       max-generations
		       selection-func
		       crossover-func
		       parent-ratio
		       mutation-chance)
  "Attempts to solve n-queens. Gives up after max-generations is exceeded."
  (let solve ((generation (make-generation generation-size n))
	      (generations 1))
    (let ((winner (get-winner generation)))
      (cond
       (winner
	(cons winner generations))
       ((= generations max-generations)
	#f)
       (else
	(solve (run-generation generation
			       selection-func
			       crossover-func
			       parent-ratio
			       mutation-chance)
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

(export solve-problem)
