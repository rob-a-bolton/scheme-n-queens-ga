(define-module (snqga crossover))

(use-modules (snqga chromosomes)
	     (snqga util)
	     (srfi srfi-1)
	     (srfi srfi-11))

(define (map-crossover chromosomes crossover-func)
  "Crosses the supplied chromosomes using the given function. Invalids are
   substituted with their parents."
  (let cross-cdr ((processed '())
		  (remaining chromosomes))
    (cond
     ((null? remaining)
      processed)
     ((null? (cdr remaining))
      (cons (car remaining) processed))
     (else
      (let-values (((child-1 child-2) (crossover-func (car remaining) (cadr remaining))))
	(cross-cdr (if (or (not (is-valid? child-1))
			   (not (is-valid? child-2)))
		       processed
		       (cons child-1 (cons child-2 processed)))
		   (cddr remaining)))))))

(define (co-single c1 c2)
  "Combines two chromosomes by splitting them both in two and swapping halves."
  (let* ((position (random (1- (length c1))))
	 (g1 (append (list-head c1 position)
		     (list-tail c2 position)))
	 (g2 (append (list-head c2 position)
		     (list-tail c1 position))))
    (values g1 g2)))

(define (co-two-point c1 c2)
  "Splits both the chromosomes in three pieces, and swaps their middles."
  (let* ((start (random (1- (length c1))))
	 (middle (random (- (length c1) start)))
	 (end (+ middle start)))
    (values (append (list-head c1 start)
		    (list-head (list-tail c2 start) middle)
		    (list-tail c1 end))
	    (append (list-head c2 start)
		    (list-head (list-tail c1 start) middle)
		    (list-tail c2 end)))))

(define crossover-funcs
  `((single . ,co-single)
    (two-point . ,co-two-point)))

(define (get-crossover-func name)
  "Looks up and returns a crossover function by name"
  (alist-get crossover-funcs name))

(export map-crossover crossover-funcs get-crossover-func)
