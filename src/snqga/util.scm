(define-module (snqga util))

(use-modules (srfi srfi-1)
	     (srfi srfi-19))

(define (alist-get alist elem)
  "Gets an alist entry. An alist is a list of (key . val) pairs."
  (let ((result (find (λ (pair)
			(equal? elem (car pair)))
		      alist)))
    (if result
	(cdr result)
	#f)))

(define (time-str time)
  (format #f "~ss, ~sns" (time-second time)
	     	   	 (time-nanosecond time)))

(define (n-triangle n)
  "Triangle number calculation."
  (/ (+ (* n n) n) 2))

(define (choose-normalised normalised-list)
  "Chooses a random element from a sorted normalised weighted list"
  (let ((r (random:uniform)))
    (let select-individual ((rest normalised-list)
			    (total (caar normalised-list)))
      (if (> total r)
	  (cdar rest)
	  (select-individual (cdr rest)
			     (+ total (caadr normalised-list)))))))

(export alist-get time-str n-triangle choose-normalised)
