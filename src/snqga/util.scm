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

(export alist-get time-str)
