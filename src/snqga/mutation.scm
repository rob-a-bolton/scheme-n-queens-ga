(define-module (snqga mutation))

(define (mutate-gene gene n)
  "Mutates either the X or Y of a gene."
  (if (< (random:uniform) 0.5)
      (cons (random n) (cdr gene))
      (cons (car gene) (random n))))

(define (mutate chromosome)
  "Mutates a chromosome."
  (let* ((index (random (length chromosome)))
	 (gene (list-ref chromosome index))
	 (new-gene (mutate-gene gene (length chromosome))))
    (if (member new-gene chromosome)
	(mutate chromosome)
	(append (list-head chromosome index)
		(list new-gene)
		(list-tail chromosome (1+ index))))))

(export mutate mutate-gene)
