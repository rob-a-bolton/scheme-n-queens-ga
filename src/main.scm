#!/usr/bin/guile \
-e main -s
!#

(add-to-load-path (dirname (current-filename)))
(use-modules (ice-9 getopt-long)
	     (srfi srfi-19)
             (snqga genetics)
	     (snqga chromosomes)
	     (snqga selection)
	     (snqga crossover)
	     (snqga util))

(define version '(0 0 1))

(define program-name "snqga")

(define help-str
  (string-join
    `(,(format #f "Usage: ~a [OPTS]" program-name)
       ""
       " Where OPTS are one or more of:"
       "  -v, --version                     Prints program version."
       "  -h, --help                        Prints this help."
       "  -L, --list-selections             List available selection functions."
       "  -C, --list-crossovers		    List available crossover functions."
       "  -n, --n-queens           n        Problem size (number of queens). [REQUIRED]"
       "  -g, --generation-size    [n 10]   Number of chromosomes in generation."
       "  -m, --max-generations    n        Maximum number of generations to try."
       "  -p, --parent-ratio	   [n 0.4]  Ratio of chromosomes to become parents."
       "  -M, --mutation-chance    [n 0.1]  Chance of mutation per chromosome."
       "  -s, --selection-function name	    Selection function to use."
       "  -c, --crossover-function name	    Crossover function to use."
       ""
       " The following selection method-specific options are available:"
       "  --tournament-size [n 2] Sets the number of chromosomes considered in"
       "                          a tournament")
     "\n"))

(define (print-error msg)
  (let ((port (current-error-port)))
    (display msg port)
    (newline port)))

(define (print-version)
  (print-error (format #f "~a v~a.~a.~a  Written by Robert Bolton <rab26@aber.ac.uk>"
                          program-name
                          (car version)
                          (cadr version)
                          (caddr version))))

(define (print-help)
  (print-error help-str))

(define (exit-with-error msg)
  (print-error msg)
  (exit 1))

(define (maybe-string->number n)
  (if n (string->number n) #f))

(define (main args)
  (let* ((opts-spec
           '((version (single-char #\v) (value #f))
             (help (single-char #\h) (value #f))
	     (list-selections (single-char #\L) (value #f))
	     (list-crossovers (single-char #\C) (value #f))
             (n-queens (single-char #\n) (value #t))
	     (generation-size (single-char #\g) (value #t))
	     (max-generations (single-char #\m) (value #t))
	     (parent-ratio (single-char #\p) (value #t))
	     (mutation-chance (single-char #\M) (value #t))
	     (selection-function (single-char #\s) (value #t))
	     (crossover-function (single-char #\c) (value #t))
	     (tournament-size (value #t))))
         (opts (getopt-long args opts-spec))
         (version-wanted?
	   (option-ref opts 'version #f))
         (help-wanted?
	   (option-ref opts 'help #f))
	 (list-selections?
	   (option-ref opts 'list-selections #f))
	 (list-crossovers?
	   (option-ref opts 'list-crossovers #f))
         (n-queens
	   (maybe-string->number (option-ref opts 'n-queens #f)))
	 (generation-size
	   (string->number (option-ref opts 'generation-size "20")))
	 (max-generations
	   (maybe-string->number (option-ref opts 'max-generations #f)))
	 (parent-ratio
	   (string->number (option-ref opts 'parent-ratio "0.4")))
	 (mutation-chance
	   (string->number (option-ref opts 'mutation-chance "0.1")))
	 (selection-function
	   (string->symbol (option-ref opts 'selection-function "roulette")))
	 (crossover-function
	   (string->symbol (option-ref opts 'crossover-function "single")))
	 (tournament-size
	   (string->number (option-ref opts 'tournament-size "2")))
	 (selection-config `((tournament-size . ,tournament-size))))
    (cond
      ((or version-wanted? help-wanted?)
        (begin
          (when version-wanted? (print-version))
          (when help-wanted? (print-help))))
      ((or list-selections? list-crossovers?)
        (begin
	  (when list-selections?
		(display "Selection functions:\n")
		(map (λ (pair)
		       (display (car pair))
		       (newline))
		     selection-funcs))
	  (when list-crossovers?
		(display "Crossover functions:\n")
		(map (λ (pair)
		       (display (car pair))
		       (newline))
		     crossover-funcs))))
      ((or (not n-queens) (< n-queens 1))
          (exit-with-error "N-queens must be supplied and greater than 0."))
      (else
       (set! *random-state* (random-state-from-platform))
       (let* ((start-time (current-time time-monotonic))
	      (solution (solve-problem n-queens
				       generation-size
				       max-generations
				       (generate-selector selection-function
							  selection-config)
				       (get-crossover-func crossover-function)
				       parent-ratio
				       mutation-chance))
	      (end-time (current-time time-monotonic))
	      (time-taken (time-difference end-time start-time)))
	 (if solution
	     (begin
	       (format #t "~a, ~a generations:~%~a~%"
		       (time-str time-taken)
		       (cdr solution)
		       (chromosome->board (cdar solution)))
	       (newline))
	     (exit-with-error "Could not find a solution.")))))))

