#!/usr/bin/guile \
-e main -s
!#

(add-to-load-path (dirname (current-filename)))
(use-modules (ice-9 getopt-long)
             (snqga genetics))

(define version '(0 0 1))

(define program-name "snqga")

(define help-str
  (string-join
    `(,(format #f "Usage: ~a [OPTS]" program-name)
       ""
       " Where OPTS are one or more of:"
       "  -v, --version        Prints program version."
       "  -h, --help           Prints this help."
       "  -n, --n-queens   n   Problem size (number of queens). [REQUIRED]")
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
             (n-queens (single-char #\n) (value #t))))
         (opts (getopt-long args opts-spec))
         (version-wanted? (option-ref opts 'version #f))
         (help-wanted? (option-ref opts 'help #f))
         (n-queens (maybe-string->number (option-ref opts 'n-queens #f))))
    (cond
      ((or version-wanted? help-wanted?)
        (begin
          (when version-wanted? (print-version))
          (when help-wanted? (print-help))))
      ((or (not n-queens) (< n-queens 1))
          (exit-with-error "N-queens must be supplied and greater than 0."))
      (else
        (exit-with-error "Not yet implemented")))))

