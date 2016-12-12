snqga
=====
(scheme n-queens genetic algorithm)

This program solves the n-queens problem using a genetic algorithm.

Run the program as either *./main.scm args* or *guile main.scm -e main -s args*

Usage
-----

Usage: snqga [OPTS]

Where OPTS are one or more of:
 -v, --version                   Prints program version.
 -h, --help                      Prints this help.
 -L, --list-selections           List available selection functions.
 -C, --list-crossovers           List available crossover functions.
 -n, --n-queens n                Problem size (number of queens). [REQUIRED]
 -g, --generation-size [n 10]    Number of chromosomes in generation.
 -m, --max-generations n         Maximum number of generations to try.
 -p, --parent-ratio [n 0.4]      Ratio of chromosomes to become parents.
 -M, --mutation-chance [n 0.1]   Chance of mutation per chromosome.
 -s, --selection-function name	 Selection function to use.
 -c, --crossover-function name	 Crossover function to use.
 -P, --parsable                  Outputs one line only.

The following selection method-specific options are available:
 --tournament-size [n 2]         Sets the number of chromosomes considered in
                                 a tournament
