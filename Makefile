# Basic makefile for building the project.
# Stack does most of the work, this just deals
# with passing stack various options

vanguard:
	stack build --ghc-options="-threaded -O2"

profiled:
	stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts -threaded -O2"

clean:
	stack clean
