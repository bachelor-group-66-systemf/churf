.PHONY : sdist clean

language : src/Grammar/Test
	cabal install --installdir=. --overwrite-policy=always

src/Grammar/Test.hs src/Grammar/Lex.x src/Grammar/Par.y src/Grammar/Layout : Grammar.cf
	bnfc -o src -d $<

src/Grammar/Par.hs : src/Grammar/Par.y
	happy --ghc --coerce --array --info $<

src/Grammar/Lex.hs : src/Grammar/Lex.x
	alex --ghc $<

src/Grammar/%.y : Grammar.cf
	bnfc -o src -d $<

src/Grammar/Test : src/Grammar/Test.hs src/Grammar/Par.hs src/Grammar/Lex.hs src/Grammar/Layout
	ghc src/Grammar/Test.hs src/Grammar/Par.hs src/Grammar/Lex.hs src/Grammar/Abs.hs src/Grammar/Skel.hs src/Grammar/Print.hs src/Grammar/Layout -o src/Grammar/test

clean :
	rm -r src/Grammar
	rm language

test :
	./language ./sample-programs/basic-1
	./language ./sample-programs/basic-2
	./language ./sample-programs/basic-3
	./language ./sample-programs/basic-4
	./language ./sample-programs/basic-5
	./language ./sample-programs/basic-6
	./language ./sample-programs/basic-7
	./language ./sample-programs/basic-8

run :
	cabal -v0 new-run language -- "test_program"

# EOF
