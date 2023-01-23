.PHONY : sdist clean

language : src/Grammar/Test
	cabal install --installdir=. --overwrite-policy=always

src/Grammar/Test.hs src/Grammar/Lex.x src/Grammar/Par.y : Grammar.cf
	bnfc -o src -d $<

src/Grammar/Par.hs : src/Grammar/Par.y
	happy --ghc --coerce --array --info $<

src/Grammar/Lex.hs : src/Grammar/Lex.x
	alex --ghc $<

src/Grammar/%.y : Grammar.cf
	bnfc -o src -d $<

src/Grammar/Test : src/Grammar/Test.hs src/Grammar/Par.hs src/Grammar/Lex.hs
	ghc src/Grammar/Test.hs src/Grammar/Par.hs src/Grammar/Lex.hs src/Grammar/Abs.hs src/Grammar/Skel.hs src/Grammar/Print.hs -o src/Grammar/test

clean :
	rm -r src/Grammar
	rm language

# EOF
