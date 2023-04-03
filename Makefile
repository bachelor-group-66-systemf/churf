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

Grammar.tex : 
	bnfc --latex Grammar.cf

Grammar.pdf : Grammar.tex
	pdflatex Grammar.tex
	rm Grammar.aux Grammar.log

pdf : Grammar.pdf

clean :
	rm -r src/Grammar
	rm language
	rm -rf dist-newstyles
	rm Grammar.aux Grammar.fdb_latexmk Grammar.fls Grammar.log Grammar.synctex.gz Grammar.tex

test :
	cabal v2-test

# EOF
