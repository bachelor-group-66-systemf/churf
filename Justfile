# build from scratch
build:
    bnfc -o src -d Grammar.cf
    cabal install --installdir=. --overwrite-policy=always

# clean the generated directories
clean:
    rm -r src/Grammar
    rm language
    rm -r dist-newstyle/

# run all tests
test:
    cabal test

# compile a specific file
run FILE:
    cabal run language {{FILE}}
