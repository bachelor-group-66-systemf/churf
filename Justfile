build:
    bnfc -o src -d Grammar.cf

# clean the generated directories
clean:
    rm -r src/Grammar
    rm language
    rm -r dist-newstyle/

# run all tests
test:
    cabal test

ctest:
    cabal run language sample-programs/basic-1
    cabal run language sample-programs/basic-2
    cabal run language sample-programs/basic-3
    cabal run language sample-programs/basic-4
    cabal run language sample-programs/basic-5

# compile a specific file
run FILE:
    cabal run language {{FILE}}
