build:
    bnfc -o src -d Grammar.cf

# clean the generated directories
clean:
    rm -r src/Grammar
    rm language

# run all tests
test:
    cabal test

# compile a specific file
run FILE:
    cabal run language {{FILE}}
