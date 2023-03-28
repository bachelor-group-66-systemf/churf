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

debug FILE:
    cabal run language -- -d {{FILE}}

hm FILE:
    cabal run language -- -t hm {{FILE}}

bi FILE:
    cabal run language -- -t bi {{FILE}}

hmd FILE:
    cabal run language -- -d -t hm {{FILE}}

bid FILE:
    cabal run language -- -d -t bi {{FILE}}