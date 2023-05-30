# build from scratch
build:
    bnfc -o src -d Grammar.cf
    cabal install --installdir=. --overwrite-policy=always

# clean the generated directories
clean:
    rm -r src/Grammar
    rm churf
    rm -r dist-newstyle/

# run all tests
test:
    cabal test

debug FILE:
    cabal run churf -- -d {{FILE}}

hm FILE:
    cabal run churf -- -t hm {{FILE}}

bi FILE:
    cabal run churf -- -t bi {{FILE}}

hml FILE:
    cabal run churf -- -l -t hm {{FILE}}

bil FILE:
    cabal run churf -- -l -t bi {{FILE}}

hmd FILE:
    cabal run churf -- -d -t hm {{FILE}}

bid FILE:
    cabal run churf -- -d -t bi {{FILE}}

hmdm FILE:
    cabal run churf -- -d -t hm -m {{FILE}}

bidm FILE:
    cabal run churf -- -d -t bi -m {{FILE}}

hmp FILE:
    cabal run churf -- -t hm -p {{FILE}}

bip FILE:
    cabal run churf -- -t bi -p {{FILE}}

hmdp FILE:
    cabal run churf -- -t hm -d -p {{FILE}}

bidp FILE:
    cabal run churf -- -t bi -d -p {{FILE}}

quicksort:
    cabal run churf -- -t bi sample-programs/working/quicksort.crf

lc:
    cabal run churf -- -t bi sample-programs/working/lambda_calculus-2.crf
