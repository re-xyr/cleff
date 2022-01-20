stack upload . --candidate
cabal v2-haddock --haddock-for-hackage
cabal upload -d ./dist-newstyle/cleff-0.1.0.0-docs.tar.gz
