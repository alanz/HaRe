#!/bin/sh

# This script is temporary until ghc-mod is fixed to reconfigure on change

# cabal clean && cabal configure  --enable-tests --disable-optimization

# # Make sure the tests run against the newly installed environment
# (cd test/testdata && cabal clean && cabal configure)

# (cd test/testdata/cabal/cabal1 && cabal clean && cabal configure)
# (cd test/testdata/cabal/cabal2 && cabal clean && cabal configure)
# (cd test/testdata/cabal/cabal3 && cabal clean && cabal configure)
# (cd test/testdata/cabal/cabal4 && cabal clean && cabal configure)
# (cd test/testdata/cabal/foo    && cabal clean && cabal configure)

rm .ghc.environment.*
rm cabal.project.local
# rm -fr dist*



# cabal-2.2 new-configure --with-compiler=/opt/ghc/8.4.0.20180204/bin/ghc --allow-newer
# cabal new-configure --enable-tests --with-compiler=/opt/ghc/8.6.0.20180627/bin/ghc --allow-newer
# cabal new-configure --enable-tests --with-compiler=/opt/ghc/8.6.0.20180712/bin/ghc --allow-newer
# cabal-2.4 new-configure --enable-tests --with-compiler=/opt/ghc/8.6.0.20180810/bin/ghc --allow-newer
# cabal new-configure --enable-tests --with-compiler=/opt/ghc/8.6.0.20180810/bin/ghc --allow-newer

# cabal-2.4 new-configure  --with-compiler=/opt/ghc/8.6.0.20180810/bin/ghc --allow-newer

# cabal new-configure  --with-compiler=ghc-8.6.1 --allow-newer
# cabal new-configure  --with-compiler=ghc-8.6.1

# cabal new-configure --with-compiler=ghc-8.6.1 --enable-tests
# cabal new-configure --with-compiler=/opt/ghc/8.8.20190419/bin/ghc --enable-tests --allow-newer
# cabal new-configure --with-compiler=/opt/ghc/8.8.20190419/bin/ghc  --allow-newer


# cabal new-configure --with-compiler=/opt/ghc/8.8.0.20190424/bin/ghc --allow-newer --constraint=primitive==0.6.4.0
# cabal new-configure --with-compiler=/opt/ghc/8.8.0.20190424/bin/ghc --allow-newer --enable-tests
# cabal new-configure --with-compiler=/opt/ghc/8.8.0.20190424/bin/ghc

# cabal new-configure  --with-compiler=ghc-8.6.4 --allow-newer

# cabal new-configure --with-compiler=/opt/ghc/8.8.0.20190613/bin/ghc --allow-newer --enable-tests

cabal new-configure  --with-compiler=ghc-8.6.5
