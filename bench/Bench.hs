import           Dev
import           Examples

import           Criterion.Main
import           GHC.IO.Encoding
import           Universum       hiding (reduce)

main = do
  setLocaleEncoding utf8
  defaultMain
    [ bgroup "reduce" $
      map benchReduce [foo, bar, baz, quux, fuzz, muk, flub, bort, polly]
    ]

benchReduce expr = bench (show expr) $ whnf reduce expr
