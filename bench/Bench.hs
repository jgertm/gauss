import           Dev
import           Examples

import           Criterion.Main
import           GHC.IO.Encoding

main = do
  setLocaleEncoding utf8
  defaultMain [bgroup "reduce" $ map benchReduce [foo, bar, baz, quux, fuzz, muk, flub]]

benchReduce expr = bench (show expr) $ whnf reduce expr
