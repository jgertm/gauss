module Examples where

import           Dev

foo, bar, baz, quux, fuzz, muk, flub :: Expression
foo = #x + 0
bar = -(- #x)
baz = #x + (- #x)
quux = #x * (1/ #x)
fuzz = #x * (1 * (1/ #x))
muk = (2 + 2)
flub = (#x + #y) - #y

fooR, barR, bazR, quuxR, fuzzR, mukR, flubR :: Expression
fooR = #x
barR = #x
bazR = 0
quuxR = 1
fuzzR = 1
mukR = 4
flubR = #x
