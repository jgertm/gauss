module Examples where

import           Dev

import Universum

foo, bar, baz, quux, fuzz, muk, flub, bort, polly :: Expression
foo = #x + 0
bar = -(- #x)
baz = #x + (- #x)
quux = #x * (1/ #x)
fuzz = #x * (1 * (1/ #x))
muk = (2 + 2)
flub = (#x + #y) - #y
bort = #a * (#b + (1 / #a))
polly = (#x + 1) * (#x - 1)

fooR, barR, bazR, quuxR, fuzzR, mukR, flubR, bortR, pollyR :: Expression
fooR = #x
barR = #x
bazR = 0
quuxR = 1
fuzzR = 1
mukR = 4
flubR = #x
bortR = (#a * #b) + 1
pollyR = (#x ** 2) - 1
