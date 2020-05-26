module Test.Data.Number.CReal (creal_properties) where

import Data.Number.CReal
import Numeric.IEEE

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

implies x y = y || not x
dblEq x y = x == y || x == succIEEE y || x == predIEEE y

prop_creal_some_algebraic_equalities :: Bool
prop_creal_some_algebraic_equalities = all (uncurry (appRel 1000 (==)))
  ([ (sqrt 8, 2 * sqrt 2)
   , (1/(sqrt 2 - sqrt 3 + sqrt 5), (sqrt 2)/4 - (sqrt 3)/6 + (sqrt 30)/12)
   , (1 - 1, 0)
   , (2^100, sqrt(2^200))
   ] :: [(CReal, CReal)])

prop_creal_some_inequalities :: Bool
prop_creal_some_inequalities = all (uncurry (<))
  ([ (1, 1 + 2^^(-500))
   , (-1 - pi^^(-110), -1)
   , (-2^^(-500), 0)
   ] :: [(CReal, CReal)])

prop_creal_rational_toDouble_equality :: Rational -> Bool
prop_creal_rational_toDouble_equality x =
  fromRational x `dblEq` toDouble (fromRational x)

creal_properties :: Test.Framework.Test
creal_properties =
  testGroup "CReal Properties" [
    testProperty
      "creal/check some algebraic identities"
      prop_creal_some_algebraic_equalities,

    testProperty
      "creal/check some difficult inequalities"
      prop_creal_some_inequalities,

    testProperty
      "creal/convert Rational to Double via CReal"
      $ withMaxSuccess 10000 prop_creal_rational_toDouble_equality
  ]
