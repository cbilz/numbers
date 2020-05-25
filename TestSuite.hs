module Main
where

import Test.Framework (
  Test,
  defaultMain,
  )

import Test.Data.Number.BigFloat (bigfloat_properties)
import Test.Data.Number.CReal (creal_properties)

main :: IO ()
main = defaultMain tests

tests :: [Test.Framework.Test]
tests = [ bigfloat_properties, creal_properties ]
