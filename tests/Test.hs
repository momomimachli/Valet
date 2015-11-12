module Main where

import Test.Framework (defaultMain)

import Data.Valet.Tests

-- | Run the tests.
main :: IO()
main = defaultMain
    [ Data.Valet.Tests.tests
    ]
