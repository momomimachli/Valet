{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Data.Valet.Tests
Description : Tests for the Valet module.
Copyright   : (c) Leonard Monnier, 2015
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Tests for the Valet module.
-}
module Data.Valet.Tests
    ( tests
    ) where

import Data.Valet

import Control.Lens
import Data.Monoid()
import Prelude hiding (lookup)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test)

import qualified Data.Text as T

-- | Gather all tests.
tests :: Test
tests = testGroup "Data.Valet"
    [ testLaw1
    , testTopKey
    ]

--------------------------------------------------------------------------------
-- INITIALISATION
--------------------------------------------------------------------------------

data Person = Person
    { _firstName :: T.Text
    }

firstName :: Valet T.Text Maybe T.Text
firstName = valet ""
    & key .~ "firstName"

person :: Valet T.Text Maybe Person
person = Person <$> firstName

--------------------------------------------------------------------------------
-- LENS LAW 1: You get back what you put in
--------------------------------------------------------------------------------

testLaw1 :: Test
testLaw1 = testGroup "Lens Law 1"
    [ keyLensLaw1
    , showValueFuncLensLaw1
    ]

-- | Lens law one for 'key'.
keyLensLaw1 :: Test
keyLensLaw1 = testCase "Lens law 1 for key"
    $ assertEqual
      "Lens law 1 does not hold for key"
      "test"
      (person' ^. key)
    where
        person' = person & key .~ "test"

-- | Lens law one for 'showValueFunc'.
showValueFuncLensLaw1 :: Test
showValueFuncLensLaw1 = testCase "Lens law 1 for showValueFunc"
    $ assertEqual
      "Lens law 1 does not hold for showValueFunc"
      (valueFunc julius)
      ((person' ^. showValueFunc) julius)
    where
        julius = Person "Julius"
        valueFunc p = _firstName p
        person' = person & showValueFunc .~ valueFunc

--------------------------------------------------------------------------------
-- Key or no key set at the top level
--------------------------------------------------------------------------------

testTopKey :: Test
testTopKey = testGroup "Key at top level"
    [ renderSubValetKeySet
    ]

{-|
Test that a sub-valet is rendered the same way regardless if the key of the
parent valet is set or not.
-}
renderSubValetKeySet :: Test
renderSubValetKeySet = testCase "Key has no impact on render"
    $ assertEqual
       "Key at top level has an impact on the render"
       (person' ^. lookup "firstName" . _Just . render)
       (person ^. lookup "firstName" . _Just . render)
    where
        person' = person & key .~ "HomePage"
