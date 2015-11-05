{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}

{-|
Module      : Data.Valet.Utils.Values
Description : Collection of present values.
Copyright   : (c) Leonard Monnier, 2015
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Collection of values preset with a default reader and type of "data".

The used reader is the function 'read' of the 'Readable' class
(of this same package). It allows to convert any type which is an
instance of this class to the type of the 'Value'.
-}
module Data.Valet.Utils.Values
    (
      -- * Generic
      -- | Generic base which can be used to define more specialised values.
      readable,

      -- * Basics
      -- | Basic value types.
      text, int, integer, double, char, bool,

      -- * Monads
      -- | Generic base for types which are instances of Monads.
      maybeValet, listValet,

      -- * Analysis
      -- Basic analysis
      isNoLongerThan, isLongerThan
    ) where

import Control.Lens
import Data.Valet

import qualified Data.Valet.Utils.Reader as R
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- GENERIC
--------------------------------------------------------------------------------

-- | Value for which the "renderer" is an instance of the 'Readable' class.
readable :: (Monoid r, Monad m, R.Readable r a) => a -> T.Text -> Valet r m a
readable val k = valet val
    & reader .~ R.read
    & key    .~ k

--------------------------------------------------------------------------------
-- BASICS
--------------------------------------------------------------------------------

-- | Text value with default value set to @""@.
text :: (Monoid r , Monad m, R.Readable r T.Text) => T.Text -> Valet r m T.Text
text = readable ""

-- | Fixed precision integer 'Valet' with default value set to 0.
int :: (Monoid r , Monad m, R.Readable r Int) => T.Text -> Valet r m Int
int = readable 0

-- | Arbitrary precision integer 'Valet' with default value set to 0.
integer ::
       (Monoid r , Monad m, R.Readable r Integer)
    => T.Text
    -> Valet r m Integer
integer = readable 0

-- | Double value with default value set to 0.
double :: (Monoid r , Monad m, R.Readable r Double) => T.Text -> Valet r m Double
double = readable 0

-- | Bool value with default value set to @False@.
bool :: (Monoid r , Monad m, R.Readable r Bool) => T.Text -> Valet r m Bool
bool = readable False

-- | Char value with default value set to @' '@.
char :: (Monoid r , Monad m, R.Readable r Char) => T.Text -> Valet r m Char
char = readable ' '

-- | Optional value with default value set to @Nothing@.
maybeValet ::
       (Monoid r , Monad m, R.Readable r (Maybe a))
    => T.Text
    -> Valet r m (Maybe a)
maybeValet = readable Nothing

-- | List value with default value set to the empty list (@[]@).
listValet :: (Monoid r , Monad m, R.Readable r [a]) => T.Text -> Valet r m [a]
listValet = readable []

--------------------------------------------------------------------------------
-- Analysis
--------------------------------------------------------------------------------

{-|
Maximal length for a text.

The boundary is inclusive (it has to be seen as a "max length" attribute).
-}
isNoLongerThan :: Int -> T.Text -> Bool
isNoLongerThan max x = T.length x <= max

{-|
Minimal length for a text.

The boundary is inclusive (is has to be seen as a "min length" attribute).
-}
isLongerThan :: Int -> T.Text -> Bool
isLongerThan min x = T.length x >= min
