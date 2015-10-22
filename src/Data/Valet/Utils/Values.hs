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
      text, int, double, char, bool,

      -- * Monads
      -- | Generic base for types which are instances of Monads.
      maybeValue, listValue,

      check, isNoLongerThan, isLongerThan
    ) where

import Data.Valet

import qualified Data.Valet.Utils.Reader as R
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- GENERIC
--------------------------------------------------------------------------------

-- | Value for which the "reader" is an instance of the 'Readable' class.
readable :: R.Readable b a => a -> T.Text -> Value r m b a
readable val name = reader R.read $ value name val

--------------------------------------------------------------------------------
-- BASICS
--------------------------------------------------------------------------------

-- | Text value with default value set to @""@.
text :: R.Readable b T.Text => T.Text -> Value r m b T.Text
text = readable ""

-- | Integer value with default value set to 0.
int :: R.Readable b Int => T.Text -> Value r m b Int
int = readable 0

-- | Double value with default value set to 0.
double :: R.Readable b Double => T.Text -> Value r m b Double
double = readable 0

-- | Bool value with default value set to @False@.
bool :: R.Readable b Bool => T.Text -> Value r m b Bool
bool = readable False

-- | Char value with default value set to @' '@.
char :: R.Readable b Char => T.Text -> Value r m b Char
char = readable ' '

-- | Optional value with default value set to @Nothing@.
maybeValue :: R.Readable b (Maybe a) => T.Text -> Value r m b (Maybe a)
maybeValue = readable Nothing

-- | List value with default value set to the empty list (@[]@).
listValue :: R.Readable b [a] => T.Text -> Value r m b [a]
listValue = readable []

--------------------------------------------------------------------------------
-- CHECKS
--------------------------------------------------------------------------------

{-|
Build a boolean transformer.

If the boolean function returns 'True', 'Success' is returned, otherwise
a 'Failure' is returned with the provided error.
-}
check :: Monad m => (a -> Bool) -> r -> Maybe r -> Mapper r m a
check g e r = Mapper (\x -> return $ if g x then Success x else Failure e) r

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
