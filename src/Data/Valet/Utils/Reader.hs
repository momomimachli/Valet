{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : Data.Valet.Utils.Reader
Description : Generic class for the "reader".
Copyright   : (c) Leonard Monnier, 2015
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Generic 'read' function which can be used for any value.

All you need to ensure is 'Read' instance for the given value.
The 'String' value will then be converted to the type of the value.

Values can be then defined such as:

> readable :: R.Readable b T.Text => T.Text -> Value r m b T.Text
> readable name = reader R.read $ value name ""

The above example is a 'Text' value for which data can be set from
any 'Readable' instance.
-}
module Data.Valet.Utils.Reader
    ( Readable
    , Data.Valet.Utils.Reader.read
    ) where

import qualified Data.Text as T

{-|
Class defining a 'read' function.

This read function can then be used for all types being an instance of this
class.
-}
class Readable a b where
    read :: a -> b

instance Read b => Readable String b where
    read = Prelude.read

instance Read b => Readable T.Text b where
    read = Prelude.read . T.unpack
