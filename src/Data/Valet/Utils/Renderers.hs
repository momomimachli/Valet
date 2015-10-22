{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Data.Valet.Utils.Renderers
Description : Some pre-built renderers for Valet.
Copyright   : (c) Leonard Monnier, 2015
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable
-}
module Data.Valet.Utils.Renderers
    ( Vte(..)
    , Errors
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Text       as T

{-|
View, transformer and error renderer.

This renderer is an empty container which can then be filled in with concrete
implementation of each of the components.
-}
data Vte v t e = Vte
    { _view        :: Maybe v
    , _transformer :: Maybe t
    , _error       :: Maybe e
    }

instance (Monoid v, Monoid t, Monoid e) => Monoid (Vte v t e) where
    mempty = Vte Nothing Nothing Nothing

    mappend (Vte v1 t1 e1) (Vte v2 t2 e2) =
        Vte (mappend v1 v2) (mappend t1 t2) (mappend e1 e2)

{-|
Simple renderer for errors as a strict Map.

All the errors of a given 'Value' will be reported under the same
key of the Map.
-}
type Errors = M.Map T.Text [T.Text]

instance Monoid Errors where
    mempty = M.empty

    mappend = M.unionWith (++)
