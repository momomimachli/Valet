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
    ( Vame(..)
    , Errors
    ) where

import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Text       as T

{-|
View, analysis, modifications and error renderer.

This renderer is an empty container which can then be filled in with concrete
implementation of each of the components.
-}
data Vame v a m e b = Vame
    { _view     :: v
    , _analysis :: a
    , _modif    :: m
    , _error    :: e
    , _value    :: b
    }

instance (Monoid v, Monoid a, Monoid m, Monoid e, Monoid b) =>
    Monoid (Vame v a m e b) where

    mempty = Vame mempty mempty mempty mempty mempty

    mappend (Vame v1 a1 m1 e1 x1) (Vame v2 a2 m2 e2 x2) =
        Vame (v1 <> v2) (a1 <> a2) (m1 <> m2) (e1 <> e2) (x1 <> x2)

{-|
Simple renderer for errors as a strict Map.

All the errors of a given 'Value' will be reported under the same
key of the Map.
-}
type Errors = M.Map T.Text [T.Text]
