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
    ( Mvce(..)
    , Errors
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Text       as T

{-|
Model, view, controller and error renderer.

This renderer is an empty container which can then be filled in with concrete
implementation of each of the components.
-}
data Mvce m v c e = Mvce
    { _model      :: Maybe m
    , _view       :: Maybe v
    , _controller :: Maybe c
    , _errors     :: Maybe e
    }

instance (Monoid m, Monoid v, Monoid c, Monoid e) => Monoid (Mvce m v c e) where
    mempty = Mvce Nothing Nothing Nothing Nothing

    mappend (Mvce m1 v1 c1 e1) (Mvce m2 v2 c2 e2) =
        Mvce (mappend m1 m2) (mappend v1 v2) (mappend c1 c2) (mappend e1 e2)

{-|
Simple renderer for errors as a strict Map.

All the errors of a given 'Value' will be reported under the same
key of the Map.
-}
type Errors = M.Map T.Text [T.Text]

instance Monoid Errors where
    mempty = M.empty

    mappend = M.unionWith (++)
