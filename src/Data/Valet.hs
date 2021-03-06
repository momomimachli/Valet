{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Data.Valet
Description : Abstract Value which can be evaluated and rendered.
Copyright   : (c) Leonard Monnier, 2015
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

This module provides the main elements of the 'Valet' package.

The main aim of this library is to provide a more general implementation
of the digestive-functors package.

The main difference are:
- the view is part of the 'Valet' allowing to render validations;
- it is not mainly targeted toward HTML forms;
- a form created with this library is not necessarely a formlet.

It shares also some very close similarities such as the use of GADTs to define
the structure and its behaviour as 'Applicative' instance.

* Introduction

A 'Valet' can be seen as a value with additional properties. It may be:

- Rendered: displayed in another format than the one of the value itself.
- Set: from a value of another type.
- Modified: changing the value inside the valet.
- Analysed: leading to the production of a report.
- Composed: through the 'Applicative' type-class.

This library relies on lens to set, view or modify the properties of a given
valet. It allows the usage of a known and concise syntax.

* Organisation of the library

The library is organised with:
- Some types, among which the main one is the 'Valet' one.
- Lenses: allowing to set, get and modify the valet's properties.
- Getters: allowing to evaluate or render the valet.

* Creating a valet.

A 'Valet' contains a value of a given type
and is referenced by a key which must be unique.
Here is a basic 'Text' 'Valet' with an empty value:
> textValet :: Valet r m Text
> textValet = valet "myText" ""

The last type of the valet is now 'Text' which indicates that
its value is of type 'Text'.

A valet being an instance of Applicative, you can also use the pure function.
However, in such a case, you will have to later define a key if you wish
to use all the valet's functionnalities.

** The [TODO]

A valet can be set from a given type, matching the provided reader function.
In our case, the reader function is a function which converts the given type to
a 'Text'.
For example, if we want to be able to set a value from a 'String':
> textValet' :: Text -> Value String m Text
> textValet' = valet key data

It is now possible to modify the value of an existing valet:
> name = textValue "name" ""
> haskell = setValue "name" "Haskell"

** Modifications
A modification applies a function to a value
which returns a new value of the same type.

** Modifications and analysis considerations
Both topics are closely related.
An analysis can be seen as a filter using the 'id' function
with potential failure.
It could therefore be possible to approach both subjects with the same function.
However, the problems start when collecting failures.

It is also possible to add transformations to a value.
Tranformations can be of two kinds:
- the ones which just modify a value but never fail: the "filters";
- the ones which can fail: the "validators".

Transformations return a 'Result' which can be either the transformed value
in case of success or an error in case of failure.

To add a filter converting a text to upper case:
> upperTextValue :: Text -> Text -> Valet r m String Text
> upperTextValue key data =
>     transformer toUpper $ textValue key data
>     where
>         toUpper :: Text -> Result Text Text
>         toUpper = Success . upper
-}
module Data.Valet
    (
      -- * Types
      Valet
    , SomeValet
    , Analysis
    , RAnalysis
    , Modif
    , RModif
    , Result(..)
    , Coerce

      -- * Constructors
    , valet
    , someValet
    , analysis
    , modif
    , check
    , report

      -- * Lenses

      {-|
      Lenses are the mean to access, set or modify the different components
      of a valet. To know more about lenses, please refer to the
      'Control.Lens' library and the various tutorials which have been
      written about this library.
      -}
    , value
    , showValueFunc
    , key
    , analyser
    , rAnalyser
    , modifier
    , rModifier
    , reader
    , renderer

      -- ** Indexed lenses
    , keyi

      -- * Getters

      {-|
      Getters can be used as lenses but only to retrieve value, not
      to modify or set them.
      -}
    , eval
    , showValue
    , render
    , renderSubValets
    , keys

      -- ** Indexed getters

      {-|
      The key of the 'Valet' must be provided to these getters.
      -}
    , lookup
    ) where

import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad ((>=>))
import Data.Monoid
import Prelude hiding (lookup)

import qualified Data.Text as T

----------------------------------------
-- Types
----------------------------------------

{-|
The valet data-type.

* Meaning of the types

** @r@

Type in which the valet can be rendered.
This type has to be a 'Monoid' instance.

For example, the type @r@ could be a 'String'.
It would mean that a 'Valet' with, let's say, an 'Int' value could be
rendered as a 'String'.

For a webform, you could typically define a type HTML such as:
> type HTML = Text

Then, your valet would have type:
> Valet HTML m a

TODO: explain how composite types can be used.

** @m@

Monad in which modifications or analysis occur.

The most likely usage is if you need to analyse a value against the one of a
database (for example to determine if this value does not already exist in
that database.) In such case, the monad type would by 'IO' and your valet would
have type:
> Valet r IO a

The usage of a monad for modifications is less likely but could maybe occur
in some edge cases.

** @a@

Value contained in the valet.

If your valet contains a 'String' then its type would be:
> Valet r m String
-}
data Valet r m a where

    -- | Set a key to a valet.
    Key :: T.Text -> Valet r m a -> Valet r m a

    -- | Add a renderer to a valet.
    Render :: Rendered r m -> Valet r m a -> Valet r m a

    -- | Add a analyser to a valet.
    Analyse ::  Analysis r m a -> Valet r m a -> Valet r m a

    -- | Add a modifier to a valet.
    Modify :: Modif m a -> Valet r m a -> Valet r m a

    -- | Convert a renderer to a value.
    Read :: (r -> a) -> Valet r m a -> Valet r m a

    -- | Display a value as 'Text'.
    ShowValue :: (a -> T.Text) -> Valet r m a -> Valet r m a

    -- | Applicative transformation. This allows to combine valets in a given
    --   data-type in an applicative style.
    --   For example 'name' and 'age' being valets:
    --   > Person <$> name <*> age
    Apply :: Monoid r => Valet r m (c -> a) -> Valet r m c -> Valet r m a

    -- | Create a valet by setting its value.
    Value :: a -> Valet r m a

-- | Show instance for debugging purposes.
instance Show (Valet r m a) where
    show (Value _)     = "Value: x"
    show (ShowValue _ v) = "Show value: " ++ show v
    show (Read _ f)    = "Read g: " ++ show f
    show (Key x f)     = "Key " ++ show x ++ ": " ++ show f
    show (Render _ f)  = "Render r: " ++ show f
    show (Modify _ f)  = "Modify g " ++ show f
    show (Analyse _ f) = "Analyse g: " ++ show f
    show (Apply f1 f2) = "Apply: (" ++ show f1 ++ ", " ++ show f2 ++ ")"

instance (Monoid r) => Functor (Valet r m) where
    fmap g = Apply (Value g)

instance (Monoid r) => Applicative (Valet r m) where
    pure = Value
    (<*>) = Apply

type instance Index (Valet r m a) = T.Text
type instance IxValue (Valet r m a) = r

instance (Monad m, Monoid r) => Ixed (Valet r m a) where
    ix k f sv = case sv ^. lookup k of
        Just v  -> f (v ^. render) <&> \v' -> putValue k sv v'
        Nothing -> pure sv

{-|
Allows to use the 'at' lens to set or view values using their key.
-}
instance (Monad m, Monoid r) => At (Valet r m a) where
    at k f v = f vr <&> \r -> case r of
        Nothing -> v
        Just r' -> putValue k v r'
        where vr = fmap (view render) $ v ^. lookup k

{-|
Value agnostic valet.

Value agnostic valet are usefull when you wish to get information from
only a given sub-valet.
However, it is at some cost, as you then loose the possibility to retrieve
the value with its type information.

* Use
A typicall use of a value agnostic valet is when you wish to perform
a validation on a single sub-value using a key. It happens for example
with a webform when you wish to verify that a value is indeed unique on the
server side.

* Example
> validate :: (Monad m, Monoid r) => m (Maybe r)
> validate =
>     someValet (myValet & at "name" ?~ "Jean") ^. at "name" . _Just . report

The above example illustrates many concepts.

In the first part @(myValet & at "name" ?~ "Jean")@ the value of the valet is
modified using the lens 'at' and the '(?~)' combinator.
We therefore obtain a new 'Valet'. Then, this new valet is converted to
a value agnostic valet ('SomeValet') using the 'someValet' function.

From there, we need to retrieve the sub-valet and perform the analysis
of the value. The first task is accomplished thanks to the 'at' lens.
Since this operation may fail (there might be no sub-valet matching the
provided key) we need to combine it with the '_Just' prism.

Finally, we can perform a 'report' on the 'sub-valet' which will
return all the error messages or 'Nothing' if there are not any.
-}
data SomeValet r m where
    SomeValet :: (Monoid r, Monad m) => Valet r m a -> SomeValet r m

{-|
Coerce a value of a given type to a value of another type.

This class is a "trick" which is used to convert 'Valet' or
value agnostic valet ('SomeValet') to 'SomeValet'.
In the second case, the conversion is not actually one. It is
just the 'id' function.

Concretely, when you see @Coerce b (SomeValet r m)@ in a function
signature, you know that you can use indiferentely a 'Valet'
or 'SomeValet' for parameter @b@ (or even a type of your customed
instance if you wish to define one).
-}
class Coerce a b | a -> b where
    coerce :: a -> b

instance (Monoid r, Monad m) => Coerce (Valet r m a) (SomeValet r m) where
    coerce = SomeValet

instance Coerce (SomeValet r m) (SomeValet r m) where
    coerce = id

{-|
Type synonym for a function turning a value agnostic valet ('SomeValet')
to a rendered value @r@.

Having such type allow to the rendered value to access all the 'Valet's
parameters.
-}
type Rendered r m  = SomeValet r m -> r

{-|
* Conceptual meaning
An analysis encapsulates in a newtype a monadic function.
It is therefore a 'Kleisli'.

Since the 'Monoid' instance for this newtype
acts at the value level, @b@ must also be a monoid.

* Use by the valet library

In practice in this library the type @r@ is @Maybe (Rendered r)@ meaning that
from the value @a@ of the 'Valet' we might get a rendering @r@.

This type @r@ can be seen as the report of the analysis. It takes a value,
it analyses it and it produces a report (if there is something to report).

This rendering is actually not a value but a function which takes the 'Valet'
as parameter. That way, the rendering can access the data of the 'Valet'
such as its key.

Thanks to its 'Monoid' instance it's easy to combine various analysis into
one:
> analysis3 :: Analysis m a b
> analysis3 = analysis1 <> analysis2

The results of those different analyisis will be combined as a monoid.

* Possible use
A concrete example of an analysis is the validation of the value of a form.
The user submits a form, the value of a given field gets analysed and errors
get reported if any.

-- TODO: update doc.
-}
type Analysis r m a = KleisliMonoid m a (Maybe (Rendered r m))

newtype KleisliMonoid m a b = KleisliMonoid {runKleisliMonoid :: a -> m b}

{-|
* Conceptual meaning
A modification encapsulate in a newtype a monadif function which returns
a value of the same type as the initial value.

It is therefore a 'Kleisli'.

The 'Monoid' instance for this newtype
acts at the monad level @m@ through kleisli composition @(>=>)@.

* Use by the valet library
Thanks to its 'Monoid' instance different modifications acting on the same
type can be combined into one. Each modification will pass its result
to the next one:
> modif3 :: Modif m a
> modif3 = modif1 <> modif2

* Possible use
A concrete example of an modification is the filtering of value.
Imagine you want a last name in upper case, you could have filters to:
- remove all spaces before and after the name (trim);
- convert all characters to upper case.
-}
newtype Modif m a = Modif {runModif :: a -> m a}

{-|
For 'mappend', this instance returns a new 'KleisliMonoid' which function append
the results of both 'KleisliMonoid'' functions.
-}
instance (Monad m, Monoid b) => Monoid (KleisliMonoid m a b) where
    mempty = KleisliMonoid (\_ -> return mempty)
    mappend k1 k2 = KleisliMonoid g
        where
            g x = do
               r1 <- runKleisliMonoid k1 x
               r2 <- runKleisliMonoid k2 x
               return (r1 <> r2)

{-|
For 'mappend' this instance returns a new 'Modif' which function is
a monadic combination of both 'Modif'.
-}
instance Monad m => Monoid (Modif m a) where
    mempty = Modif (return . id)
    Modif g `mappend` Modif h = Modif (g >=> h)

{-|
A result which can denote a success by returning a data,
or a failure by returning a type which can be rendered.

The types are the following:
- @a@: data of the 'Valet';
- @r@: type used to display the error(s);
-}
data Monoid r => Result r a =
      Success a
    | Failure r
      deriving(Show)

instance Monoid r => Functor (Result r) where
    fmap g (Success x) = Success $ g x
    fmap _ (Failure e) = Failure e

instance Monoid r => Applicative (Result r) where
    pure = Success
    Success g  <*>  Success x  = Success $ g x
    Failure e  <*>  Success _  = Failure e
    Success _  <*>  Failure e  = Failure e
    Failure d  <*>  Failure e  = Failure (d <> e)

----------------------------------------
-- CONSTRUCTORS
----------------------------------------

{-|
Create a new valet.

This function is equivalent to 'pure'.
-}
valet ::
       Monoid r
    => a            -- ^ Initial value of the valet.
    -> Valet r m a
valet = pure

{-|
Convert a value – typically a 'Valet' or 'SomeValet' – to a value
agnostic valet ('SomeValet').
-}
someValet :: Coerce b (SomeValet r m) => b -> SomeValet r m
someValet = Data.Valet.coerce

{-|
Create an analysis from the provided function.
-}
analysis :: (a -> m (Maybe (Rendered r m))) -> Analysis r m a
analysis = KleisliMonoid

{-|
Create a modification from the provided function.
-}
modif :: (a -> m a) -> Modif m a
modif = Modif

{-|
Create an analysis from a monadic boolean function and a renderer.

The typical use case of this function is to create a validation where
the renderer will return an error.
For example, if we want to ensure that an integer is higher than a certain
value we can write:
> higherThan x =
>     check (\y -> return (y > x))
>     "The value must be higher than " <> read x <> "."

TODO: check the validity of the example.
-}
check :: Monad m => (a -> m Bool) -> Rendered r m -> Analysis r m a
check g e = analysis $ \x -> do
    predicate <- g x
    if predicate then return Nothing else return $ Just e

{-|
Create a result from a value and a report.

If the report is @Nothin@ and therefore does not contain any errors,
then a @Success@ containing the value is returned.
Otherwise, a @Failure@ containing the error of the report is returned.
-}
result :: Monoid r => a -> Maybe r -> Result r a
result x Nothing  = Success x
result _ (Just r) = Failure r

----------------------------------------
-- Setters
----------------------------------------

-- | Generic setter, which provides default behaviors for each patern match.
setter ::
       (Valet r m a -> c -> Valet r m a)
    -> Valet r m a
    -> c
    -> Valet r m a
setter _ (Value x) _       = Value x
setter h (ShowValue g v) x = ShowValue g (h v x)
setter h (Key k v) x       = Key k (h v x)
setter h (Read g v) x      = Read g (h v x)
setter h (Render g v) x    = Render g (h v x)
setter h (Modify g v) x    = Modify g (h v x)
setter h (Analyse g v) x   = Analyse g (h v x)
setter _ (Apply g v) _     = Apply g v

{-|
Set the value of the provided 'Valet'.

* Note
It cannot be used for applicative values.
By doing so, the unmodified valet will be returned.
-}
setValue :: Valet r m a -> a -> Valet r m a
setValue (Value _) x = Value x
setValue v x         = setter setValue v x

{-|
Set a function which converts the value of the 'Valet' to 'Text'.
-}
setShowValue :: Valet r m a -> (a -> T.Text) -> Valet r m a
setShowValue (Value x) g       = ShowValue g (Value x)
setShowValue (Apply h v) g     = ShowValue g (Apply h v)
setShowValue (ShowValue _ v) g = ShowValue g v
setShowValue v x               = setter setShowValue v x

{-|
Set the key of a the provided 'Valet'.

This key can then be used to retrieve the different components of the 'Valet'
and its sub-valets.
-}
setKey :: (Monoid r, Monad m) => Valet r m a -> T.Text -> Valet r m a
setKey v k
    -- We ensure that the name is at the top if defined.
    -- No name is created for an empty text value.
    | currentKey <> k == "" = v
    | currentKey      == "" = Key k v
    | otherwise             = setKey' v k
    where
        currentKey = v ^. key
        setKey' (Key _ v') k' = Key k' v'
        setKey' v' k'          = setter setKey' v' k'

{-|
Set the analyser of the provided 'Valet'.

* Note
This will replace the previously existing analyse function of the 'Valet'.
-}
setAnalyser :: Monad m => Valet r m a -> Analysis r m a -> Valet r m a
setAnalyser (Analyse _ v) c = Analyse c v
setAnalyser (Value x) c     = Analyse c (Value x)
setAnalyser (Apply g v) c   = Analyse c (Apply g v)
setAnalyser v c             = setter setAnalyser v c

{-|
Set the modifier of the provided 'Valet'.

* Note
This will replace the previously existing modify function of the 'Valet'.
-}
setModifier ::
       (Monad m, Monoid r)
    => Valet r m a
    -> Modif m a
    -> Valet r m a
setModifier (Modify _ v) m = Modify m v
setModifier (Value x) m    = Modify m (Value x)
setModifier (Apply g v) m  = Modify m (Apply g v)
setModifier v m            = setter setModifier v m

{-|
Provide a function which convert a given type to the one of the data.

This will then be used to set the data of a value by functions
such as 'setValue'.

* Note
It does not apply to applicative values.
In such case, the unmodified 'Valet' will be returned.
-}
setReader :: Valet r m a -> (r -> a) -> Valet r m a
setReader (Value x) g  = Read g (Value x)
setReader (Read _ v) g = Read g v
setReader v g          = setter setReader v g

{-|
Set the renderer of a value.
-}
setRenderer :: Valet r m a -> (SomeValet r m -> r) -> Valet r m a
setRenderer (Value x) g    = Render g (Value x)
setRenderer (Render _ v) g = Render g v
setRenderer (Apply h v) g  = Render g (Apply h v)
setRenderer v g            = setter setRenderer v g

----------------------------------------
-- Getters
----------------------------------------

-- | Generic getter for 'Valet'.
getter :: Valet r m a -> Valet r m a
getter (ShowValue _ v) = v
getter (Key _ v)       = v
getter (Read _ v)      = v
getter (Render _ v)    = v
getter (Modify _ v)    = v
getter (Analyse _ v)   = v
getter (Apply g v)     = g <*> v
getter (Value x)       = Value x

-- | Return the value of a 'Valet'.
getValue :: Valet r m a -> a
getValue (Value x)    = x
getValue (Apply g v)  = getValue g (getValue v)
getValue v            = getValue $ getter v

{-|
Retrieve the value converting the value of a 'Valet' to 'Text'.

If this function does not exist, return one which will always
return an empty 'Text' ("").
-}
getShowValue :: Valet r m a -> a -> T.Text
getShowValue (Value _)       = (\_ -> mempty)
getShowValue (Apply _ _)     = (\_ -> mempty)
getShowValue (ShowValue g _) = g
getShowValue v               = getShowValue $ getter v

-- | Return the analyser of a 'Valet'.
getAnalyser :: (Monoid r, Monad m) => Valet r m a -> Analysis r m a
getAnalyser (Value _)     = mempty
getAnalyser (Apply _ _)   = mempty
getAnalyser (Analyse g _) = g
getAnalyser v             = getAnalyser $ getter v

-- | Return the modifier of a 'Valet'.
getModifier :: Monad m => Valet r m a -> Modif m a
getModifier (Value _)    = mempty
getModifier (Apply _ _)  = mempty
getModifier (Modify g _) = g
getModifier v            = getModifier $ getter v

-- | Return the reader of the 'Valet'.
getReader :: Valet r m a -> r -> a
getReader (Value x)   = (\_ -> x)
getReader (Apply g v) = let x = getValue g (getValue v) in (\_ -> x)
getReader v           = getReader $ getter v

-- | Return the renderer of the 'Valet'.
getRenderer :: Monoid r => Valet r m a -> SomeValet r m -> r
getRenderer (Render r _) = r
getRenderer (Apply _ _)  = mempty
getRenderer (Value _)    = mempty
getRenderer v            = getRenderer $ getter v

{-|
Return a 'Result' which comes from the evaluation of the  valet
applying on it the modification and analysis which have been previously set.
-}
eval :: (Monad m, Monoid r) => Getter (Valet r m a) (m (Result r a))
eval = to $ \x -> case x of
    Value v     -> return $ Success v
    v@(Key _ _) -> do
                       val <- runModif (v ^. modifier) $ v ^. value
                       report' <- runKleisliMonoid (v ^. analyser) $ val
                       return $ result val (report' <*> pure (someValet v))
    Apply g v   -> do
                       r1 <- g ^. eval
                       r2 <- v ^. eval
                       return $ r1 <*> r2
    v           -> view eval $ getter v

-- | Generic getter for a value agnostic valet.
sGetter :: SomeValet r m -> SomeValet r m
sGetter (SomeValet (ShowValue _ v)) = SomeValet v
sGetter (SomeValet (Key _ v))       = SomeValet v
sGetter (SomeValet (Read _ v))      = SomeValet v
sGetter (SomeValet (Render _ v))    = SomeValet v
sGetter (SomeValet (Modify _ v))    = SomeValet v
sGetter (SomeValet (Analyse _ v))   = SomeValet v
sGetter (SomeValet (Apply g v))     = SomeValet $ g <*> v
sGetter (SomeValet (Value x))       = SomeValet $ Value x

{-|
Display a value contained in a 'Valet' or value agnostic valet ('SomeValet')
as 'Text'.

If no function to render to render the 'Valet' (which can be set using
the 'showValueFunc' lens) then an empty 'Text' is returned ("").
-}
showValue :: Coerce b (SomeValet r m) => Getter b T.Text
showValue = to $ \sv -> case Data.Valet.coerce sv of
    SomeValet (Value _)       -> ""
    SomeValet (ShowValue g v) -> g $ v ^. value
    SomeValet (Apply g v)     -> let t1 = g ^. showValue
                                     t2 = v ^. showValue
                                 in
                                 if t1 /= ""
                                 then t1 <> ", " <> t2
                                 else t2
    v                         -> sGetter v ^. showValue

{-|
Get the key of a 'Valet'.
-}
getKey :: Coerce b (SomeValet r m) => b -> T.Text
getKey sv = case Data.Valet.coerce sv of
    SomeValet (Value _)   -> ""
    SomeValet (Key x _)   -> x
    SomeValet (Apply _ _) -> ""
    v                     -> getKey $ sGetter v

{-|
Return the result of the analysis performed on a valet after its values have
been modified.

* Use
This can be usefull, if you wish to validate the value of a specific element
of a 'Valet'.

* Examples
To obtain a report, you first need to obtain a value agnostic valet
of type 'SomeValet' turning the 'lookup' function into a 'Getter'.

> somePerson :: Maybe (SomeValet r m)
> somePerson = person ^. to (lookup "name)

As the lookup may fail and return a @Maybe (SomeValet r m)@
rather than @SomeValet r m@ the 'report' 'Getter' will need to be used
in conjunction with the '_Just' 'Prism':

> myReport :: m (Maybe r)
> myReport = person ^. to (lookup "name") . _Just . report

Another way to achieve the same result is to turn person into 'SomeValet'
and then use the 'at' lens.

> myReport = someValet person ^. at "name" . _Just . report

TODO: check the validity of the example.
-}
report ::
       (Coerce a (SomeValet r m), Monad m, Monoid r)
    => Getter a (m (Maybe r))
report =
    to $ result' . Data.Valet.coerce
    where
        result' (SomeValet v) = do
            r <- v ^. eval
            case r of
                Success _  -> return Nothing
                Failure e  -> return $ Just e

{-|
Render a value and its sub-values.
-}
render :: (Coerce b (SomeValet r m), Monoid r) => Getter b r
render = to $ \sv -> case Data.Valet.coerce sv of
    v@(SomeValet (Key _ v')) -> (v' ^. renderer $ v) <> (someValet v') ^. render
    SomeValet (Apply g v)    -> g ^. render <> v ^. render
    SomeValet (Value _)      -> mempty
    v                        -> (view render) $ sGetter v

{-|
Render the sub-valets of a 'Valet' but not the 'Valet' itself.
-}
renderSubValets :: (Coerce b (SomeValet r m), Monoid r) => Getter b r
renderSubValets = to $ \sv -> case Data.Valet.coerce sv of
    SomeValet (Value _)   -> mempty
    SomeValet (Apply g v) -> g ^. render <> v ^. render
    v                     -> (view renderSubValets) $ sGetter v

-- | Lookup for a valet returning a value agnostic valet.
lookup :: Coerce b (SomeValet r m) => T.Text -> Getter b (Maybe (SomeValet r m))
lookup k = to $ \s -> case Data.Valet.coerce s of
    SomeValet (Value _)       -> Nothing
    v@(SomeValet (Key x v'))  -> if k == x then Just v else v' ^. lookup k
    SomeValet (ShowValue _ v) -> SomeValet v ^. lookup k
    SomeValet (Read _ v)      -> SomeValet v ^. lookup k
    SomeValet (Render _ v)    -> SomeValet v ^. lookup k
    SomeValet (Modify _ v)    -> SomeValet v ^. lookup k
    SomeValet (Analyse _ v)   -> SomeValet v ^. lookup k
    SomeValet (Apply g v)     ->    SomeValet g ^. lookup k
                               <|> SomeValet v ^. lookup k

{-|
Get all the keys of a 'Valet' and its sub-valets.

This can be usefull if you wish to ensure that all the keys of a given
'Valet' are indeed unique (as they should be!).

Typically:
> areKeysUnique :: Bool
> areKeysUnique = let ks = valet ^. keys in nub ks == ks

TODO: check the validity of the above example.
-}
keys :: Coerce b (SomeValet r m) => Getter b [T.Text]
keys = to $ \sv -> case Data.Valet.coerce sv of
    SomeValet (Value _)   -> []
    SomeValet (Key x v)   -> [x] <> v ^. keys
    SomeValet (Apply g v) -> g ^. keys <> v ^. keys
    v                     -> (view keys) $ sGetter v

----------------------------------------
-- LENSES
----------------------------------------

{-|
Value contained in a valet.

As you need to define a value when you create a 'Valet' using 'valet' or 'pure'
functions, this lens is needed only if you wish to set a new value:
> valet' :: Valet r m String
> valet' = valet ~. "new value"

or if you want to access the initially defined value (without applying any
modifications nor analysis):
> myValue :: String
> myValue = valet ^. value

TODO: check that the above examples are valid.
-}
value :: Lens' (Valet r m a) a
value = lens getValue setValue

{-|
Function which convert the value of a 'Valet' to 'Text'.

It is usefull to have such function when you wish to display the value
of the 'Valet' in the renderer function (as the renderer takes as
parameter a value agnostic valet ('SomeValet') rather than a 'Valet').
-}
showValueFunc :: Lens' (Valet r m a) (a -> T.Text)
showValueFunc = lens getShowValue setShowValue

{-|
Key of a valet.

This key should be unique as it can be used for interacting with the sub-valets
contained in a valet.

To ensure this, you can use the 'keys' function or 'keyi' lens.
-}
key :: (Monoid r, Monad m) => Lens' (Valet r m a) T.Text
key = lens getKey setKey

{-|
Analysis of a value.
-}
analyser ::
       (Monad m, Monoid r)
    => Lens' (Valet r m a) (Analysis r m a)
analyser = lens getAnalyser setAnalyser

-- | Modification of a value.
modifier ::
       (Monad m, Monoid r)
    => Lens' (Valet r m a) (Modif m a)
modifier = lens getModifier setModifier

-- | Reader of a value.
reader :: Lens' (Valet r m a) (r -> a)
reader = lens getReader setReader

{-|
Renderer of a valet.

This renderer could for example render the value as:
- a JSON object;
- a HTML form;
- a SQL statement;
- etc.

It could also be a combination of multiple renderers with a data type such as:
@
data Renderer = Renderer
    { json :: Json
    , html :: Html
    }
@
-}
renderer :: Monoid r => Lens' (Valet r m a) (SomeValet r m -> r)
renderer = lens getRenderer setRenderer

type RAnalysis r m a = (Analysis r m a, SomeValet r m -> r)
type RModif r m a = (Modif m a, SomeValet r m -> r)

{-|
Lens combining in a tuple an analysis and a rendering (expected to be the
rendering of the analysis).
-}
rAnalyser :: (Monad m, Monoid r) => Lens' (Valet r m a) (RAnalysis r m a)
rAnalyser = lens
    (\x -> (getAnalyser x, getRenderer x))
    (\z (x , y) -> setAnalyser (setRenderer z y) x)

{-|
Lens combining in a tuple a modifier and a rendering (expected to be the
rendering of the modifier).
-}
rModifier :: (Monad m, Monoid r) => Lens' (Valet r m a) (RModif r m a)
rModifier = lens
    (\x -> (getModifier x, getRenderer x))
    (\z (x, y) -> setModifier (setRenderer z y) x)

----------------------------------------
-- Indexed lenses
----------------------------------------

{-|
Key of a sub-valet.

This lens can be used to modify the current key or to check if a given
key exists.

-- TODO: examples.
-}
keyi ::
       (Monoid r, Monad m)
    => T.Text                     -- ^ Key of the key.
    -> Lens' (Valet r m a) T.Text
keyi k = lens (\x -> x ^. lookup k . _Just . to getKey) (putKey k)

----------------------------------------
-- Indexed Setters
----------------------------------------

{-|
Replace the key of the sub-value matching the provided key, so the
current key will be replaced by the new one.
-}
putKey ::
       T.Text -- ^ Current key.
    -> Valet r m a
    -> T.Text -- ^ New key.
    -> Valet r m a
putKey k vt k' = case vt of
    Key x v   -> if k == x then Key k' v else Key x (putKey k v k')
    Apply g v -> putKey k g k' <*> putKey k v k'
    x         -> setter (putKey k) x k'

{-|
Insert the provided data in the sub-value matching the key.
-}
putValue ::
       Monad m
    => T.Text        -- ^ Key.
    -> Valet r m a
    -> r             -- ^ Data.
    -> Valet r m a
putValue key' = putValueReader key' Nothing

{-|
Insert the provided data in the sub-value matching the key
using the provided reader function, if any.
-}
putValueReader ::
       Monad m
    => T.Text         -- ^ Key.
    -> Maybe (r -> a) -- ^ Optional reader function.
    -> Valet r m a
    -> r              -- ^ Data.
    -> Valet r m a
putValueReader key' reader' form val = case form of
    Value x  -> case reader' of
                     Just g  -> Value $ g val
                     Nothing -> Value x
    Read g v -> Read g $ putValueReader key' (Just g) v val
    Key x v  -> if key' == x
                then Key x $ putValueReader key' reader' v val
                else Key x (valueReader reader' v val)
    Apply g v ->     putValueReader key' Nothing g val
                 <*> putValueReader key' Nothing v val
    x         -> setter (putValueReader key' reader') x val
    where
        valueReader ::
            Monad m => Maybe (r -> a) -> Valet r m a -> r -> Valet r m a
        valueReader reader'' vt newValet = case vt of
            Value x -> case reader'' of
                           Just g -> Value $ g newValet
                           Nothing -> Value x
            Apply g v ->     putValueReader key' Nothing g newValet
                         <*> putValueReader key' Nothing v newValet
            x -> setter (valueReader reader'') x newValet

{-|
Replace the current renderer of a sub-valet matching the provided key
by a new one.

-- TODO: implement as a lens.
-}
putRenderer ::
       T.Text       -- ^ Key.
    -> Valet r m a
    -> Rendered r m -- ^ Renderer.
    -> Valet r m a
putRenderer k vt r = case vt of
    Key x v    -> if k == x
                  then Key k (putRenderer' k v r)
                  else Key k (putRenderer k v r)
    Apply g v  -> putRenderer k g r <*> putRenderer k v r
    x          -> setter (putRenderer k) x r
    where
        putRenderer' k' vt' r' = case vt' of
            Render _ v -> Render r' v
            x          -> setter (putRenderer' k') x r'

----------------
--- EXAMPLES ---
----------------

-- Validators.

minLength :: Monad m => Int -> RAnalysis T.Text m T.Text
minLength min =
    ( check
          (\x -> return (T.length x >= min))
          (\y -> "For " <> (y ^. to getKey) <> " the minimal length is "
                 <> T.pack (show min) <> ".")
    , const "function minLength();"
    )

failure :: a -> Bool
failure _ = False

-- Filters.

{-|
-- Monadic filters.
randomInt :: T.Text -> Int -> IO (Result T.Text Int)
randomInt _ x = do
    r <- getStdRandom (randomR (1,6))
    return $ Success (r * x)
-}

-- Elements.

varchar ::
       Monad m
    => T.Text  -- ^ Key.
    -> Int     -- ^ Max length.
    -> Valet T.Text m T.Text
varchar name l =
    pure mempty
        & key           .~ name
        & showValueFunc .~ T.pack . show
        & reader        .~ id
        & renderer      .~ renderVarchar l

int :: Monad m => T.Text -> Valet T.Text m Int
int name =
    valet 0
        & key           .~ name
        & showValueFunc .~ T.pack . show
        & reader        .~ (read . T.unpack)
        & renderer      .~ renderInt

-- Renderers.

renderInt :: Monad m => SomeValet T.Text m -> T.Text
renderInt f =
       "<input name=\"" <> getKey f <> "\" "
    <> "type=\"text\" value=\"" <> f ^. showValue <> "\"/>"

renderVarchar :: Monad m => Int -> SomeValet T.Text m -> T.Text
renderVarchar l f =
       "<input name=\"" <> getKey f <> "\" "
    <> "type=\"text\" value=\"" <> f ^. showValue <> "\" "
    <> "length=\"" <> T.pack (show l) <> "\""
    <> "/>"

-- Example.

data Page = Page
    { url :: T.Text
    , pageName :: T.Text
    , visits :: Int
    } deriving (Show)

myTest :: Valet T.Text Maybe T.Text
myTest = varchar "url" 256
      & rModifier <>~ ( modif (return . T.toUpper)
                      , \x -> "<script>toUpper(\"" <> x ^. to getKey <> "\");</script>"
                      )
      & rAnalyser <>~ minLength 4

{-|
Example of a custom rendering.
-}
renderPage :: Monad m => Valet T.Text m Page -> T.Text
renderPage f =
       "<form name=\"" <> f ^. key <> "\"> "
    <>     "<h1>My url</h1>"
    <>     f ^. at "url" . non ""
    <> "</form>"

myUrl :: Valet T.Text Maybe T.Text
myUrl =
    varchar "url" 256
      & rModifier <>~ ( modif (return . T.toUpper)
                      , \x -> "<script>toUpper(\"" <> x ^. to getKey <> "\");</script>"
                      )
      & rAnalyser <>~ minLength 4

myName :: Valet T.Text Maybe T.Text
myName =
    varchar "page" 256
        & rAnalyser <>~ minLength 3

myVisits :: Valet T.Text Maybe Int
myVisits = int "visits"

myPage :: Valet T.Text Maybe Page
myPage = (Page <$> myUrl <*> myName <*> myVisits) & key .~ "t"

mySetName :: Valet T.Text Maybe T.Text
mySetName = myName & value .~ "home"

mySetUrl :: Valet T.Text Maybe T.Text
mySetUrl = myUrl & value .~ "http"

mySetPage :: Valet T.Text Maybe Page
mySetPage = myPage
    & at "url" ?~ "http"
    & at "visits" ?~ "10"
    & at "page" ?~ "home"

test = (myPage & at "url" ?~ "H!") ^. lookup "url" . _Just . report
