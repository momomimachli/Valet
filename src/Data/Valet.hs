{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Data.Value
Description : Abstract Value which can be evaluated and rendered.
Copyright   : (c) Leonard Monnier, 2015
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

The main aim of this library is to provide a more general implementation
of the digestive-functors package.

The main difference are:
- a transformer can also have a view:
- it is not mainly targeted toward HTML forms
- a form created with this library is not necessarely a formlet.

It shares also some very close similarities such as:

** Use of 'Applicative'

The use of 'Applicative' implies that the whole structure is evaluated in any
cases, even following an error. Therefore, all errors are collected – rather
than being able to stop the computation as it would be the case with a 'Monad'.


This library is mainly organised around the 'Value'.
A 'Value' contains a data of a given type and is identified by a key
(which must be unique).
Here is a basic 'Text' 'Value':
> textValue :: Text -> Text -> Value r m b Text
> textValue = Value

The last type of the value is now 'Text' which indicates that
its data is of type 'Text'.

A value can be set from a given type, matching the provided reader function.
In our case, the reader function is a function which converts the given type to
a 'Text'.
For example, if we want to be able to set a value from a 'String':
> textValue :: Text -> Value r m String Text
> textValue key data = reader pack $ Value key data

It is now possible to modify the data of an existing value:
> name = textValue "name" ""
> haskell = setData "name" "Haskell"

** Transformations

It is also possible to add transformations to a value.
Tranformations can be of two kinds:
- the ones which just modify a value but never fail: the "filters";
- the ones which can fail: the "validators".

Transformations return a 'Result' which can be either the transformed value
in case of success or an error in case of failure.

To add a filter converting a text to upper case:
> upperTextValue :: Text -> Text -> Value r m String Text
> upperTextValue key data =
>     transformer toUpper $ textValue key data
>     where
>         toUpper :: Text -> Result Text Text
>         toUpper = Success . upper
-}
module Data.Valet
    (
      -- * Types
      Value
    , Result

      -- * Constructors

      -- | The below functions are the one needed to create a new Value.

    , value
    , reader
    , renderer
    , transformer
    , transformerM

      -- * Getters
    , eval
    , getData

    , -- * Setters
      setData

      -- * Renderers
    , render
    , renderKey
    ) where

import Control.Applicative
import Data.Maybe
import Data.Monoid

import qualified Data.Text as T

----------------------------------------
-- Data types
----------------------------------------

{-|
A "value" which has the following properties:
- it contains a data
- this data can be modified and/or validated
- it can be rendered
- it may have a name
- it may contain other values.

This data type is rather generic and multi-purposes.
You can customise it to fulfill your requirements.

Its type means the following:
- r: rendered value and erros;
- m: monad in which transformations occur;
- b: type which can be converted to a data;
- a: data contained in the value.
-}
data Value r m b a where

    -- | Create a value by setting its data.
    Value :: a -> Value r m b a

    -- | Convert a value of type b to the type a of the value data.
    Read :: (b -> a) -> Value r m b a -> Value r m b a

    -- | Set a name to a value.
    Name :: T.Text -> Value r m b a -> Value r m b a

    -- | Add a renderer a value.
    Render :: (Value r m b a -> r) -> Value r m b a -> Value r m b a

    -- | Add a transformer to a value.
    Map :: Monoid r => Mapper r m a -> Value r m b a -> Value r m b a

    -- | Applicative transformation.
    Apply :: Monoid r => Value r m b (c -> a) -> Value r m b c -> Value r m b a

-- | Show instance for debugging purposes.
instance Show (Value r m b a) where
    show (Value x)     = "Value x:"
    show (Read _ f)    = "Read g: " ++ show f
    show (Name x f)    = "Name " ++ show x ++ ": " ++ show f
    show (Render _ f)  = "Render r: " ++ show f
    show (Map _ f)     = "Map g: " ++ show f
    show (Apply f1 f2) = "Apply: (" ++ show f1 ++ ", " ++ show f2 ++ ")"

instance (Monoid r) => Functor (Value r m b) where
    fmap g = Apply (Value g)

instance (Monoid r) => Applicative (Value r m b) where
    pure = Value
    (<*>) = Apply

{-|
Mappers used for the transformation.

The mappers has also an optional renderer which can be set
to render a transformation. In the case of a webform it
could be as a javascript for example.

The types are the following:
- @r@: optional renderer to display the tranformation;
- @m@: monad inside which the transformation is occuring;
- @a@: type of the data on which the transformation is performed.
-}
data Mapper r m a = Mapper
    {
      -- | Transformation function.
      mapFunc   :: T.Text            -- ^ Key of the 'Value'.
                   -> a              -- ^ Initial data of the 'Value'.
                   -> m (Result r a) -- ^ Tranformed result.

      -- | Optional renderer.
    , mapRender :: Maybe r
    }

{-|
A result which can denote a Success by returning a data,
or a failure by returning a type which can be rendered.
Such value must also be an instance of Monoid.

The types are the following:
- @a@: data of the 'Value';
- @r@: renderer used to display the error(s);
-}
data Result r a =
      Success a
    | Failure r
      deriving(Show)

instance Functor (Result r) where
    fmap g (Success x) = Success $ g x
    fmap _ (Failure e) = Failure e

instance Monoid r => Applicative (Result r) where
    pure = Success
    (Success g) <*> (Success x) = Success $ g x
    (Failure e) <*> (Success _) = Failure e
    (Success _) <*> (Failure e) = Failure e
    (Failure e1) <*> (Failure e2) = Failure (e1 <> e2)

instance Monoid r => Monad (Result r) where
    (Success x) >>= g = g x
    (Failure e) >>= _ = Failure e

----------------------------------------
-- Helpers
----------------------------------------

{-|
Get the key of a value.
-}
getKey :: Value r m b a -> T.Text
getKey (Value x)    = ""
getKey (Read _ f)   = getKey f
getKey (Name x _)   = x
getKey (Render _ f) = getKey f
getKey (Map _ f)    = getKey f
getKey (Apply _ _)  = ""

{-|
Set the data of a value matching the provided key.
-}
setData :: Monad m => T.Text -> b -> Value r m b a -> Value r m b a
setData key val form =
    setData' key val Nothing form
    where
        setData' ::
               Monad m
            => T.Text
            -> b
            -> Maybe (b -> a)
            -> Value r m b a
            -> Value r m b a
        setData' key' val' reader form' =
            case form' of
                Value x    -> case reader of
                                  Just f  -> Value $ f val'
                                  Nothing -> Value x
                Read g f   -> Read g $ setData' key' val' (Just g) f
                Name x f   -> if key' == x
                                  then Name x $ setData' key' val' reader f
                                  else Name x f
                Render x f -> Render x $ setData' key' val' reader f
                Map g f    -> Map g $ setData' key' val' reader f
                Apply g f  ->     setData' key' val' Nothing g
                              <*> setData' key' val' Nothing f

----------------------------------------
-- Getting values
----------------------------------------

-- | Retrieve the data of a value without applying any transformers.
getData :: Value r m b a -> a
getData (Value x)    = x
getData (Name _ f)   = getData f
getData (Read _ f)   = getData f
getData (Render _ f) = getData f
getData (Map _ f)    = getData f
getData (Apply g f)  = (getData g) (getData f)

{-|
Evaluate a value applying (on the data) the tranformer which have been
previously set.
-}
eval :: (Monad m, Monoid r) => Value r m b a -> m (Result r a)
eval (Value x)    = return $ Success x
eval (Name _ f)   = eval f
eval (Read _ f)   = eval f
eval (Render _ f) = eval f
eval (Map g f)    = do
                        r <- eval f
                        case r of
                            Success x -> (mapFunc g $ getKey f) x
                            Failure e -> return $ Failure e
eval (Apply g f)  = do
                        h <- eval g
                        v <- eval f
                        return (h <*> v)

----------------------------------------
-- Renderers
----------------------------------------

{-|
Render a value and its sub-values.
-}
render :: Monoid r => Value r m b a -> r
render (Value _)    = mempty
render (Name _ f)   = render f
render (Read _ f)   = render f
render (Render x f) = x f
render (Map g f)    = case mapRender g of
                         Just r  -> r <> render f
                         Nothing -> render f
render (Apply g f)  = render g <> render f

{-|
Render the value matching the given key.
-}
renderKey :: Monoid r => T.Text -> Value r m b a -> r
renderKey k (Value _)    = mempty
renderKey k (Name x f)   = if k == x then render f else mempty
renderKey k (Read _ f)   = renderKey k f
renderKey k (Render x f) = renderKey k f
renderKey k (Map _ f)    = renderKey k f
renderKey k (Apply g v)  = renderKey k g <> renderKey k v

----------------------------------------
-- CONSTRUCTORS
----------------------------------------

{-|
Create a new value.

Setting up an identifier is mandatory.
This will allow to use the identifier as a key for tasks such as:
- changing the data contained in the value;
- rendering this specific value.
-}
value ::
       T.Text        -- ^ Value identifier.
    -> a             -- ^ Default value.
    -> Value r m b a
value k x = Name k $ Value x

{-|
Provide a function which convert a given type to the one of the data.

This will then be used to set the data of a value by functions
such as 'setData'.
-}
reader :: (b -> a) -> Value r m b a -> Value r m b a
reader g (Name x v) = Name x (Read g v)
reader _ v          = v

{-|
Set the renderer of a value.

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
renderer :: (Value r m b a -> r) -> Value r m b a -> Value r m b a
renderer g (Name x v) = Name x (Render g v)
renderer _ v          = v

{-|
Add a pure transformation to a value.
-}
transformer ::
       (Monad m ,Monoid r)
    => (T.Text -> a -> Result r a)
    -> Maybe r
    -> Value r m b a
    -> Value r m b a
transformer g = transformerM (\k v -> return $ g k v)

{-|
Add a monadic transformation to a value.

Typically, it could be a check against data stored in a database –
to verify for that a data is unique for example.
In such case, the @m@ type would be IO.
-}
transformerM ::
     ( Monad m -- ^ Monad in which the tranformation applies.
     , Monoid r
     )
    => (T.Text -> a -> m (Result r a)) -- ^ Tranformer function.
    -> Maybe r                         -- ^ Optional renderer.
    -> Value r m b a                 -- ^ Value on which the transformer applies.
    -> Value r m b a
transformerM g r = Map (Mapper g r)

{-|
----------------
--- EXAMPLES ---
----------------

-- Validators.

minLength :: Int -> T.Text -> T.Text -> Result T.Text T.Text
minLength min key x
    | T.length x >= min = Success x
    | otherwise         = Failure msg
    where
        msg = "Too short, the minimal length is " <> T.pack (show min) <> "."

-- Filters.

upper :: T.Text -> T.Text -> Result T.Text T.Text
upper _ x = Success $ T.toUpper x

-- Monadic filters.
randomInt :: T.Text -> Int -> IO (Result T.Text Int)
randomInt _ x = do
    r <- getStdRandom (randomR (1,6))
    return $ Success (r * x)

-- Elements.

varchar ::
       T.Text  -- ^ Name.
    -> Int     -- ^ Max length.
    -> Value T.Text m T.Text T.Text
varchar name l = renderer (renderVarchar l) $ reader id $ value name ""

int :: T.Text -> Value T.Text m T.Text Int
int name = renderer renderInt $ reader (read . T.unpack) $ value name 0

-- Renderers.

renderInt :: Value T.Text m b Int -> T.Text
renderInt f =
       "<input name=\"" <> getKey f <> "\" "
    <> "type=\"text\" value=\"" <> T.pack (show $ getData f) <> "\"/>"

renderVarchar :: Int -> Value T.Text m b T.Text -> T.Text
renderVarchar l f =
       "<input name=\"" <> getKey f <> "\" "
    <> "type=\"text\" value=\"" <> T.pack (show $ getData f) <> "\" "
    <> "length=\"" <> T.pack (show l) <> "\""
    <> "/>"

-- Example.

data Page = Page
    { url :: T.Text
    , pageName :: T.Text
    , visits :: Int
    } deriving (Show)

{-|
Example of a custom rendering.
-}
renderPage :: Value T.Text m T.Text Page -> T.Text
renderPage f =
       "<form name=\"" <> getKey f <> "\"> "
    <>     "<h1>My url</h1>"
    <>     renderKey "url" f
    <> "</form>"

myUrl :: Monad m => Value T.Text m T.Text T.Text
myUrl =
      transformer upper (Just "<script>toUpper(\"url\");</script>")
    $ transformer (minLength 4) Nothing
    $ varchar "url" 256

myName :: Value T.Text m T.Text T.Text
myName = varchar "page" 256

myVisits :: Value T.Text IO T.Text Int
myVisits = transformerM randomInt Nothing $ int "visits"

myPage :: Value T.Text IO T.Text Page
myPage = Page <$> myUrl <*> myName <*> myVisits

mySetPage :: Value T.Text IO T.Text Page
mySetPage = foldr (\(x, y) p -> setData x y p) myPage [("visits", "15"), ("url", "http")]
-}
