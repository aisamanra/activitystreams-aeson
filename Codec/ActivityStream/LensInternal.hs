{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

-- Okay, I'm gonna justify this in a comment: I will never, under any
-- circumstances, build a library that has an explicit `lens` dependency.
-- I think `lens` is awesome, but it is also a giant package, and I
-- don't want to inflict it on end-users who might not want it.

-- I am more okay with lens-family-core, but in this case, all I need
-- is really the `makeLens` function, which (not counting whitespace
-- and comments) is three lines of code. Three lines! And it doesn't make
-- sense to drag in a whole extra package when I can just copy this
-- in.

-- There's also reimplementations of `get` and `set` for possible internal
-- use---three lines each, for a total of nine. Nine lines of
-- easily-copyable, verifiable boilerplate. Instead of another dependency
-- that must be downloaded and installed and managed by Cabal and
-- addressed in constraint-solving...

-- And that is why this module reimplement a few `lens` functions.

module Codec.ActivityStream.LensInternal
         ( get
         , set
         , Lens'
         , makeLens
         , makeAesonLensMb
         , makeAesonLens
         ) where

import           Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromJust)
import           Data.Text (Text)

-- We need these to write get and set
newtype C a b = C { fromC :: a } deriving (Functor)
newtype I a   = I { fromI :: a } deriving (Functor)

-- This is the same type alias as in @Control.Lens@, and so can be used
-- anywhere lenses are needed.
type Lens' a b = forall f. Functor f => (b -> f b) -> (a -> f a)

get :: Lens' a b -> a -> b
get lens a = fromC (lens C a)

set :: Lens' a b -> b -> a -> a
set lens x a = fromI (lens (const I x) a)

makeLens :: (a -> b) -> (b -> a -> a) -> Lens' a b
makeLens get set f a = (`set` a) `fmap` f (get a)

-- This is necessary because of the way we store values as Aeson
-- values underneath.
fromJSON' :: FromJSON a => Aeson.Value -> Maybe a
fromJSON' v = case fromJSON v of
  Success a -> Just a
  Error _   -> Nothing

-- Create a lens into an Aeson object wrapper that takes and
-- returns a Maybe value. When used as a setter, it can either
-- insert a value in, or delete it from the object (if it is
-- used with Nothing.)
makeAesonLensMb :: (FromJSON v, ToJSON v)
                => Text -> Lens' c Aeson.Object -> Lens' c (Maybe v)
makeAesonLensMb key fromObj = fromObj . makeLens g s
  where g o = HM.lookup key o >>= fromJSON'
        s (Just v) o = HM.insert key (toJSON v) o
        s Nothing  o = HM.delete key o


-- Create a lens into an Aeson object wrapper. This will fail if
-- the object does not contain the relevant key.
makeAesonLens :: (FromJSON v, ToJSON v)
              => Text -> Lens' c Aeson.Object -> Lens' c v
makeAesonLens key fromObj = fromObj . makeLens g s
  where g o   = fromJust (HM.lookup key o >>= fromJSON')
        s v o = HM.insert key (toJSON v) o
