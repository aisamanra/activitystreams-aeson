{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Codec.ActivityStream.LensInternal where

import           Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromJust)
import           Data.Text (Text)

-- This way, we don't have to import lens... but we can still export lenses!
newtype Const a b = Const { fromConst :: a }
instance Functor (Const a) where fmap f (Const x) = Const x

-- We need these to write get and set
newtype Id a = Id { fromId :: a }
instance Functor Id where fmap f (Id x) = Id (f x)

-- | This is the same type alias as in @Control.Lens@, and so can be used
--   anywhere lenses are needed.
type Lens' a b = forall f. Functor f => (b -> f b) -> (a -> f a)

get :: Lens' a b -> a -> b
get lens a = fromConst (lens Const a)

set :: Lens' a b -> b -> a -> a
set lens x a = fromId (lens (const Id x) a)

makeLens :: (a -> b) -> (b -> a -> a) -> Lens' a b
makeLens get set f a = (`set` a) `fmap` f (get a)

fromJSON' :: FromJSON a => Aeson.Value -> Maybe a
fromJSON' v = case fromJSON v of
  Success a -> Just a
  Error _   -> Nothing

-- Create a lens into an Aeson object wrapper that takes and
-- returns a Maybe value
makeAesonLensMb :: (FromJSON v, ToJSON v)
                => Text -> Lens' c Aeson.Object -> Lens' c (Maybe v)
makeAesonLensMb key fromObj = fromObj . lens
  where lens = makeLens
                 (\ o -> HM.lookup key o >>= fromJSON')
                 (\case Just v  -> HM.insert key (toJSON v)
                        Nothing -> HM.delete key)


-- Create a lens into an Aeson object wrapper
makeAesonLens :: (FromJSON v, ToJSON v)
              => Text -> Lens' c Aeson.Object -> Lens' c v
makeAesonLens key fromObj = fromObj . lens
  where lens = makeLens
                 (\ o -> fromJust (HM.lookup key o >>= fromJSON'))
                 (\ v o -> HM.insert key (toJSON v) o)
