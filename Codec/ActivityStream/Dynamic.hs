{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Codec.ActivityStream.Dynamic
Description : A (more dynamic) interface to Activity Streams
Copyright   : (c) Getty Ritter, 2014
Maintainer  : gdritter@galois.com

This is an interface to ActivityStreams that simply wraps an underlying
@aeson@ Object, and exposes a set of (convenient) lenses to access the
values inside. If an @aeson@ object is wrapped in the respective wrapper,
it will contain the obligatory values for that type (e.g. an @Activity@
is guaranteed to have a @published@ date.)
-}

module Codec.ActivityStream.Dynamic
       ( Lens'
         -- * MediaLink
       , MediaLink
       , mlDuration
       , mlHeight
       , mlWidth
       , mlURL
       , mlRest
       , makeMediaLink
         -- * Object
       , Object
       , oAttachments
       , oAuthor
       , oContent
       , oDisplayName
       , oDownstreamDuplicates
       , oId
       , oImage
       , oObjectType
       , oPublished
       , oSummary
       , oUpdated
       , oUpstreamDuplicates
       , oURL
       , oRest
       , emptyObject
         -- * Activity
       , Activity
       , acActor
       , acContent
       , acGenerator
       , acIcon
       , acId
       , acPublished
       , acProvider
       , acTarget
       , acTitle
       , acUpdated
       , acURL
       , acVerb
       , acRest
       , makeActivity
         -- * Collection
       , Collection
       , cTotalItems
       , cItems
       , cURL
       , cRest
       , makeCollection
       ) where

import           Data.Aeson ( FromJSON(..)
                            , ToJSON(..)
                            , Result(..)
                            , fromJSON
                            )
import qualified Data.Aeson as A
import           Data.DateTime (DateTime)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)

import Codec.ActivityStream.LensInternal ( Lens'
                                         , makeLens
                                         , makeAesonLens
                                         , makeAesonLensMb
                                         )

data MediaLink = MediaLink { fromMediaLink :: A.Object } deriving (Eq, Show)

instance FromJSON MediaLink where
  parseJSON (A.Object o) | HM.member "url" o = return (MediaLink o)
                       | otherwise         = fail "..."
  parseJSON _ = fail "..."

instance ToJSON MediaLink where
  toJSON (MediaLink o) = A.Object o

mlRest :: Lens' MediaLink A.Object
mlRest = makeLens fromMediaLink (\ o' m -> m { fromMediaLink = o' })

mlDuration :: Lens' MediaLink (Maybe Int)
mlDuration = makeAesonLensMb "duration" mlRest

mlHeight :: Lens' MediaLink (Maybe Int)
mlHeight = makeAesonLensMb "height" mlRest

mlWidth :: Lens' MediaLink (Maybe Int)
mlWidth = makeAesonLensMb "width" mlRest

mlURL :: Lens' MediaLink Text
mlURL = makeAesonLens "url" mlRest

-- | Create a @MediaLink@ with just a @url@ property.
makeMediaLink :: Text -> MediaLink
makeMediaLink url = MediaLink (HM.insert "url" (toJSON url) HM.empty)

-- | Object

data Object = Object { fromObject :: A.Object } deriving (Eq, Show)

instance FromJSON Object where
  parseJSON (A.Object o) = return (Object o)
  parseJSON _            = fail "..."

instance ToJSON Object where
  toJSON (Object o) = A.Object o

oRest :: Lens' Object A.Object
oRest = makeLens fromObject (\ o' m -> m { fromObject = o' })

oAttachments :: Lens' Object (Maybe [Object])
oAttachments = makeAesonLensMb "attachments" oRest

oAuthor :: Lens' Object (Maybe Object)
oAuthor = makeAesonLensMb "author" oRest

oContent :: Lens' Object (Maybe Text)
oContent = makeAesonLensMb "content" oRest

oDisplayName :: Lens' Object (Maybe Text)
oDisplayName = makeAesonLensMb "displayName" oRest

oDownstreamDuplicates :: Lens' Object (Maybe [Text])
oDownstreamDuplicates = makeAesonLensMb "downstreamDuplicates" oRest

oId :: Lens' Object (Maybe Text)
oId = makeAesonLensMb "id" oRest

oImage :: Lens' Object (Maybe MediaLink)
oImage = makeAesonLensMb "image" oRest

oObjectType :: (FromJSON o, ToJSON o) => Lens' Object (Maybe o)
oObjectType = makeAesonLensMb "objectType" oRest

oPublished :: Lens' Object (Maybe DateTime)
oPublished = makeAesonLensMb "published" oRest

oSummary :: Lens' Object (Maybe Text)
oSummary = makeAesonLensMb "summary" oRest

oUpdated :: Lens' Object (Maybe DateTime)
oUpdated = makeAesonLensMb "updated" oRest

oUpstreamDuplicates :: Lens' Object (Maybe [Text])
oUpstreamDuplicates = makeAesonLensMb "upstreamDuplicates" oRest

oURL :: Lens' Object (Maybe Text)
oURL = makeAesonLensMb "url" oRest

-- | Create an @Object@ with no fields.
emptyObject :: Object
emptyObject = Object HM.empty

-- | Activity

data Activity = Activity { fromActivity :: A.Object } deriving (Eq, Show)

instance FromJSON Activity where
  parseJSON (A.Object o)
    | HM.member "published" o && HM.member "provider" o = return (Activity o)
    | otherwise = fail "..."
  parseJSON _ = fail "..."

instance ToJSON Activity where
  toJSON (Activity o) = A.Object o

acRest :: Lens' Activity A.Object
acRest = makeLens fromActivity (\ o' m -> m { fromActivity = o' })

acActor :: Lens' Activity Object
acActor = makeAesonLens "actor" acRest

acContent :: Lens' Activity (Maybe Text)
acContent = makeAesonLensMb "content" acRest

acGenerator :: Lens' Activity (Maybe Object)
acGenerator = makeAesonLens "generator" acRest

acIcon :: Lens' Activity (Maybe MediaLink)
acIcon = makeAesonLensMb "icon" acRest

acId :: Lens' Activity (Maybe Text)
acId = makeAesonLensMb "id" acRest

acPublished :: Lens' Activity DateTime
acPublished = makeAesonLens "published" acRest

acProvider :: Lens' Activity (Maybe Object)
acProvider = makeAesonLensMb "provider" acRest

acTarget :: Lens' Activity (Maybe Object)
acTarget = makeAesonLensMb "target" acRest

acTitle :: Lens' Activity (Maybe Text)
acTitle = makeAesonLensMb "title" acRest

acUpdated :: Lens' Activity (Maybe DateTime)
acUpdated = makeAesonLensMb "updated" acRest

acURL :: Lens' Activity (Maybe Text)
acURL = makeAesonLensMb "url" acRest

acVerb :: (FromJSON v, ToJSON v) => Lens' Activity (Maybe v)
acVerb = makeAesonLensMb "verb" acRest

-- | Create an @Activity@ with an @actor@, @published@, and
--   @provider@ property.
makeActivity :: Object -> DateTime -> Object -> Activity
makeActivity actor published provider = Activity
  $ HM.insert "actor"     (toJSON actor)
  $ HM.insert "published" (toJSON published)
  $ HM.insert "provider"  (toJSON provider)
  $ HM.empty

-- | Collection

data Collection = Collection { fromCollection :: A.Object } deriving (Eq, Show)

instance FromJSON Collection where
  parseJSON (A.Object o) = return (Collection o)
  parseJSON _            = fail "..."

instance ToJSON Collection where
  toJSON (Collection o) = A.Object o

cRest :: Lens' Collection A.Object
cRest = makeLens fromCollection (\ o' m -> m { fromCollection = o' })

cTotalItems :: Lens' Collection (Maybe Int)
cTotalItems = makeAesonLensMb "totalItems" cRest

cItems :: Lens' Collection (Maybe [Object])
cItems = makeAesonLensMb "items" cRest

cURL :: Lens' Collection (Maybe Text)
cURL = makeAesonLensMb "url" cRest

-- | Create a @Collection@ with an @items@ and a @url@ property
--   and fill in the corresponding @totalItems@ field with the
--   length of the @items@ array.
makeCollection :: [Object] -> Text -> Collection
makeCollection objs url = Collection
  $ HM.insert "totalItems" (toJSON (length objs))
  $ HM.insert "items"      (toJSON objs)
  $ HM.insert "url"        (toJSON url)
  $ HM.empty
