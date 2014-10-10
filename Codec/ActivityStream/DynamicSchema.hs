{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}

module Codec.ActivityStream.DynamicSchema
  ( module Codec.ActivityStream.Dynamic
  -- * Verbs
  , SchemaVerb(..)
  -- * Object Types
  , SchemaObjectType(..)
  -- ** Audio/Video
  , avEmbedCode
  , avStream
  -- ** Binary
  , bnCompression
  , bnData
  , bnFileUrl
  , bnLength
  , bnMd5
  , bnMimeType
  -- ** Event
  , evAttended
  , evAttending
  , evEndTime
  , evInvited
  , evMaybeAttending
  , evNotAttendedBy
  , evNotAttending
  , evStartTime
  -- ** Issue
  , isTypes
  -- ** Permission
  , pmScope
  , pmActions
  -- ** Place
  , plPosition
  , plAddress
  -- *** PlacePosition
  , PlacePosition
  -- *** PlaceAddress
  , PlaceAddress
  -- ** Role/Group
  , rlMembers
  -- ** Task
  , tsActor
  , tsBy
  , tsObject
  , tsPrerequisites
  , tsRequired
  , tsSupersedes
  , tsVerb
  -- * Basic Extension Properties
  , acContext
  , getLocation
  , oMood
  , oRating
  , acResult
  , getSource
  , getStartTime
  , getEndTime
  , Mood
  , moodRest
  , moodDisplayName
  , moodImage
  ) where

import qualified Data.Aeson as Aeson
import           Data.DateTime (DateTime)
import           Data.Aeson ( FromJSON(..), ToJSON(..) )
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)

import Codec.ActivityStream.LensInternal
import Codec.ActivityStream.Dynamic
import Codec.ActivityStream.Schema (SchemaVerb(..), SchemaObjectType(..))

-- audio/video

-- | A fragment of HTML markup that, when embedded within another HTML
--   page, provides an interactive user-interface for viewing or listening
--   to the video or audio stream.
avEmbedCode :: Lens' Object (Maybe Text)
avEmbedCode = makeAesonLensMb "embedCode" oRest

-- | An Activity Streams Media Link to the video or audio content itself.
avStream :: Lens' Object (Maybe MediaLink)
avStream = makeAesonLensMb "stream" oRest

-- binary

-- | An optional token identifying a compression algorithm applied to
--   the binary data prior to Base64-encoding. Possible algorithms
--   are "deflate" and "gzip", respectively indicating the use of
--   the compression mechanisms defined by RFC 1951 and RFC 1952.
--   Additional compression algorithms MAY be used but are not defined
--   by this specification. Note that previous versions of this
--   specification allowed for multiple compression algorithms to be
--   applied and listed using a comma-separated format. The use of
--   multiple compressions is no longer permitted.
bnCompression :: Lens' Object (Maybe Text)
bnCompression = makeAesonLensMb "compression" oRest

bnData :: Lens' Object (Maybe Text)
bnData = makeAesonLensMb "data" oRest

bnFileUrl :: Lens' Object (Maybe Text)
bnFileUrl = makeAesonLensMb "fileUrl" oRest

bnLength :: Lens' Object (Maybe Text)
bnLength = makeAesonLensMb "length" oRest

bnMd5 :: Lens' Object (Maybe Text)
bnMd5 = makeAesonLensMb "md5" oRest

bnMimeType :: Lens' Object (Maybe Text)
bnMimeType = makeAesonLensMb "mimeType" oRest

-- event

evAttended :: Lens' Object (Maybe Collection)
evAttended = makeAesonLensMb "attended" oRest

evAttending :: Lens' Object (Maybe Collection)
evAttending = makeAesonLensMb "attending" oRest

evEndTime :: Lens' Object (Maybe DateTime)
evEndTime = makeAesonLensMb "endTime" oRest

evInvited :: Lens' Object (Maybe Collection)
evInvited = makeAesonLensMb "invited" oRest

evMaybeAttending :: Lens' Object (Maybe Collection)
evMaybeAttending = makeAesonLensMb "maybeAttending" oRest

evNotAttendedBy :: Lens' Object (Maybe Collection)
evNotAttendedBy = makeAesonLensMb "notAttendedBy" oRest

evNotAttending :: Lens' Object (Maybe Collection)
evNotAttending = makeAesonLensMb "notAttending" oRest

evStartTime :: Lens' Object (Maybe DateTime)
evStartTime = makeAesonLensMb "startTime" oRest

-- issue

isTypes :: Lens' Object (Maybe [Text])
isTypes = makeAesonLensMb "types" oRest

-- permission

pmScope :: Lens' Object (Maybe Object)
pmScope = makeAesonLensMb "scope" oRest

pmActions :: Lens' Object (Maybe [Text])
pmActions = makeAesonLensMb "actions" oRest

-- place

plPosition :: Lens' Object (Maybe PlacePosition)
plPosition = makeAesonLensMb "position" oRest

plAddress :: Lens' Object (Maybe PlaceAddress)
plAddress = makeAesonLensMb "address" oRest

data PlacePosition = PPO { fromPPO :: Aeson.Object } deriving (Eq, Show)

instance FromJSON PlacePosition where
  parseJSON (Aeson.Object o)
    |    HM.member "altitude" o
      && HM.member "latitude" o
      && HM.member "longitude" o = return (PPO o)
    | otherwise = fail "..."
  parseJSON _ = fail "..."

instance ToJSON PlacePosition where
  toJSON = Aeson.Object . fromPPO

data PlaceAddress = PAO { fromPAO :: Aeson.Object } deriving (Eq, Show)

instance FromJSON PlaceAddress where
  parseJSON (Aeson.Object o)
    |    HM.member "formatted" o
      && HM.member "streetAddress" o
      && HM.member "locality" o
      && HM.member "region" o
      && HM.member "postalCode" o
      && HM.member "country" o = return (PAO o)
    | otherwise = fail "..."
  parseJSON _ = fail "..."

instance ToJSON PlaceAddress where
  toJSON = Aeson.Object . fromPAO

-- role/group

rlMembers :: Lens' Object (Maybe [Object])
rlMembers = makeAesonLensMb "members" oRest

-- Task

tsActor :: Lens' Object (Maybe Object)
tsActor = makeAesonLensMb "actor" oRest

tsBy :: Lens' Object (Maybe DateTime)
tsBy = makeAesonLensMb "by" oRest

tsObject :: Lens' Object (Maybe Object)
tsObject = makeAesonLensMb "object" oRest

tsPrerequisites :: Lens' Object (Maybe [Object])
tsPrerequisites = makeAesonLensMb "prerequisites" oRest

tsRequired :: Lens' Object (Maybe Bool)
tsRequired = makeAesonLensMb "required" oRest

tsSupersedes :: Lens' Object (Maybe [Object])
tsSupersedes = makeAesonLensMb "supersedes" oRest

tsVerb :: Lens' Object (Maybe SchemaVerb)
tsVerb = makeAesonLensMb "verb" oRest

-- extra properties

acContext :: Lens' Activity (Maybe Object)
acContext = makeAesonLensMb "context" acRest

getLocation :: Lens' a Aeson.Object -> Lens' a (Maybe Object)
getLocation = makeAesonLensMb "location"

oMood :: Lens' Object (Maybe Mood)
oMood = makeAesonLensMb "mood" oRest

oRating :: Lens' Object (Maybe Double)
oRating = makeAesonLensMb "rating" oRest

acResult :: Lens' Activity (Maybe Object)
acResult = makeAesonLensMb "result" acRest

getSource :: Lens' a Aeson.Object -> Lens' a (Maybe Object)
getSource = makeAesonLensMb "source"

getStartTime :: Lens' a Aeson.Object -> Lens' a (Maybe Text)
getStartTime = makeAesonLensMb "startTime"

getEndTime :: Lens' a Aeson.Object -> Lens' a (Maybe Text)
getEndTime = makeAesonLensMb "endTime"

-- mood

data Mood = Mood { fromMood :: Aeson.Object } deriving (Eq, Show)

instance FromJSON Mood where
  parseJSON (Aeson.Object o)
    |    HM.member "displayName" o
      && HM.member "image" o = return (Mood o)
    | otherwise = fail "..."
  parseJSON _ = fail "..."

instance ToJSON Mood where
  toJSON = Aeson.Object . fromMood

moodRest :: Lens' Mood Aeson.Object
moodRest = makeLens fromMood (\ o' m -> m { fromMood = o' })

moodDisplayName :: Lens' Mood Text
moodDisplayName = makeAesonLens "displayName" moodRest

moodImage :: Lens' Mood MediaLink
moodImage = makeAesonLens "image" moodRest
