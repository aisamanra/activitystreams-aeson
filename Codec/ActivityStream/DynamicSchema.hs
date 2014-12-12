{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Codec.ActivityStream.DynamicSchema
Description : A (more dynamic) interface to the Activity Streams Base Schema
Copyright   : (c) Getty Ritter, 2014
Maintainer  : gdritter@galois.com

This is an interface to the extended ActivityStreams schema which defines
an extensive set of @verb@ values, additional @objectType@ values, and a
set of extended properties for 'Object's.

Most of the inline documentation is drawn directly from the
<https://github.com/activitystreams/activity-schema/blob/master/activity-schema.md Activity Base Schema draft>
specification, with minor modifications
to refer to the corresponding data types in this module and to clarify
certain aspects. This is not an approved draft, and as such may be
subject to changes which will be reflected in this module. In contrast to
"Codec.ActivityStream", the API in this module makes __no guarantees about
long-term stability__.
-}

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
  , evAttendedBy
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
  , oTags
    -- * Mood
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

-- | The URL-Safe Base64-encoded representation of the binary data
bnData :: Lens' Object (Maybe Text)
bnData = makeAesonLensMb "data" oRest
-- | An optional IRI for the binary data described by this object.
bnFileUrl :: Lens' Object (Maybe Text)
bnFileUrl = makeAesonLensMb "fileUrl" oRest

-- | The total number of unencoded, uncompressed octets contained
-- within the "data" field.
bnLength :: Lens' Object (Maybe Text)
bnLength = makeAesonLensMb "length" oRest

-- | An optional MD5 checksum calculated over the unencoded,
-- uncompressed octets contained within the "data" field
bnMd5 :: Lens' Object (Maybe Text)
bnMd5 = makeAesonLensMb "md5" oRest

-- | The MIME Media Type of the binary data contained within the object.
bnMimeType :: Lens' Object (Maybe Text)
bnMimeType = makeAesonLensMb "mimeType" oRest

-- event

-- | A collection object as defined in Section 3.5 of the JSON
-- Activity Streams specification that provides information about
-- entities that attended the event.
evAttendedBy :: Lens' Object (Maybe Collection)
evAttendedBy = makeAesonLensMb "attendedBy" oRest

-- | A collection object as defined in Section 3.5 of the JSON
-- Activity Streams specification that provides information about
-- entities that intend to attend the event.
evAttending :: Lens' Object (Maybe Collection)
evAttending = makeAesonLensMb "attending" oRest

-- | The date and time that the event ends represented as a String
-- conforming to the "date-time" production in [RFC3339].
evEndTime :: Lens' Object (Maybe DateTime)
evEndTime = makeAesonLensMb "endTime" oRest

-- | A collection object as defined in Section 3.5 of the JSON
-- Activity Streams specification that provides information about
-- entities that have been invited to the event.
evInvited :: Lens' Object (Maybe Collection)
evInvited = makeAesonLensMb "invited" oRest

-- | A collection object as defined in Section 3.5 of the JSON
-- Activity Streams specification that provides information about
-- entities that possibly may attend the event.
evMaybeAttending :: Lens' Object (Maybe Collection)
evMaybeAttending = makeAesonLensMb "maybeAttending" oRest

-- | A collection object as defined in Section 3.5 of the JSON
-- Activity Streams specification that provides information about
-- entities that did not attend the event.
evNotAttendedBy :: Lens' Object (Maybe Collection)
evNotAttendedBy = makeAesonLensMb "notAttendedBy" oRest

-- | A collection object as defined in Section 3.5 of the JSON
-- Activity Streams specification that provides information about
-- entities that do not intend to attend the event.
evNotAttending :: Lens' Object (Maybe Collection)
evNotAttending = makeAesonLensMb "notAttending" oRest

-- | The date and time that the event begins represented as a String
-- confirming to the "date-time" production in RFC 3339.
evStartTime :: Lens' Object (Maybe DateTime)
evStartTime = makeAesonLensMb "startTime" oRest

-- issue

-- | An array of one or more absolute IRI's that describe the type of
-- issue represented by the object. Note that the IRI's are intended
-- for use as identifiers and MAY or MAY NOT be dereferenceable.
isTypes :: Lens' Object (Maybe [Text])
isTypes = makeAesonLensMb "types" oRest

-- permission

-- | A single Activity Streams Object, of any objectType, that
-- identifies the scope of the permission. For example, if the
-- permission objects describes write permissions for a given file,
-- the scope property would be a file object describing that file.
pmScope :: Lens' Object (Maybe Object)
pmScope = makeAesonLensMb "scope" oRest

-- | An array of Strings that identify the specific actions associated
-- with the permission. The actions are application and scope
-- specific. No common, core set of actions is defined by this
-- specification.
pmActions :: Lens' Object (Maybe [Text])
pmActions = makeAesonLensMb "actions" oRest

-- place

-- | The latitude, longitude and altitude of the place as a point on
-- Earth. Represented as a JSON Object as described below.
plPosition :: Lens' Object (Maybe PlacePosition)
plPosition = makeAesonLensMb "position" oRest

-- | A physical address represented as a JSON object as described below.
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

-- | An optional Activity Streams Collection object listing the
-- members of a group, or listing the entities assigned to a
-- particular role.
rlMembers :: Lens' Object (Maybe [Object])
rlMembers = makeAesonLensMb "members" oRest

-- Task

-- | An Activity Streams Object that provides information about the
-- actor that is expected to complete the task.
tsActor :: Lens' Object (Maybe Object)
tsActor = makeAesonLensMb "actor" oRest

-- | A RFC 3339 date-time specifying the date and time by which the
-- task is to be completed.
tsBy :: Lens' Object (Maybe DateTime)
tsBy = makeAesonLensMb "by" oRest

-- | An Activity Streams object describing the object of the task.
tsObject :: Lens' Object (Maybe Object)
tsObject = makeAesonLensMb "object" oRest

-- | An Array of other Task objects that are to be completed before
-- this task can be completed.
tsPrerequisites :: Lens' Object (Maybe [Object])
tsPrerequisites = makeAesonLensMb "prerequisites" oRest

-- | A boolean value indicating whether completion of this task is
-- considered to be mandatory.
tsRequired :: Lens' Object (Maybe Bool)
tsRequired = makeAesonLensMb "required" oRest

-- | An Array of other Task objects that are superseded by this task object.
tsSupersedes :: Lens' Object (Maybe [Object])
tsSupersedes = makeAesonLensMb "supersedes" oRest

-- | A string indicating the verb for this task as defined in Section
-- 3.2 of [activitystreams].
tsVerb :: Lens' Object (Maybe SchemaVerb)
tsVerb = makeAesonLensMb "verb" oRest

-- extra properties

-- | The additional @context@ property allows an 'Activity' to further
-- include information about why a particular action occurred by
-- providing details about the context within which a particular
-- Activity was performed. The value of the @context@ property is an
-- 'Object' of any @objectType@. The meaning of the @context@ property is
-- only defined when used within an 'Activity' object.
acContext :: Lens' Activity (Maybe Object)
acContext = makeAesonLensMb "context" acRest

-- | When appearing within an activity, the location data indicates
-- the location where the activity occurred. When appearing within an
-- object, the location data indicates the location of that object at
-- the time the activity occurred.
getLocation :: Lens' a Aeson.Object -> Lens' a (Maybe Object)
getLocation = makeAesonLensMb "location"

-- | Mood describes the mood of the user when the activity was
-- performed. This is usually collected via an extra field in the user
-- interface used to perform the activity. For the purpose of the
-- schema, a mood is a freeform, short mood keyword or phrase along
-- with an optional mood icon image.
oMood :: Lens' Object (Maybe Mood)
oMood = makeAesonLensMb "mood" oRest

-- | A rating given as a number between 1.0 and 5.0 inclusive with one
-- decimal place of precision. Represented in JSON as a property
-- called @rating@ whose value is a JSON number giving the rating.
oRating :: Lens' Object (Maybe Double)
oRating = makeAesonLensMb "rating" oRest

-- | The @result@ provides a description of the result of any particular
-- activity. The value of the @result@ property is an Object of any
-- objectType. The meaning of the @result@ property is only defined when
-- used within an 'Activity' object.
acResult :: Lens' Activity (Maybe Object)
acResult = makeAesonLensMb "result" acRest

-- | The @source@ property provides a reference to the original source of
-- an object or activity. The value of the @source@ property is an
-- Object of any objectType.
--
-- The @source@ property is closely related to
-- the @generator@ and @provider@ properties but serves the distinct
-- purpose of identifying where the activity or object was originally
-- published as opposed to identifying the applications that generated
-- or published it.
getSource :: Lens' a Aeson.Object -> Lens' a (Maybe Object)
getSource = makeAesonLensMb "source"

-- | When an long running Activity occurs over a distinct period of
-- time, or when an Object represents a long-running process or event,
-- the @startTime@ propertiy can be used to specify the
-- date and time at which the activity or object begins.
-- The values for each are represented as JSON Strings
-- conforming to the "date-time" production in RFC3339.
getStartTime :: Lens' a Aeson.Object -> Lens' a (Maybe Text)
getStartTime = makeAesonLensMb "startTime"

-- | When an long running Activity occurs over a distinct period of
-- time, or when an Object represents a long-running process or event,
-- the @endTime@ propertiy can be used to specify the
-- date and time at which the activity or object concludes.
-- The values for each are represented as JSON Strings
-- conforming to the "date-time" production in RFC3339.
getEndTime :: Lens' a Aeson.Object -> Lens' a (Maybe Text)
getEndTime = makeAesonLensMb "endTime"

-- | A listing of the objects that have been associated with a
-- particular object. Represented in JSON using a property named @tags@
-- whose value is an Array of objects.
oTags :: Lens' Object (Maybe [Object])
oTags = makeAesonLensMb "tags" oRest

-- mood

-- | Mood describes the mood of the user when the activity was
-- performed. This is usually collected via an extra field in the user
-- interface used to perform the activity. For the purpose of this
-- schema, a mood is a freeform, short mood keyword or phrase along
-- with an optional mood icon image.
data Mood = Mood { fromMood :: Aeson.Object } deriving (Eq, Show)

instance FromJSON Mood where
  parseJSON (Aeson.Object o)
    |    HM.member "displayName" o
      && HM.member "image" o = return (Mood o)
    | otherwise = fail "..."
  parseJSON _ = fail "..."

instance ToJSON Mood where
  toJSON = Aeson.Object . fromMood

-- | Access to the underlying JSON object of a 'Mood'
moodRest :: Lens' Mood Aeson.Object
moodRest = makeLens fromMood (\ o' m -> m { fromMood = o' })

-- | The natural-language, human-readable and plain-text keyword or
-- phrase describing the mood. HTML markup MUST NOT be included.
moodDisplayName :: Lens' Mood Text
moodDisplayName = makeAesonLens "displayName" moodRest

-- | An optional image that provides a visual representation of the mood.
moodImage :: Lens' Mood MediaLink
moodImage = makeAesonLens "image" moodRest
