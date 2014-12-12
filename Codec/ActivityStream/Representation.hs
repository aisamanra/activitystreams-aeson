{-# LANGUAGE OverloadedStrings #-}


{-|
Module      : Codec.ActivityStream.Representation
Description : A (more dynamic) interface to Activity Streams
Copyright   : (c) Getty Ritter, 2014
Maintainer  : gdritter@galois.com

This is an interface to ActivityStreams that simply wraps an underlying
@aeson@ Object, and exposes a set of (convenient) lenses to access the
values inside. If an @aeson@ object is wrapped in the respective wrapper,
it will contain the obligatory values for that type (e.g. an 'Activity'
is guaranteed to have a @published@ date.)

Most of the inline documentation is drawn directly from the
<http://activitystrea.ms/specs/json/1.0/ JSON Activity Streams 1.0>
specification, with minor modifications
to refer to the corresponding data types in this module and to clarify
certain aspects.
-}

module Codec.ActivityStream.Representation
       ( Lens'
         -- * Object
       , Object
       , emptyObject
         -- ** Object Lenses
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
         -- * Activity
       , Activity
       , makeActivity
       , asObject
         -- ** Activity Lenses
       , acActor
       , acContent
       , acGenerator
       , acIcon
       , acId
       , acObject
       , acPublished
       , acProvider
       , acTarget
       , acTitle
       , acUpdated
       , acURL
       , acVerb
       , acRest
         -- * MediaLink
       , MediaLink
       , makeMediaLink
         -- ** MediaLink Lenses
       , mlDuration
       , mlHeight
       , mlWidth
       , mlURL
       , mlRest
         -- * Collection
       , Collection
       , makeCollection
         -- ** Collection Lenses
       , cTotalItems
       , cItems
       , cURL
       , cRest
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

import Codec.ActivityStream.Internal (ensure)
import Codec.ActivityStream.LensInternal

-- | Some types of objects may have an alternative visual representation in
--   the form of an image, video or embedded HTML fragments. A 'MediaLink'
--   represents a hyperlink to such resources.
newtype MediaLink = MediaLink { fromMediaLink :: A.Object } deriving (Eq, Show)

instance FromJSON MediaLink where
  parseJSON (A.Object o) = do
    ensure "MediaLink" o ["url"]
    return (MediaLink o)
  parseJSON _ = fail "MediaLink not an object"

instance ToJSON MediaLink where
  toJSON (MediaLink o) = A.Object o

-- | Access the underlying JSON object that represents a Media Link
mlRest :: Lens' MediaLink A.Object
mlRest = makeLens fromMediaLink (\ o' m -> m { fromMediaLink = o' })

-- | A hint to the consumer about the length, in seconds, of the media
--   resource identified by the url property. A media link MAY contain
--   a "duration" property when the target resource is a time-based
--   media item such as an audio or video.
mlDuration :: Lens' MediaLink (Maybe Int)
mlDuration = makeAesonLensMb "duration" mlRest

-- | A hint to the consumer about the height, in pixels, of the media
--   resource identified by the url property. A media link MAY contain
--   a @height@ property when the target resource is a visual media item
--   such as an image, video or embeddable HTML page.
mlHeight :: Lens' MediaLink (Maybe Int)
mlHeight = makeAesonLensMb "height" mlRest

-- | A hint to the consumer about the width, in pixels, of the media
--   resource identified by the url property. A media link MAY contain
--   a @width@ property when the target resource is a visual media item
--   such as an image, video or embeddable HTML page.
mlWidth :: Lens' MediaLink (Maybe Int)
mlWidth = makeAesonLensMb "width" mlRest

-- | The IRI of the media resource being linked. A media link MUST have a
--   @url@ property.
mlURL :: Lens' MediaLink Text
mlURL = makeAesonLens "url" mlRest

-- | Create a @MediaLink@ with just a @url@ property, and all other
--   properties undefined.
makeMediaLink :: Text -> MediaLink
makeMediaLink url = MediaLink (HM.insert "url" (toJSON url) HM.empty)

-- | Within the specification, an 'Object' is a thing, real or
--   imaginary, which participates in an activity. It may be the
--   entity performing the activity, or the entity on which the
--   activity was performed. An object consists of properties
--   defined below. Certain object types may
--   further refine the meaning of these properties, or they may
--   define additional properties.
--
--   To maintain this flexibility in the Haskell environment, an
--   'Object' is an opaque wrapper over an underlying JSON value,
--   and the 'oRest' accessor can be used to access that underlying
--   value.

newtype Object = Object { fromObject :: A.Object } deriving (Eq, Show)

instance FromJSON Object where
  parseJSON (A.Object o) = return (Object o)
  parseJSON _            = fail "Object not an object"

instance ToJSON Object where
  toJSON (Object o) = A.Object o

-- | Access the underlying JSON object that represents an 'Object'
oRest :: Lens' Object A.Object
oRest = makeLens fromObject (\ o' m -> m { fromObject = o' })

-- | A collection of one or more additional, associated objects, similar
--   to the concept of attached files in an email message. An object MAY
--   have an attachments property whose value is a JSON Array of 'Object's.
oAttachments :: Lens' Object (Maybe [Object])
oAttachments = makeAesonLensMb "attachments" oRest

-- | Describes the entity that created or authored the object. An object
--   MAY contain a single author property whose value is an 'Object' of any
--   type. Note that the author field identifies the entity that created
--   the object and does not necessarily identify the entity that
--   published the object. For instance, it may be the case that an
--   object created by one person is posted and published to a system by
--   an entirely different entity.
oAuthor :: Lens' Object (Maybe Object)
oAuthor = makeAesonLensMb "author" oRest

-- | Natural-language description of the object encoded as a single JSON
--   String containing HTML markup. Visual elements such as thumbnail
--   images MAY be included. An object MAY contain a @content@ property.
oContent :: Lens' Object (Maybe Text)
oContent = makeAesonLensMb "content" oRest

-- | A natural-language, human-readable and plain-text name for the
--   object. HTML markup MUST NOT be included. An object MAY contain
--   a @displayName@ property. If the object does not specify an @objectType@
--   property, the object SHOULD specify a @displayName@.
oDisplayName :: Lens' Object (Maybe Text)
oDisplayName = makeAesonLensMb "displayName" oRest

-- | A JSON Array of one or more absolute IRI's
--   <http://www.ietf.org/rfc/rfc3987.txt [RFC3987]> identifying
--   objects that duplicate this object's content. An object SHOULD
--   contain a @downstreamDuplicates@ property when there are known objects,
--   possibly in a different system, that duplicate the content in this
--   object. This MAY be used as a hint for consumers to use when
--   resolving duplicates between objects received from different sources.
oDownstreamDuplicates :: Lens' Object (Maybe [Text])
oDownstreamDuplicates = makeAesonLensMb "downstreamDuplicates" oRest

-- | Provides a permanent, universally unique identifier for the object in
--   the form of an absolute IRI
--   <http://www.ietf.org/rfc/rfc3987.txt [RFC3987]>. An
--   object SHOULD contain a single @id@ property. If an object does not
--   contain an @id@ property, consumers MAY use the value of the @url@
--   property as a less-reliable, non-unique identifier.

oId :: Lens' Object (Maybe Text)
oId = makeAesonLensMb "id" oRest

-- | Description of a resource providing a visual representation of the
--   object, intended for human consumption. An object MAY contain an
--   @image@ property whose value is a 'MediaLink'.
oImage :: Lens' Object (Maybe MediaLink)
oImage = makeAesonLensMb "image" oRest

-- | Identifies the type of object. An object MAY contain an @objectType@
--   property whose value is a JSON String that is non-empty and matches
--   either the "isegment-nz-nc" or the \"IRI\" production in
--   <http://www.ietf.org/rfc/rfc3987.txt [RFC3987]>. Note
--   that the use of a relative reference other than a simple name is
--   not allowed. If no @objectType@ property is contained, the object has
--   no specific type.
oObjectType :: (FromJSON o, ToJSON o) => Lens' Object (Maybe o)
oObjectType = makeAesonLensMb "objectType" oRest

-- | The date and time at which the object was published. An object MAY
--   contain a @published@ property.
oPublished :: Lens' Object (Maybe DateTime)
oPublished = makeAesonLensMb "published" oRest

-- | Natural-language summarization of the object encoded as a single
--   JSON String containing HTML markup. Visual elements such as thumbnail
--   images MAY be included. An activity MAY contain a @summary@ property.
oSummary :: Lens' Object (Maybe Text)
oSummary = makeAesonLensMb "summary" oRest

-- | The date and time at which a previously published object has been
--   modified. An Object MAY contain an @updated@ property.
oUpdated :: Lens' Object (Maybe DateTime)
oUpdated = makeAesonLensMb "updated" oRest

-- | A JSON Array of one or more absolute IRI's
--   <http://www.ietf.org/rfc/rfc3987.txt [RFC3987]> identifying
--   objects that duplicate this object's content. An object SHOULD contain
--   an @upstreamDuplicates@ property when a publisher is knowingly
--   duplicating with a new ID the content from another object. This MAY be
--   used as a hint for consumers to use when resolving duplicates between
--   objects received from different sources.
oUpstreamDuplicates :: Lens' Object (Maybe [Text])
oUpstreamDuplicates = makeAesonLensMb "upstreamDuplicates" oRest

-- | An IRI <http://www.ietf.org/rfc/rfc3987.txt [RFC3987]>
--   identifying a resource providing an HTML representation of the
--   object. An object MAY contain a url property
oURL :: Lens' Object (Maybe Text)
oURL = makeAesonLensMb "url" oRest

-- | Create an @Object@ with no fields.
emptyObject :: Object
emptyObject = Object HM.empty

-- | In its simplest form, an 'Activity' consists of an @actor@, a @verb@, an
--   @object@, and a @target@. It tells the story of a person performing an
--   action on or with an object -- "Geraldine posted a photo to her
--   album" or "John shared a video". In most cases these components
--   will be explicit, but they may also be implied.

newtype Activity = Activity { fromActivity :: A.Object } deriving (Eq, Show)

instance FromJSON Activity where
  parseJSON (A.Object o) = do
    ensure "Activity" o ["published", "provider"]
    return (Activity o)
  parseJSON _ = fail "\"Activity\" not an object"

instance ToJSON Activity where
  toJSON (Activity o) = A.Object o

-- | Access the underlying JSON object that represents an 'Activity'
acRest :: Lens' Activity A.Object
acRest = makeLens fromActivity (\ o' m -> m { fromActivity = o' })

-- | Describes the entity that performed the activity. An activity MUST
--   contain one @actor@ property whose value is a single 'Object'.
acActor :: Lens' Activity Object
acActor = makeAesonLens "actor" acRest

-- | Natural-language description of the activity encoded as a single
--   JSON String containing HTML markup. Visual elements such as
--   thumbnail images MAY be included. An activity MAY contain a
--   @content@ property.
acContent :: Lens' Activity (Maybe Text)
acContent = makeAesonLensMb "content" acRest

-- | Describes the application that generated the activity. An activity
--   MAY contain a @generator@ property whose value is a single 'Object'.
acGenerator :: Lens' Activity (Maybe Object)
acGenerator = makeAesonLens "generator" acRest

-- | Description of a resource providing a visual representation of the
--   object, intended for human consumption. The image SHOULD have an
--   aspect ratio of one (horizontal) to one (vertical) and SHOULD be
--   suitable for presentation at a small size. An activity MAY have
--   an @icon@ property.
acIcon :: Lens' Activity (Maybe MediaLink)
acIcon = makeAesonLensMb "icon" acRest

-- | Provides a permanent, universally unique identifier for the activity
--   in the form of an absolute IRI
--   <http://www.ietf.org/rfc/rfc3987.txt [RFC3987]>. An
--   activity SHOULD contain a single @id@ property. If an activity does
--   not contain an @id@ property, consumers MAY use the value of the
--   @url@ property as a less-reliable, non-unique identifier.
acId :: Lens' Activity (Maybe Text)
acId = makeAesonLensMb "id" acRest

-- | Describes the primary object of the activity. For instance, in the
--   activity, "John saved a movie to his wishlist", the object of the
--   activity is "movie". An activity SHOULD contain an @object@ property
--   whose value is a single 'Object'. If the @object@ property is not
--   contained, the primary object of the activity MAY be implied by
--   context.
acObject :: Lens' Activity (Maybe Object)
acObject = makeAesonLensMb "object" acRest

-- | The date and time at which the activity was published. An activity
--   MUST contain a @published@ property.
acPublished :: Lens' Activity DateTime
acPublished = makeAesonLens "published" acRest

-- | Describes the application that published the activity. Note that this
--   is not necessarily the same entity that generated the activity. An
--   activity MAY contain a @provider@ property whose value is a
--   single 'Object'.
acProvider :: Lens' Activity (Maybe Object)
acProvider = makeAesonLensMb "provider" acRest

-- | Describes the target of the activity. The precise meaning of the
--   activity's target is dependent on the activities verb, but will
--   often be the object the English preposition "to". For instance, in
--   the activity, "John saved a movie to his wishlist", the target of
--   the activity is "wishlist". The activity target MUST NOT be used
--   to identity an indirect object that is not a target of the
--   activity. An activity MAY contain a @target@ property whose value
--   is a single 'Object'.
acTarget :: Lens' Activity (Maybe Object)
acTarget = makeAesonLensMb "target" acRest

-- | Natural-language title or headline for the activity encoded as a
--  single JSON String containing HTML markup. An activity MAY contain
--  a @title@ property.
acTitle :: Lens' Activity (Maybe Text)
acTitle = makeAesonLensMb "title" acRest

-- | The date and time at which a previously published activity has
--   been modified. An Activity MAY contain an @updated@ property.
acUpdated :: Lens' Activity (Maybe DateTime)
acUpdated = makeAesonLensMb "updated" acRest

-- | An IRI <http://www.ietf.org/rfc/rfc3987.txt RFC3987>
--   identifying a resource providing an HTML representation of the
--   activity. An activity MAY contain a @url@ property.
acURL :: Lens' Activity (Maybe Text)
acURL = makeAesonLensMb "url" acRest

-- | Identifies the action that the activity describes. An activity SHOULD
--   contain a verb property whose value is a JSON String that is
--   non-empty and matches either the \"isegment-nz-nc\" or the
--   \"IRI\" production in <http://www.ietf.org/rfc/rfc3987.txt [RFC3987]>.
--   Note that the use of a relative
--   reference other than a simple name is not allowed. If the @verb@ is
--   not specified, or if the value is null, the @verb@ is
--   assumed to be \"post\".
acVerb :: (FromJSON v, ToJSON v) => Lens' Activity (Maybe v)
acVerb = makeAesonLensMb "verb" acRest

-- | Create an @Activity@ with an @actor@, @published@, and
--   @provider@ property.
makeActivity :: Object -> DateTime -> Activity
makeActivity actor published = Activity
  $ HM.insert "actor"     (toJSON actor)
  $ HM.insert "published" (toJSON published)
  $ HM.empty

-- | JSON Activity Streams 1.0 specificies that an @Activity@ may be used as an
--   @Object@. In such a case, the object may have fields permitted on either an
--   @Activity@ or an @Object@
asObject :: Activity -> Object
asObject act = Object (fromActivity act)

-- | A "collection" is a generic list of 'Object's of any object type.
--   The @objectType@ of each item in the collection MAY be omitted if
--   the type of object can be established through context. The collection
--   is used primarily as the root of an Activity Streams document as described
--   in Section 4,
--   but can be used as the value of extension properties in a variety of
--   situations.

newtype Collection = Collection { fromCollection :: A.Object } deriving (Eq, Show)

instance FromJSON Collection where
  parseJSON (A.Object o) = return (Collection o)
  parseJSON _            = fail "\"Collection\" not an object"

instance ToJSON Collection where
  toJSON (Collection o) = A.Object o

-- | Access the underlying JSON object that represents a 'Collection'
cRest :: Lens' Collection A.Object
cRest = makeLens fromCollection (\ o' m -> m { fromCollection = o' })

-- | Non-negative integer specifying the total number of activities
--   within the stream. The Stream serialization MAY contain a
--   @totalItems@ property. (NOTE: there is a typo in the original
--   specification, in which it inconsistently refers to this as
--   either @totalItems@ or @count@.)
cTotalItems :: Lens' Collection (Maybe Int)
cTotalItems = makeAesonLensMb "totalItems" cRest

-- | An array containing a listing of 'Object's of any object type.
--   If used in combination with the @url@ property, the @items@ array
--   can be used to provide a subset of the objects that may be
--   found in the resource identified by the @url@.
cItems :: Lens' Collection (Maybe [Object])
cItems = makeAesonLensMb "items" cRest

-- | An IRI <http://activitystrea.ms/specs/json/1.0/#RFC3987 [RFC3987]>
--   referencing a JSON document containing the full
--   listing of objects in the collection.
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
