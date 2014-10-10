{-# LANGUAGE TemplateHaskell #-}

module Codec.ActivityStream.Schema where

import Data.Aeson hiding (Object)
import Data.Aeson.TH
import Data.DateTime
import Data.Text (Text)

import Codec.ActivityStream.Internal
import Codec.ActivityStream.Representation

data SchemaVerb
  = Accept -- ^ Indicates that that the actor has accepted the object.
           --   For instance, a person accepting an award, or accepting
           -- an assignment.
  | Access -- ^ Indicates that the actor has accessed the object. For
           --   instance, a person accessing a room, or accessing a file.
  | Acknowledge -- ^ Indicates that the actor has acknowledged the object.
                --   This effectively signals that the actor is aware of the
                --   object's existence.
  | Add -- ^ Indicates that the actor has added the object to the target.
        --   For instance, adding a photo to an album.
  | Agree -- ^ Indicates that the actor agrees with the object. For example,
          --   a person agreeing with an argument, or expressing agreement
          --   with a particular issue.
  | Append -- ^ Indicates that the actor has appended the object to the
           --   target. For instance, a person appending a new record
           --   to a database.
  | Approve -- ^ Indicates that the actor has approved the object. For
            --   instance, a manager might approve a travel request.
  | Archive -- ^ Indicates that the actor has archived the object.
  | Assign -- ^ Indicates that the actor has assigned the object to the target.
  | At -- ^ Indicates that the actor is currently located at the object.
       --   For instance, a person being at a specific physical location.
  | Attach -- ^ Indicates that the actor has attached the object to the
           --   target. For instance, a person attaching a file to a wiki
           --   page or an email.
  | Attend -- ^
  | Author -- ^
  | Authorize -- ^
  | Borrow -- ^
  | Build -- ^
  | Cancel -- ^
  | Close -- ^
  | Complete -- ^
  | Confirm -- ^
  | Consume -- ^
  | Checkin -- ^
  | Create -- ^
  | Delete -- ^
  | Deliver -- ^
  | Deny -- ^
  | Disagree -- ^
  | Dislike -- ^
  | Experience -- ^
  | Favorite -- ^
  | Find -- ^
  | FlagAsInappropriate -- ^
  | Follow -- ^
  | Give -- ^
  | Host -- ^
  | Ignore -- ^
  | Insert -- ^
  | Install -- ^
  | Interact -- ^
  | Invite -- ^
  | Join -- ^
  | Leave -- ^
  | Like -- ^
  | Listen -- ^
  | Lose -- ^
  | MakeFriend -- ^
  | Open -- ^
  | Play -- ^
  | Post -- ^
  | Present -- ^
  | Purchase -- ^
  | Qualify -- ^
  | Read -- ^
  | Receive -- ^
  | Reject -- ^
  | Remove -- ^
  | RemoveFriend -- ^
  | Replace -- ^
  | Request -- ^
  | RequestFriend -- ^
  | Resolve -- ^
  | Return -- ^
  | Retract -- ^
  | RsvpMaybe -- ^
  | RsvpNo -- ^
  | RsvpYes -- ^
  | Satisfy -- ^
  | Save -- ^
  | Schedule -- ^
  | Search -- ^
  | Sell -- ^
  | Send -- ^
  | Share -- ^
  | Sponsor -- ^
  | Start -- ^
  | StopFollowing -- ^
  | Submit -- ^
  | Tag -- ^
  | Terminate -- ^
  | Tie -- ^
  | Unfavorite -- ^
  | Unlike -- ^
  | Unsatisfy -- ^
  | Unsave -- ^
  | Unshare -- ^
  | Update -- ^
  | Use -- ^
  | Watch -- ^
  | Win -- ^ foo
    deriving (Eq, Show, Read)

deriveJSON (commonOptsCC "") ''SchemaVerb

data SchemaObjectType
  = Alert
  | Application
  | Article
  | Audio
  | Badge
  | Binary
  | Bookmark
  | Collection
  | Comment
  | Device
  | Event
  | File
  | Game
  | Group
  | Image
  | Issue
  | Job
  | Note
  | Offer
  | Organization
  | Page
  | Person
  | Place
  | Process
  | Product
  | Question
  | Review
  | Service
  | Task
  | Video
    deriving (Eq, Show, Read)

deriveJSON (commonOptsCC "") ''SchemaObjectType

type SchemaObject = Object SchemaObjectType
type SchemaCollection = Collection SchemaObjectType

data AVObj = AVObj
  { avEmbedCode :: Maybe Text
  , avStream    :: Maybe MediaLink
  , avRest      :: SchemaObject
  } deriving (Eq, Show)

data BinaryObj = BinaryObj
  { bnCompression :: Maybe Text
  , bnData        :: Maybe Text
  , bnFileUrl     :: Maybe Text
  , bnLength      :: Maybe Int
  , bnMd5         :: Maybe Text
  , bnMimeType    :: Maybe Text
  , bnRest        :: SchemaObject
  } deriving (Eq, Show)

data EventObj = EventObj
  { evAttendedBy     :: Maybe SchemaCollection
  , evAttending      :: Maybe SchemaCollection
  , evEndTime        :: Maybe DateTime
  , evInvited        :: Maybe SchemaCollection
  , evMaybeAttending :: Maybe SchemaCollection
  , evNotAttendedBy  :: Maybe SchemaCollection
  , evNotAttending   :: Maybe SchemaCollection
  , evStartTime      :: Maybe DateTime
  , evRest           :: SchemaObject
  } deriving (Eq, Show)

data IssueObj = IssueObj
  { isTypes  :: Maybe [Text]
  , isRest   :: SchemaObject
  } deriving (Eq, Show)

data PlaceObj = PlaceObj
  { plPosition :: Maybe PlacePositionObj
  , plAddress  :: Maybe PlaceAddressObj
  , plRest     :: SchemaObject
  } deriving (Eq, Show)

data PlacePositionObj = PlacePositionObj
  { ppAltitude  :: Integer
  , ppLatitude  :: Integer
  , ppLongitude :: Integer
  } deriving (Eq, Show)

data PlaceAddressObj = PlaceAddressObj
  { paFormatted     :: Text
  , paStreetAddress :: Text
  , paLocality      :: Text
  , paRegion        :: Text
  , paPostalCode    :: Text
  , paCountry       :: Text
  } deriving (Eq, Show)

data TaskObj = TaskObj
  { tsActor         :: Maybe SchemaObject
  , tsBy            :: Maybe DateTime
  , tsObject        :: Maybe SchemaObject
  , tsPrerequisites :: Maybe [TaskObj]
  , tsRequired      :: Maybe Bool
  , tsSupersedes    :: Maybe [TaskObj]
  , tsVerb          :: Maybe SchemaVerb
  , tsRest          :: SchemaObject
  } deriving (Eq, Show)
