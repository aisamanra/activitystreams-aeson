{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Codec.ActivityStream.Representation where

import           Control.Applicative
import           Control.Lens hiding ((.=))
import           Data.Aeson ( FromJSON(..)
                            , ToJSON(..)
                            , Value
                            , fromJSON
                            , object
                            , (.=)
                            , (.:)
                            , (.:?)
                            )
import qualified Data.Aeson as Ae
import           Data.Aeson.TH
import           Data.DateTime
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (catMaybes)
import           Data.Text (Text)

import Codec.ActivityStream.Internal

data Verb ext
  = Post
  | VerbExt ext
    deriving (Eq, Show)

instance FromJSON ext => FromJSON (Verb ext) where
  parseJSON (Ae.String "post") = return Post
  parseJSON ext                = VerbExt `fmap` parseJSON ext

instance ToJSON ext => ToJSON (Verb ext) where
  toJSON Post          = Ae.String "post"
  toJSON (VerbExt ext) = toJSON ext

data MediaLink = MediaLink
  { _mlDuration :: Maybe Int
  , _mlHeight   :: Maybe Int
  , _mlURL      :: Text
  , _mlWidth    :: Maybe Int
  } deriving (Eq, Show)

makeLenses ''MediaLink
deriveJSON (commonOpts "_ml") ''MediaLink

data Object objType = Object
  { _oAttachments          :: Maybe [Object objType]
  , _oAuthor               :: Maybe (Object objType)
  , _oContent              :: Maybe Text
  , _oDisplayName          :: Maybe Text
  , _oDownstreamDuplicates :: Maybe [Text]
  , _oId                   :: Maybe Text
  , _oImage                :: Maybe MediaLink
  , _oObjectType           :: Maybe objType
  , _oPublished            :: Maybe DateTime
  , _oSummary              :: Maybe Text
  , _oUpdated              :: Maybe DateTime
  , _oUpstreamDuplicates   :: Maybe [Text]
  , _oURL                  :: Maybe Text
  , _oRest                 :: [(Text, Value)]
  } deriving (Eq, Show)

makeLenses ''Object

objectFields :: [Text]
objectFields =
  [ "attachments"
  , "author"
  , "content"
  , "displayName"
  , "downstreamDuplicates"
  , "id"
  , "image"
  , "objectType"
  , "published"
  , "summary"
  , "updated"
  , "upstreamDuplicates"
  , "url"
  ]

instance FromJSON objType => FromJSON (Object objType) where
  parseJSON (Ae.Object o) =
    Object <$> o .:? "attachments"
           <*> o .:? "author"
           <*> o .:? "content"
           <*> o .:? "displayName"
           <*> o .:? "downstreamDuplicates"
           <*> o .:? "id"
           <*> o .:? "image"
           <*> o .:? "objectType"
           <*> o .:? "published"
           <*> o .:? "summary"
           <*> o .:? "updated"
           <*> o .:? "upstreamDuplicates"
           <*> o .:? "url"
           <*> pure rest
    where rest = HM.toList (foldr HM.delete o objectFields)

instance ToJSON objType => ToJSON (Object objType) where
  toJSON obj = object (attrs ++ _oRest obj)
    where attrs = catMaybes
            [ "attachments"          .=? _oAttachments obj
            , "author"               .=? _oAuthor obj
            , "content"              .=? _oContent obj
            , "displayName"          .=? _oDisplayName obj
            , "downstreamDuplicates" .=? _oDownstreamDuplicates obj
            , "id"                   .=? _oId obj
            , "image"                .=? _oImage obj
            , "objectType"           .=? _oObjectType obj
            , "published"            .=? _oPublished obj
            , "summary"              .=? _oSummary obj
            , "updated"              .=? _oUpdated obj
            , "upstreamDuplicates"   .=? _oUpstreamDuplicates obj
            , "url"                  .=? _oURL obj
            ]
          (.=?) :: ToJSON a => Text -> Maybe a -> Maybe (Text, Value)
          x .=? Just y  = Just (x, toJSON y)
          _ .=? Nothing = Nothing
          infix 1 .=?

emptyObject :: Object objType
emptyObject = Object
  { _oAttachments          = Nothing
  , _oAuthor               = Nothing
  , _oContent              = Nothing
  , _oDisplayName          = Nothing
  , _oDownstreamDuplicates = Nothing
  , _oId                   = Nothing
  , _oImage                = Nothing
  , _oObjectType           = Nothing
  , _oPublished            = Nothing
  , _oSummary              = Nothing
  , _oUpdated              = Nothing
  , _oUpstreamDuplicates   = Nothing
  , _oURL                  = Nothing
  , _oRest                 = []
  }

data Activity verb objType = Activity
  { _acActor     :: Object objType
  , _acContent   :: Maybe Text
  , _acGenerator :: Maybe (Object objType)
  , _acIcon      :: Maybe MediaLink
  , _acId        :: Maybe Text
  , _acPublished :: DateTime
  , _acProvider  :: Object objType
  , _acTarget    :: Maybe (Object objType)
  , _acTitle     :: Maybe Text
  , _acUpdated   :: Maybe DateTime
  , _acURL       :: Maybe Text
  , _acVerb      :: Maybe verb
  } deriving (Eq, Show)

makeLenses ''Activity
deriveJSON (commonOpts "_ac") ''Activity

makeMinimalActivity :: Object objType -> DateTime -> Object objType
                       -> Activity verb objType
makeMinimalActivity actor published provider = Activity
  { _acActor     = actor
  , _acContent   = Nothing
  , _acGenerator = Nothing
  , _acIcon      = Nothing
  , _acId        = Nothing
  , _acPublished = published
  , _acProvider  = provider
  , _acTarget    = Nothing
  , _acTitle     = Nothing
  , _acUpdated   = Nothing
  , _acURL       = Nothing
  , _acVerb      = Nothing
  }

data Collection objType = Collection
  { _cTotalItems :: Maybe Int
  , _cItems      :: Maybe [Object objType]
  , _cURL        :: Maybe Text
  } deriving (Eq, Show)

makeLenses ''Collection
deriveJSON (commonOpts "_c") ''Collection

makeCollection :: Maybe [Object objType] -> Maybe Text -> Collection objType
makeCollection objs url = Collection
  { _cTotalItems = fmap length objs
  , _cItems      = objs
  , _cURL        = url
  }
