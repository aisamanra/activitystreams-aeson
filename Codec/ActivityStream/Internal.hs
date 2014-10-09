{-# LANGUAGE ViewPatterns #-}

module Codec.ActivityStream.Internal (commonOpts, commonOptsCC) where

import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Data.Text (pack, unpack)
import Network.URI (URI, parseURI)

instance FromJSON URI where
  parseJSON (String ((parseURI . unpack) -> Just u)) = return u
  parseJSON _ = mzero

instance ToJSON URI where
  toJSON = String . pack . show

toCamelCaseUpper :: String -> String
toCamelCaseUpper = toCamelCase True

toCamelCaseLower :: String -> String
toCamelCaseLower = toCamelCase False

toCamelCase :: Bool -> String -> String
toCamelCase = go
  where go _ ""    = ""
        go _ ('-':cs)   = go True cs
        go True (c:cs)  = toUpper c : go False cs
        go False (c:cs) = c : go False cs

fromCamelCase :: String -> String
fromCamelCase (c:cs)
  | isUpper c = toLower c : go cs
  | otherwise = go (c:cs)
  where go "" = ""
        go (c:cs)
          | c == ' '  = go cs
          | isUpper c = '-' : toLower c : go cs
          | otherwise = c : go cs

commonOpts :: String -> Options
commonOpts prefix = defaultOptions
  { fieldLabelModifier = drop (length prefix)
  , omitNothingFields  = True
  }

commonOptsCC :: String -> Options
commonOptsCC prefix = defaultOptions
  { fieldLabelModifier     = fromCamelCase . drop (length prefix)
  , constructorTagModifier = fromCamelCase
  , omitNothingFields      = True
  }
