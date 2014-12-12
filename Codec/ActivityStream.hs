{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Codec.ActivityStream
Description : The basic Activity Streams structures
Copyright   : (c) Getty Ritter, 2014
Maintainer  : gdritter@galois.com

This is an interface to ActivityStreams that simply wraps an underlying
@aeson@ Object, and exposes a set of convenient lenses to access the
values inside. If an @aeson@ object appears wrapped in some respective wrapper,
it will necessarily contain the obligatory values for that type
(e.g. an 'Activity' is guaranteed to have a @published@ date.)

Most of the inline documentation is drawn directly from the
<http://activitystrea.ms/specs/json/1.0/ JSON Activity Streams 1.0>
specification, with minor modifications
to refer to the corresponding data types in this module and to clarify
certain aspects.
-}

module Codec.ActivityStream
  ( module Codec.ActivityStream.Representation
  ) where

import Codec.ActivityStream.Representation
