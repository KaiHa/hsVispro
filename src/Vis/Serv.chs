{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Vis.Serv
( Hostname(..)
, Path(..)
, SkLine(..)
, withHostname
, withPath
)
where

import Data.String
import Foreign.Ptr
import Foreign.C

#include <VisServ.h>

newtype Hostname = Hostname { unHostname :: String } deriving (Eq, IsString, Show)
newtype Path     = Path     { unPath     :: String } deriving (Eq, IsString, Show)

{# pointer *SkLine newtype #} deriving (Eq)

withHostname a = withCString (unHostname a)
withPath     a = withCString (unPath a)
