{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Vis.Serv
( Hostname(..)
, Path(..)
, SkLine(..)
, withHostname
, withPath
)
where

import Foreign.Ptr
import Foreign.C

#include <VisServ.h>

newtype Hostname = Hostname { unHostname :: String } deriving (Eq, Show)
newtype Path     = Path     { unPath     :: String } deriving (Eq, Show)

{# pointer *SkLine newtype #} deriving (Eq)

withHostname a = withCString (unHostname a)
withPath     a = withCString (unPath a)
