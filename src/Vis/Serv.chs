{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Vis.Serv
( Hostname(..)
, SkLine(..)
, vskInit
)
where

import Foreign.Ptr

#include <VisServ.h>

newtype Hostname = Hostname { unHostname :: String } deriving (Eq, Show)

{# pointer *SkLine   newtype #}

-- XXX do we need this?
vskInit :: IO ()
vskInit = {# call VskInitLoop as ^ #}

