{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Vis where

import Foreign.C

#include <VisIOB.h>

{#enum define PvType
    { IO_UNKNOWN as Unknown
    , IO_INT     as Int
    , IO_FLOAT   as Float
    , IO_STRING  as String
    } deriving (Eq, Ord)
}
