{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Vis.Reg
where

import Foreign.C
import Foreign.Ptr
import Vis.Serv

#include <RegLib.h>

{# pointer *SkLine newtype nocode #}

{# fun VrkConnect as ^
    { toCStr* `Hostname', `String' } -> `SkLine' #}
    where toCStr a = withCString (unHostname a)
