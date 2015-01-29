{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Vis.Reg
( Path(..)
, regConnect
)
where

import Foreign.C
import Foreign.Ptr
import Vis.Serv

#include <RegLib.h>

{# pointer *SkLine  newtype nocode #}
{# pointer *RegPath newtype #}

{# fun VrkConnect as ^
    { withHostname* `Hostname', `String' } -> `SkLine' #}

{# fun VrkListPath as ^
    { `SkLine', withPath* `Path' } -> `RegPath' #}

regConnect :: Hostname -> String -> IO (Maybe SkLine)
regConnect h n = do
    l <- vrkConnect h n
    if (l == SkLine nullPtr)
    then return Nothing
    else return $ Just l
