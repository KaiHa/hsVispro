{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Vis.Reg
( Hostname(..)
, Path(..)
, regConnect
, regListPaths
)
where

import Control.Applicative ((<$>))
import Foreign.C
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Vis.Serv

#include <RegLib.h>

{# pointer *SkLine  newtype nocode #}
{# pointer *RegPath #}

{# fun VrkConnect     as ^ { withHostname* `Hostname', `String' } -> `SkLine' #}
{# fun VrkListAllPath as ^ { `SkLine', withPath* `Path' } -> `RegPath' #}
{# fun VrkFreePath    as ^ { `RegPath' } -> `()' #}


regConnect :: Hostname -> String -> IO (Maybe SkLine)
regConnect h n = do
    l <- vrkConnect h n
    if (l == SkLine nullPtr)
    then return Nothing
    else return $ Just l


regListPaths :: SkLine -> Path -> IO [Path]
regListPaths l p = do
    a  <- vrkListAllPath l p
    as <- append [] a
    vrkFreePath a
    return as
    where
      append xs y
        | y == nullPtr = return xs
        | otherwise    = do
                         next <- {# get RegPath->next #} y
                         name <- {# get RegPath->name #} y >>= peekCAString
                         append (Path name : xs) next
