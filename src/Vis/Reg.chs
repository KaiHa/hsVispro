{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Vis.Reg
( Hostname(..)
, Path(..)
, RegObj(..)
, VSet(..)
, regConnect
, regListPaths
, regListVSets
)
where

import Control.Applicative ((<$>))
import Data.Int            (Int8)
import Foreign.C
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Vis.Serv

#include <RegLib.h>

type Name = String
data VSet = VSetString Name String | VSetInt Name Integer | VSetDouble Name Double
          | VSetBool Name Bool | VSetByte Name Int8 | VSetUnknown Name
    deriving (Show, Eq)
data RegObj = RegObj Path [VSet]
    deriving (Show, Eq)

{# enum define VSetType
    { REG_VSET_LONG	   as TypeLong
    , REG_VSET_STRING  as TypeString
    , REG_VSET_DOUBLE  as TypeDouble
    , REG_VSET_BYTE    as TypeByte
    , REG_VSET_BOOL	   as TypeBool
    } deriving (Eq, Show) #}

{# pointer *SkLine  newtype nocode #}
{# pointer *RegPath #}
{# pointer *RegVSet #}

{# fun VrkConnect     as ^ { withHostname* `Hostname', `String' } -> `SkLine' #}
{# fun VrkListAllPath as ^ { `SkLine', withPath* `Path' } -> `RegPath' #}
{# fun VrkReadVSet    as ^ { `SkLine', withPath* `Path' } -> `RegPath' #}
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
    bs <- walk [] a
    vrkFreePath a
    return bs
    where
      walk :: [Path] -> RegPath -> IO [Path]
      walk xs r
        | r == nullPtr = return xs
        | otherwise    = do
                         next <- {# get RegPath->next #} r
                         name <- {# get RegPath->name #} r >>= peekCAString
                         walk (Path name : xs) next


regListVSets :: SkLine -> Path -> IO [RegObj]
regListVSets l p = do
    a  <- vrkListAllPath l p
    bs <- walk [] a
    vrkFreePath a
    return bs
    where
      walk :: [RegObj] -> RegPath -> IO [RegObj]
      walk xs r
        | r == nullPtr = return xs
        | otherwise    = do
                         next  <- {# get RegPath->next #} r
                         name  <- {# get RegPath->name #} r >>= peekCAString
                         vset  <- vrkReadVSet l $ Path name
                         vsets <- {# get RegPath->vset #} vset >>= regGetVSets
                         vrkFreePath vset
                         walk (RegObj (Path name) vsets : xs) next


regGetVSets :: RegVSet -> IO [VSet]
regGetVSets = walk []
  where
    walk :: [VSet] -> RegVSet -> IO [VSet]
    walk xs r
      | r == nullPtr = return xs
      | otherwise    = do
           next  <- {# get RegVSet->next  #} r
           name  <- {# get RegVSet->name  #} r >>= peekCAString
           ftype <- toEnum <$> fromIntegral <$>  {# get RegVSet->ftype #} r
           vset <- case ftype of
               TypeDouble -> (\(CDouble a) -> VSetDouble name a)      <$> {# get RegVSet->v.db        #} r
               TypeBool   -> (\(CChar a)   -> VSetBool name (a /= 0)) <$> {# get RegVSet->v.cval      #} r
               TypeByte   -> (\(CChar a)   -> VSetByte name a )       <$> {# get RegVSet->v.cval      #} r
               TypeLong   -> VSetInt    name . from <$> (peekArray 1  =<< {# get RegVSet->v.la        #} r)
               TypeString -> VSetString name        <$> (peekCAString =<< {# get RegVSet->v.bval.data #} r)
               _          -> return $ VSetUnknown name
           walk (vset : xs) next
           where
             from = fromIntegral . head
