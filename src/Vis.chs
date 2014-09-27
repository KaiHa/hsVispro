{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Vis where

import Data.Bits ((.|.))
import Data.Int  (Int64)
import Foreign.C
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable

#include <VisIOB.h>

{# enum define PvType
    { IO_UNKNOWN as PvUnknown
    , IO_INT     as PvInt
    , IO_FLOAT   as PvFloat
    , IO_STRING  as PvString
    } deriving (Eq, Show) #}

{# enum define IoMask
    { IOB_MASK_ACCESS  as IoMaskAccess
    , IOB_MASK_CHANGE  as IoMaskChange
    , IOB_MASK_ICHANGE as IoMaskIChange
    , IOB_MASK_FCHANGE as IoMaskFChange
    , IOB_MASK_SCHANGE as IoMaskSChange
    , IOB_MASK_STATE   as IoMaskState
    , IOB_MASK_CRONO   as IoMaskCrono
    , IOB_MASK_ICRONO  as IoMaskICrono
    , IOB_MASK_FCRONO  as IoMaskFCrono
    , IOB_MASK_SCRONO  as IoMaskSCrono
    , IOB_MASK_CHGOREP as IoMaskChgorep
    , IOB_MASK_WEIGHT  as IoMaskWeight
    , IOB_MASK_ACCNT   as IoMaskAccnt
    , IOB_MASK_ADVANCE as IoMaskAdvance
    } deriving (Eq, Show) #}

{# pointer *IobPV newtype #}
{# pointer IobEventProc #}

{# fun VikWaitAccess as ^
    {      `String'
    , ored `[IoMask]'
    ,      `IobEventProc'
    , id   `(Ptr ())'
    } -> `IobPV' id #}
    where ored = fromIntegral . foldl (\a b -> a .|. fromEnum b) 0

getPvType :: IobPV -> IO PvType
getPvType (IobPV pv) =
    {# get IobPV->type #} pv >>= return . toEnum . fromIntegral

getIntFromPv :: IobPV -> IO Int64
getIntFromPv (IobPV pv) =
    {# get IobPV->u.i.val #} pv >>= return . fromIntegral

getFloatFromPv :: IobPV -> IO Double
getFloatFromPv (IobPV pv) =
    {# get IobPV->u.f.val #} pv >>= \(CDouble a) -> return a

getStringFromPv :: IobPV -> IO String
getStringFromPv (IobPV pv) =
    {# get IobPV->u.s.val #} pv >>= peekCAString

getPv :: String -> IO IobPV
getPv path = vikWaitAccess path [] nullFunPtr nullPtr
