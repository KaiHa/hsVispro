{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Vis where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable

#include <VisIOB.h>

{# enum define PvType
    { IO_UNKNOWN as PvUnknown
    , IO_INT     as PvInt
    , IO_FLOAT   as PvFloat
    , IO_STRING  as PvString
    } deriving (Eq) #}

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
    } deriving (Eq) #}

{# pointer *IobPV newtype #}
{# pointer IobEventProc newtype #}

{# fun VikWaitAccess as ^
    {    `String'
    ,    `IoMask'
    ,    `IobEventProc'
    , id `(Ptr ())'
    } -> `IobPV' id #}

getPvType :: IobPV -> IO PvType
getPvType (IobPV t) =
    {# get IobPV->type #} t >>= return . toEnum . fromIntegral
