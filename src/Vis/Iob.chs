{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Vis.Iob
( Hostname(..)
, IoMask
, iobConnect
, iobGetValue
, iobSubscribeValue
)
where

import Control.Applicative ((<$>))
import Data.Bits ((.|.))
import Data.Int  (Int64)
import Data.Word (Word64)
import Foreign.C
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

#include <VisIOB.h>
#include <VisServ.h>

data IobValue = IobInt Int64 | IobFloat Double | IobString String | Error String
    deriving (Show)

type IobState = [Word64]

newtype Hostname = Hostname { unHostname :: String } deriving (Eq, Show)

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

ored :: Integral a => [IoMask] -> a
ored = fromIntegral . foldl (\a b -> a .|. fromEnum b) 0

{# pointer *IobPV    newtype #}
{# pointer *IOBEvent newtype #}
{# pointer *SkLine   newtype #}
type IobEventProc = Ptr () ->  IobPV -> Ptr () -> IO ()
foreign import ccall "wrapper"
    mkIobEventProc :: IobEventProc -> IO (FunPtr IobEventProc)

updateValue :: Ptr () ->  IobPV -> Ptr () -> IO ()
updateValue _ pv _ = unwrapValue pv >>= putStrLn . show

{# fun VikConnect as iobConnect
    { toCStr* `Hostname', `String' } -> `SkLine' #}
    where toCStr a = withCString (unHostname a)

{# fun VikWaitAccess as ^
    {      `String'
    , ored `[IoMask]'
    , id   `FunPtr IobEventProc'
    , id   `(Ptr ())'
    } -> `IobPV' id #}

{# fun VikAccess as ^
    {      `String'
    , ored `[IoMask]'
    , id   `FunPtr IobEventProc'
    , id   `(Ptr ())'
    } -> `IobPV' id #}

{# fun VikRelease as ^
    {      `IobPV'
    , ored `[IoMask]'
    , id   `FunPtr IobEventProc'
    , id   `(Ptr ())'
    } -> `()' id #}

step :: IO ()
step = {# call VskStep as ^ #}

-- XXX do we need this?
init :: IO ()
init = {# call VskInitLoop as ^ #}

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

getStateFromPV :: IobPV -> IO [Word64]
getStateFromPV (IobPV pv) =
    {# get IobPV->state #} pv >>= peekArray 3 >>= return . map fromIntegral

-- |Request a value from the IOBase server.
iobGetValue :: String -> IO (IobValue, IobState)
iobGetValue path = do
    step -- XXX where to put this?
    pv  <- vikWaitAccess path mask nullFunPtr nullPtr
    val <- unwrapValue pv
    state <- getStateFromPV pv
    vikRelease pv mask nullFunPtr nullPtr
    return (val, state)
    where mask    = [IoMaskChange]

-- |Subscribe to a value from the IOBase server.
iobSubscribeValue :: String -> IobEventProc -> IO ()
iobSubscribeValue path callback = do
    fp <- mkIobEventProc callback -- XXX when should we freeHaskellFunPtr
    pv <- vikAccess path mask fp nullPtr
--    vikRelease pv mask fp nullPtr
--    freeHaskellFunPtr fp
    return ()
    where mask    = [IoMaskChange]

-- XXX Is the IO monad necessary
unwrapValue :: IobPV -> IO IobValue
unwrapValue pv =
    getPvType pv >>=
    \a -> case a of
        PvInt     -> IobInt    <$> getIntFromPv pv
        PvFloat   -> IobFloat  <$> getFloatFromPv pv
        PvString  -> IobString <$> getStringFromPv pv
        otherwise -> return $ Error "Unknown PV type returned"
