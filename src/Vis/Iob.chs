{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Vis.Iob
( Hostname(..)
, IobValue(..)
, IoMask
, iobAccess
, iobConnect
, iobGetValue
, iobRelease
, iobSetValue
, iobSubscribeValue
, getValueFromPv
)
where

import Control.Applicative ((<$>))
import Control.Monad       (void, when)
import Data.Bits ((.|.), (.&.))
import Data.Int  (Int64)
import Data.Word (Word32)
import Foreign.C
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

#include <VisIOB.h>
#include <VisServ.h>

data IobValue = IobInt Int64 | IobFloat Double | IobString String | IobUnknownType
    deriving (Show, Eq)

data IobState = IobState [PvState] Word32 Word32 deriving (Show, Eq)

data PvHandle = PvHandle IobPV [IoMask] (FunPtr IobEventProc) (Ptr ())

data ReturnCode = Success | ErrorCode CInt deriving (Show, Eq)

data UserWord = UserWord1 | UserWord2 deriving (Show, Eq)

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


{# enum define PvState
    { IF_ERROR        as PvStateError
    , IF_START        as PVStateErrorNotReady
    , IF_NOCONN       as PvStateErrorNoConnection
    , IF_CONNLOST     as PvStateErrorConnectionLost
    , IF_PVLOST       as PvStateErrorPvLost
    , IF_FORCED       as PvStateForced
    , IF_PROVIDED     as PvStateProvided
    , IF_USERPROVIDED as PvStateUSERPROVIDED
    , IF_REPLACE1     as PvStateREPLACE1
    , IF_REPLACE2     as PvStateREPLACE2
    , IF_REPLACE3     as PvStateREPLACE3
    , IF_MBINVALID    as PvStateMBINVALID
    , IF_MBLOWER      as PvStateUnderRange
    , IF_MBUPPER      as PvStateOverRange
    , IF_SAVEINIT     as PvStateSaveInit
    , IF_LICBLK       as PvStateLicBlk
    , IF_REMOTE       as PvStateRemote
    , IF_VOTED        as PvStateVoted
    , IF_PULSE        as PvStatePulse
    , IF_BACKSEND     as PvStateBackSend
    , IF_BINARY       as PvStateBinary
    , IF_RDONLY       as PvStateReadonly
    } deriving (Eq, Show, Ord) #}

-- |Convert an integer into a list of 'PVState'
--
-- >>> toPvState 5
-- [PvStateError,PvStateNoConnection]
toPvState :: Integral a => a -> [PvState]
toPvState n = [x | x <- [PvStateError .. ], (fromEnum x .&. n') /= 0]
    where n' = fromIntegral n

ored :: Integral a => [IoMask] -> a
ored = fromIntegral . foldl (\a b -> a .|. fromEnum b) 0

{# pointer *IobPV    newtype #}
{# pointer *IOBEvent newtype #}
{# pointer *SkLine   newtype #}
type IobEventProc = Ptr () ->  IobPV -> Ptr () -> IO ()
foreign import ccall "wrapper"
    mkIobEventProc :: IobEventProc -> IO (FunPtr IobEventProc)


{# fun VikConnect as ^
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

{# fun VikSetVal as ^
    { `IobPV'
    , `String'
    } -> `CInt' id #}

{# fun VikSetUsrState as ^
    { `IobPV'
    , `CInt'
    , `CULong'
    } -> `CInt' id #}

step :: IO ()
step = {# call VskStep as ^ #}

-- XXX do we need this?
vskInit :: IO ()
vskInit = {# call VskInitLoop as ^ #}

getPvType :: IobPV -> IO PvType
getPvType pv =
    {# get IobPV->type #} pv >>= return . toEnum . fromIntegral

getIntFromPv :: IobPV -> IO Int64
getIntFromPv pv =
    {# get IobPV->u.i.val #} pv >>= return . fromIntegral

getFloatFromPv :: IobPV -> IO Double
getFloatFromPv pv =
    {# get IobPV->u.f.val #} pv >>= \(CDouble a) -> return a

getStringFromPv :: IobPV -> IO String
getStringFromPv pv =
    {# get IobPV->u.s.val #} pv >>= peekCAString

getStateFromPV :: IobPV -> IO IobState
getStateFromPV pv = do
    [a,b,c] <- {# get IobPV->state #} pv >>= peekArray 3
    return $ IobState (toPvState a) (fromIntegral b) (fromIntegral c)

cintToReturnCode :: CInt -> ReturnCode
cintToReturnCode err = if (err == 0) then Success else ErrorCode err

-- |Extract the value from a IobPv.
getValueFromPv :: IobPV -> IO IobValue
getValueFromPv pv =
    getPvType pv >>=
    \a -> case a of
        PvInt     -> IobInt    <$> getIntFromPv pv
        PvFloat   -> IobFloat  <$> getFloatFromPv pv
        PvString  -> IobString <$> getStringFromPv pv
        otherwise -> return $ IobUnknownType


iobConnect :: Hostname -> String -> IO SkLine
iobConnect h n = do
    l <- vikConnect h n
    -- XXX Do we need to do more here?
    return l


-- |Request a value from the IOBase server.
iobGetValue :: String -> IO (IobValue, IobState)
iobGetValue path = do
    step -- XXX where to put this?
    pv <- vikWaitAccess path [] nullFunPtr nullPtr
    val <- getValueFromPv pv
    state <- getStateFromPV pv
    vikRelease pv [] nullFunPtr nullPtr
    return (val, state)


-- |Get a PV handle from the IOBase server.
-- You must free the 'PvHandle' with 'iobRelease' if you no longer need it.
iobAccess:: String -> IO PvHandle
iobAccess path = do
    step -- XXX where to put this?
    pv <- vikAccess path [] nullFunPtr nullPtr
    return (PvHandle pv [] nullFunPtr nullPtr)


-- |Release a PvHandle.
iobRelease :: PvHandle -> IO ()
iobRelease (PvHandle pv mask fp arg) = do
    vikRelease pv mask fp arg
    when (fp /= nullFunPtr) $ freeHaskellFunPtr fp
    step -- XXX where to put this?


-- |Write a value to the IOBase server.
iobSetValue :: PvHandle -> IobValue -> IO ReturnCode
iobSetValue (PvHandle pv _ _ _) val = do
    err <- set val
    step
    return $ cintToReturnCode err
    where
        set (IobString v) = vikSetVal pv v
        set (IobInt    v) = vikSetVal pv $ show v
        set (IobFloat  v) = vikSetVal pv $ show v


-- |Write a user status to the IOBase server.
iobSetState :: PvHandle -> UserWord -> Word32 -> IO ReturnCode
iobSetState (PvHandle pv _ _ _) w val = do
    err <- vikSetUsrState pv n $ CULong val
    step
    return $ cintToReturnCode err
    where toCInt UserWord1 = 0
          toCInt UserWord2 = 1
          n = toCInt w


-- |Subscribe to a value from the IOBase server.
-- You must end the subscription and free the 'PvHandle' with 'iobRelease'.
iobSubscribeValue :: String -> IobEventProc -> IO PvHandle
iobSubscribeValue path callback = do
    step
    fp <- mkIobEventProc callback
    pv <- vikAccess path mask fp nullPtr
    return (PvHandle pv mask fp nullPtr)
    where mask    = [IoMaskChange]
