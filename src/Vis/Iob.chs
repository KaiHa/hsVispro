{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Vis.Iob
( Hostname(..)
, IobEventType(..)
, IobMask
, IobState(..)
, IobValue(..)
, Path(..)
, PvState(..)
, ReturnCode(..)
, TimeSpan(..)
, iobAccess
, iobConnect
, iobGetValue
, iobRelease
, iobPulseValue
, iobSetValue
, iobStep
, iobSubscribeValue
, getPathFromPv
, getValueFromPv
)
where

import Control.Applicative ((<$>))
import Control.Monad       (void, when)
import Data.Bits ((.|.), (.&.))
import Data.Int  (Int64)
import Data.Word (Word32, Word64)
import Foreign.C
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Vis.Serv

#include <VisIOB.h>

data IobValue = IobInt Int64 | IobFloat Double | IobString String | IobUnknownType
    deriving (Show, Eq, Ord)

data IobState = IobState [PvState] Word32 Word32 deriving (Show, Eq, Ord)

data PvHandle = PvHandle IobPV [IobMask] (FunPtr IobEventProc) (Ptr ())
    deriving (Show, Eq, Ord)

data ReturnCode = Success | ErrorCode CInt deriving (Show, Eq, Ord)

data UserWord = UserWord1 | UserWord2 deriving (Show, Eq, Ord)

data TimeSpan = Milliseconds Word64 deriving (Show, Eq, Ord)

{# enum define PvType
    { IO_UNKNOWN as PvUnknown
    , IO_INT     as PvInt
    , IO_FLOAT   as PvFloat
    , IO_STRING  as PvString
    } deriving (Eq, Show) #}

{# enum define IobMask
    { IOB_MASK_ACCESS  as IobMaskAccess
    , IOB_MASK_CHANGE  as IobMaskChange
    , IOB_MASK_ICHANGE as IobMaskIChange
    , IOB_MASK_FCHANGE as IobMaskFChange
    , IOB_MASK_SCHANGE as IobMaskSChange
    , IOB_MASK_STATE   as IobMaskState
    , IOB_MASK_CRONO   as IobMaskCrono
    , IOB_MASK_ICRONO  as IobMaskICrono
    , IOB_MASK_FCRONO  as IobMaskFCrono
    , IOB_MASK_SCRONO  as IobMaskSCrono
    , IOB_MASK_CHGOREP as IobMaskChgorep
    , IOB_MASK_WEIGHT  as IobMaskWeight
    , IOB_MASK_ACCNT   as IobMaskAccnt
    , IOB_MASK_ADVANCE as IobMaskAdvance
    } deriving (Eq, Show, Ord) #}

{# enum define IobEventType
    { IOB_ACCESS         as IobEventAccess
    , IOB_NOACCESS       as IobEventNoAccess
    , IOB_CHANGE_INT     as IobEventChangeInt
    , IOB_CHANGE_FLOAT   as IobEventChangeFloat
    , IOB_CHANGE_STRG    as IobEventChangeStrg
    , IOB_STATE          as IobEventState
    , IOB_RECONF         as IobEventReconf
    , IOB_NEWNAME        as IobEventNewName
    , IOB_CLEAR_RESPONSE as IobEventClearResponse
    , IOB_CRONO_INT      as IobEventCronoInt
    , IOB_CRONO_FLOAT    as IobEventCronoFloat
    , IOB_CRONO_STRG     as IobEventCronoStrg
    , IOB_CHGOREP_INT    as IobEventChGoRepInt
    , IOB_CHGOREP_FLOAT  as IobEventChGoRepFloat
    , IOB_CHGOREP_STRG   as IobEventChGoRepStrg
    , IOB_SET_MASTER     as IobEventSetMaster
    , IOB_CHANGE_WEIGHT  as IobEventChangeWeight
    , IOB_CHANGE_ACCNT   as IobEventChangeAccnt
    , IOB_INFOREQ        as IobEventInfoReq
    , IOB_LIST_FORCED    as IobEventListForced
    , IOB_LIST_ALL       as IobEventListAll
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

ored :: Integral a => [IobMask] -> a
ored = fromIntegral . foldl (\a b -> a .|. fromEnum b) 0

{# pointer *IobPV    newtype  #} deriving (Eq, Ord, Show)
{# pointer *IOBEvent #}
{# pointer *SkLine   newtype nocode #}
type IobEventProc = Ptr () ->  IobPV -> IOBEvent -> IO ()
foreign import ccall "wrapper"
    mkIobEventProc :: IobEventProc -> IO (FunPtr IobEventProc)


{# fun VikConnect as ^
    { withHostname* `Hostname', `String' } -> `SkLine' #}

{# fun VikWaitAccess as ^
    { withPath* `Path'
    , ored      `[IobMask]'
    , id        `FunPtr IobEventProc'
    , id        `(Ptr ())'
    } -> `IobPV' id #}

{# fun VikAccess as ^
    { withPath* `Path'
    , ored      `[IobMask]'
    , id        `FunPtr IobEventProc'
    , id        `(Ptr ())'
    } -> `IobPV' id #}

{# fun VikRelease as ^
    {      `IobPV'
    , ored `[IobMask]'
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

{# fun VikSetPulse as ^
    { `IobPV'
    , `String'
    , `CULong'
    , `String'
    } -> `CInt' id #}

iobStep :: IO ()
iobStep = {# call VskStep as ^ #}

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

-- |Extract the name from an IobPv.
getPathFromPv :: IobPV -> IO Path
getPathFromPv pv = Path <$> (peekCAString =<< {# get IobPV->name #} pv)

-- |A connect is usually not necessary.
iobConnect :: Hostname -> String -> IO (Maybe SkLine)
iobConnect h n = do
    l <- vikConnect h n
    if (l == SkLine nullPtr)
    then return Nothing
    else return $ Just l


-- |Request a value from the IOBase server.
iobGetValue :: Path -> IO (IobValue, IobState)
iobGetValue path = do
    iobStep -- XXX where to put this?
    pv <- vikWaitAccess path [] nullFunPtr nullPtr
    val <- getValueFromPv pv
    state <- getStateFromPV pv
    vikRelease pv [] nullFunPtr nullPtr
    return (val, state)


-- |Get a PV handle from the IOBase server.
-- You must free the 'PvHandle' with 'iobRelease' if you no longer need it.
iobAccess:: Path -> IO PvHandle
iobAccess path = do
    iobStep -- XXX where to put this?
    pv <- vikAccess path [] nullFunPtr nullPtr
    return (PvHandle pv [] nullFunPtr nullPtr)


-- |Release a PvHandle.
iobRelease :: PvHandle -> IO ()
iobRelease (PvHandle pv mask fp arg) = do
    vikRelease pv mask fp arg
    when (fp /= nullFunPtr) $ freeHaskellFunPtr fp
    iobStep -- XXX where to put this?


-- |Write a value to the IOBase server.
iobSetValue :: PvHandle -> IobValue -> IO ReturnCode
iobSetValue (PvHandle pv _ _ _) val = do
    err <- set val
    iobStep
    return $ cintToReturnCode err
    where
        set (IobString v) = vikSetVal pv v
        set (IobInt    v) = vikSetVal pv $ show v
        set (IobFloat  v) = vikSetVal pv $ show v


-- |Write a value to the IOBase server.
iobPulseValue :: PvHandle -> IobValue -> TimeSpan -> IobValue -> IO ReturnCode
iobPulseValue (PvHandle pv _ _ _) val (Milliseconds ms) val' = do
    err <- pulse val val'
    iobStep
    return $ cintToReturnCode err
    where
        t = fromIntegral ms
        pulse (IobString v) (IobString v') = vikSetPulse pv v        t v'
        pulse (IobInt    v) (IobInt    v') = vikSetPulse pv (show v) t (show v')
        pulse (IobFloat  v) (IobFloat  v') = vikSetPulse pv (show v) t (show v')


-- |Write a user status to the IOBase server.
iobSetState :: PvHandle -> UserWord -> Word32 -> IO ReturnCode
iobSetState (PvHandle pv _ _ _) w val = do
    err <- vikSetUsrState pv n $ CULong val
    iobStep
    return $ cintToReturnCode err
    where toCInt UserWord1 = 0
          toCInt UserWord2 = 1
          n = toCInt w


-- |Subscribe to a value from the IOBase server.
-- You must end the subscription and free the 'PvHandle' with 'iobRelease'.
iobSubscribeValue :: Path -> (IobPV -> IobEventType -> IO ()) -> IO PvHandle
iobSubscribeValue path callback = do
    iobStep
    fp <- mkIobEventProc callback'
    pv <- vikAccess path mask fp nullPtr
    return (PvHandle pv mask fp nullPtr)
    where
      mask    = [IobMaskChange]
      callback' _ a b = do b' <- {# get IOBEvent->type #} b
                           callback a $ toEnum $ fromIntegral b'
