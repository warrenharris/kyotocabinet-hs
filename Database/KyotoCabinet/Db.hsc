{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, DeriveDataTypeable,
             ScopedTypeVariables #-}
{-
Copyright 2011, Google Inc.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above
copyright notice, this list of conditions and the following disclaimer
in the documentation and/or other materials provided with the
distribution.
    * Neither the name of Google Inc. nor the names of its
contributors may be used to endorse or promote products derived from
this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

#include <kclangc.h>

-- | Kyoto Cabinet DB bindings. All IO monad functions can throw KcException.
--
-- This documentation is not a complete description of the Kyoto
-- Cabinet DB interface.  You will need to refer to Kyoto Cabinet DB's
-- C API documentation for the details: <http://fallabs.com/kyotocabinet/>
--
-- Requires Kyoto Cabinet version >= 1.2.48

module Database.KyotoCabinet.Db (
  -- * Types
  KcDb,
  KcCur,
  KcMap,
  KcMapSort,
  KcError(..),
  KcTune(..),
  KcTuneType(..),
  KcLogger(..),
  KcLogLevel(..),
  KcOption(..),
  KcCompressor(..),
  KcComparator(..),
  KcOpenMode(..),
  KcMergeMode(..),
  KcException(..),
  KcVisitAction(..),
  KcVisitFull,
  KcVisitEmpty,
  KcFileProc,

  -- * Utilities
  -- | Most of these routines are probably not needed by haskell users, but are
  -- included for completeness.
  kcversion,
  kcmalloc,
  kcfree,
  kctime,
  kcatoi,
  kcatoix,
  kcatof,
  kchashmurmur,
  kchashfnv,
  kcnan,
  kcinf,
  kcchknan,
  kcchkinf,
  kcecodename,

  -- * KcDb Operations
  kcdbnew,
  kcdbdel,
  kcdbopen,
  kcdbclose,
  kcdbecode,
  kcdbemsg,
  kcdbaccept,
  kcdbacceptbulk,
  kcdbiterate,
  kcdbset,
  kcdbadd,
  kcdbreplace,
  kcdbappend,
  kcdbincrint,
  kcdbincrdouble,
  kcdbcas,
  kcdbremove,
  kcdbget,
  kcdbgetbuf,
  kcdbsetbulk,
  kcdbremovebulk,
  kcdbgetbulk,
  kcdbclear,
  kcdbsync,
  kcdbcopy,
  kcdbbegintran,
  kcdbbegintrantry,
  kcdbendtran,
  kcdbdumpsnap,
  kcdbloadsnap,
  kcdbcount,
  kcdbsize,
  kcdbpath,
  kcdbstatus,
  kcdbmatchprefix,
  kcdbmatchregex,
  kcdbmerge,

  -- * KcCur Operations
  kcdbcursor,
  kccurdel,
  kccuraccept,
  kccurremove,
  kccurgetkey,
  kccurgetvalue,
  kccurget,
  kccurjump,
  kccurjumpkey,
  kccurjumpback,
  kccurjumpbackkey,
  kccurstep,
  kccurstepback,
  kccurdb,
  kccurecode,
  kccuremsg,

  -- * KcMap Operations
  kcmapnew,
  kcmapdel,
  kcmapset,
  kcmapadd,
  kcmapreplace,
  kcmapappend,
  kcmapremove,
  kcmapget,
  kcmapclear,
  kcmapcount,

  -- * KcMapIter Operations
  kcmapiterator,
  kcmapiterdel,
  kcmapitergetkey,
  kcmapitergetvalue,
  kcmapiterget,
  kcmapiterstep,

  -- * KcMapSort Operations
  kcmapsorter,
  kcmapsortdel,
  kcmapsortgetkey,
  kcmapsortgetvalue,
  kcmapsortget,
  kcmapsortstep,

  -- * Scoped Operations
  kcwithdbopen,
  kcwithdbcursor,
  kcwithdbtran,
  kcwithmap,
  kcwithmapiter,
  kcwithmapsort
  ) where

import Control.Applicative
import Control.Exception
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Typeable
import Data.Int
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Prelude hiding (catch)
--import Debug.Trace

--------------------------------------------------------------------------------
-- Structs

-- | Binary string of byte array.
newtype KcStr = KcStr BS.ByteString

instance Storable KcStr where
  sizeOf    _ = (#size KCSTR)
  alignment _ = alignment (undefined :: CSize)
  peek ptr = do
    b <- (#peek KCSTR, buf) ptr
    s <- (#peek KCSTR, size) ptr
    KcStr <$> BS.unsafePackCStringFinalizer b s (kcfree b)
  poke ptr (KcStr bs) = do
    BS.unsafeUseAsCStringLen bs $ \(b, s) -> do
      (#poke KCSTR, buf) ptr b
      (#poke KCSTR, size) ptr s

withKcStrArray :: [BS.ByteString] -> (Ptr KcStr -> IO a) -> IO a
withKcStrArray strs f = do
  allocaArray (length strs) $ \arr -> do
    pokeArray arr (map KcStr strs)
    f arr

-- | Key-Value record.
newtype KcRec = KcRec (BS.ByteString, BS.ByteString)

instance Storable KcRec where
  sizeOf    _ = (#size KCREC)
  alignment _ = alignment (undefined :: KcStr)
  peek ptr = do
    KcStr k <- (#peek KCREC, key) ptr
    KcStr v <- (#peek KCREC, value) ptr
    return $ KcRec (k, v)
  poke ptr (KcRec (k, v)) = do
    (#poke KCREC, key) ptr (KcStr k)
    (#poke KCREC, value) ptr (KcStr v)

withKcRecArray :: [(BS.ByteString, BS.ByteString)] -> (Ptr KcRec -> IO a) -> IO a
withKcRecArray recs f = do
  allocaArray (length recs) $ \arr -> do
    pokeArray arr (map KcRec recs)
    f arr

bsListOfCStringArr :: Ptr CString -> CLLong -> IO [BS.ByteString]
bsListOfCStringArr strarr n = do
  strlist <- peekArray (fromIntegral n) strarr
  bssOfCstrs strlist

bssOfCstrs :: [CString] -> IO [BS.ByteString]
bssOfCstrs [] = return []
bssOfCstrs (h:t) = do
  hLen <- BS.c_strlen h
  bs' <- BS.unsafePackCStringFinalizer (castPtr h) (fromIntegral hLen) (kcfree h)
  rest <- bssOfCstrs t
  return $ bs' `seq` (bs':rest)

bsOfCString :: CString -> Ptr CSize -> IO (Maybe BS.ByteString)
bsOfCString cstr szp =
  if cstr == nullPtr then return Nothing
  else do
    csiz <- peek szp
    bs <- BS.unsafePackCStringFinalizer (castPtr cstr) (fromIntegral csiz)
          (kcfree cstr)
    return $ Just bs

--------------------------------------------------------------------------------
-- Constants

-- | Error codes.
data KcError = KCESUCCESS   -- ^ success
             | KCENOIMPL    -- ^ not implemented
             | KCEINVALID   -- ^ invalid operation
             | KCENOREPOS   -- ^ no repository
             | KCENOPERM    -- ^ no permission
             | KCEBROKEN    -- ^ broken file
             | KCEDUPREC    -- ^ record duplication
             | KCENOREC     -- ^ no record
             | KCELOGIC     -- ^ logical inconsistency
             | KCESYSTEM    -- ^ system error
             | KCEMISC      -- ^ miscellaneous error
             | KCUNKNOWNERROR Int
             deriving (Eq, Show)

kcErrorOfNum :: CInt -> KcError
kcErrorOfNum (#const KCESUCCESS) = KCESUCCESS
kcErrorOfNum (#const KCENOIMPL) = KCENOIMPL
kcErrorOfNum (#const KCEINVALID) = KCEINVALID
kcErrorOfNum (#const KCENOREPOS) = KCENOREPOS
kcErrorOfNum (#const KCENOPERM) = KCENOPERM
kcErrorOfNum (#const KCEBROKEN) = KCEBROKEN
kcErrorOfNum (#const KCEDUPREC) = KCEDUPREC
kcErrorOfNum (#const KCENOREC) = KCENOREC
kcErrorOfNum (#const KCELOGIC) = KCELOGIC
kcErrorOfNum (#const KCESYSTEM) = KCESYSTEM
kcErrorOfNum (#const KCEMISC) = KCEMISC
kcErrorOfNum n = KCUNKNOWNERROR (fromIntegral n)

-- | Open modes.
data KcOpenMode = KCOREADER         -- ^ open as a reader
                | KCOWRITER         -- ^ open as a writer
                | KCOCREATE         -- ^ writer creating
                | KCOTRUNCATE       -- ^ writer truncating
                | KCOAUTOTRAN       -- ^ auto transaction
                | KCOAUTOSYNC       -- ^ auto synchronization
                | KCONOLOCK         -- ^ open without locking
                | KCOTRYLOCK        -- ^ lock without blocking
                | KCONOREPAIR       -- ^ open without auto repair
                deriving (Eq, Show)

numOfKcOpenMode :: KcOpenMode -> Int
numOfKcOpenMode KCOREADER = (#const KCOREADER)
numOfKcOpenMode KCOWRITER = (#const KCOWRITER)
numOfKcOpenMode KCOCREATE = (#const KCOCREATE)
numOfKcOpenMode KCOTRUNCATE = (#const KCOTRUNCATE)
numOfKcOpenMode KCOAUTOTRAN = (#const KCOAUTOTRAN)
numOfKcOpenMode KCOAUTOSYNC = (#const KCOAUTOSYNC)
numOfKcOpenMode KCONOLOCK = (#const KCONOLOCK)
numOfKcOpenMode KCOTRYLOCK = (#const KCOTRYLOCK)
numOfKcOpenMode KCONOREPAIR = (#const KCONOREPAIR)

numOfOpenModes :: [KcOpenMode] -> CUInt
numOfOpenModes mode = fromIntegral $ foldr (\m acc -> numOfKcOpenMode m .|. acc) 0 mode

-- | Merge modes.
data KcMergeMode = KCMSET       -- ^ overwrite the existing value
                 | KCMADD       -- ^ keep the existing value
                 | KCMREPLACE   -- ^ modify the existing record only
                 | KCMAPPEND    -- ^ append the new value
                 deriving (Eq, Show)

numOfKcMergeMode :: KcMergeMode -> Int
numOfKcMergeMode KCMSET = (#const KCMSET)
numOfKcMergeMode KCMADD = (#const KCMADD)
numOfKcMergeMode KCMREPLACE = (#const KCMREPLACE)
numOfKcMergeMode KCMAPPEND = (#const KCMAPPEND)

{- link errors:
-- | The package version.
kcVERSION = #const KCVERSION

-- | Special pointer for no operation by the visiting function.
kcVISNOP = #const KCVISNOP

-- | Special pointer to remove the record by the visiting function.
kcVISREMOVE = #const KCVISREMOVE
-}

-- | Current KC version.
kcversion :: IO String
kcversion = kcVERSION >>= peekCAString

foreign import ccall safe "kclangc.h getKCVERSION" kcVERSION
  :: IO CString

foreign import ccall safe "kclangc.h getKCVISNOP" kcVISNOP
  :: IO CString

foreign import ccall safe "kclangc.h getKCVISREMOVE" kcVISREMOVE
  :: IO CString

-- | Return one of these from 'KcVisitFull', 'KcVisitEmpty' or
-- 'KcFileProc' to update the database after a visitor access.
data KcVisitAction = KCVISNOP | KCVISSET BS.ByteString | KCVISREMOVE

-- | Type of a visitor function when key-value is present.
type KcVisitFull = BS.ByteString -> BS.ByteString -> IO KcVisitAction

callVisitFull :: KcVisitFull -> KCVISITFULL
callVisitFull visitFull kbuf ksiz vbuf vsiz sp _ = do
  key <- BS.unsafePackCStringLen (kbuf, fromIntegral ksiz)
  val <- BS.unsafePackCStringLen (vbuf, fromIntegral vsiz)
  rv <- visitFull key val
  case rv of
    KCVISNOP -> kcVISNOP
    KCVISSET newVal ->
      BS.unsafeUseAsCStringLen newVal $ \(cstr, clen) -> do
        poke sp (fromIntegral clen)
        return cstr -- XXX what if newVal is gc'd?
    KCVISREMOVE -> kcVISREMOVE

type KCVISITFULL =
  CString -> CSize -> CString -> CSize -> Ptr CSize -> Ptr () -> IO CString

foreign import ccall "wrapper" wrapKCVISITFULL
  :: KCVISITFULL -> IO (FunPtr KCVISITFULL)

-- | Type of a visitor function when key-value is absent.
type KcVisitEmpty = BS.ByteString -> IO KcVisitAction

callVisitEmpty :: KcVisitEmpty -> KCVISITEMPTY
callVisitEmpty visitEmpty kbuf ksiz sp _ = do
  key <- BS.unsafePackCStringLen (kbuf, fromIntegral ksiz)
  rv <- visitEmpty key
  case rv of
    KCVISNOP -> kcVISNOP
    KCVISSET newVal ->
      BS.unsafeUseAsCStringLen newVal $ \(cstr, clen) -> do
        poke sp (fromIntegral clen)
        return cstr -- XXX what if newVal is gc'd?
    KCVISREMOVE -> kcVISREMOVE

type KCVISITEMPTY = CString -> CSize -> Ptr CSize -> Ptr () -> IO CString

foreign import ccall "wrapper" wrapKCVISITEMPTY
  :: KCVISITEMPTY -> IO (FunPtr KCVISITEMPTY)

-- | Type of a database synchronization callback.
type KcFileProc = FilePath -> Int64 -> Int64 -> IO Bool

callFileProc :: KcFileProc -> KCFILEPROC
callFileProc fileProc path count size _ = do
  filePath <- peekCAString path
  rv <- fileProc filePath (fromIntegral count) (fromIntegral size)
  if rv then return 1 else return 0

type KCFILEPROC = CString -> CLLong -> CLLong -> Ptr () -> IO CInt

foreign import ccall "wrapper" wrapKCFILEPROC
  :: KCFILEPROC -> IO (FunPtr KCFILEPROC)

int64_min :: CLLong
int64_min = #const INT64_MIN

--------------------------------------------------------------------------------
-- Utilities

-- | Allocate a region on memory.
kcmalloc :: Int -> IO (Ptr a)
kcmalloc sz = _kcmalloc (fromIntegral sz)

foreign import ccall safe "kclangc.h kcmalloc" _kcmalloc
  :: CSize -> IO (Ptr a)

-- | Release a region allocated in the library.
foreign import ccall safe "kclangc.h kcfree" kcfree
  :: Ptr a -> IO ()

-- | Get the time of day in seconds.
kctime :: IO Double
kctime = _kctime >>= return . realToFrac

foreign import ccall safe "kclangc.h kctime" _kctime
  :: IO CDouble

-- | Convert a string to an integer.
kcatoi :: String -> IO Int64
kcatoi str =
  withCAString str $ \cstr ->
  _kcatoi cstr >>= return . fromIntegral

foreign import ccall unsafe "kclangc.h kcatoi" _kcatoi
  :: CString -> IO CLLong

-- | Convert a string with a metric prefix to an integer.
kcatoix :: String -> IO Int64
kcatoix str =
  withCAString str $ \cstr ->
  _kcatoix cstr >>= return . fromIntegral

foreign import ccall unsafe "kclangc.h kcatoix" _kcatoix
  :: CString -> IO CLLong

-- | Convert a string to a real number.
kcatof :: String -> IO Double
kcatof str =
  withCAString str $ \cstr ->
  _kcatof cstr >>= return . realToFrac

foreign import ccall unsafe "kclangc.h kcatof" _kcatof
  :: CString -> IO CDouble

-- | Get the hash value by MurMur hashing.
kchashmurmur :: BS.ByteString -> IO Int64
kchashmurmur buff =
  BS.unsafeUseAsCStringLen buff $ \(b, n) ->
    _kchashmurmur b (fromIntegral n) >>= return . fromIntegral

foreign import ccall unsafe "kclangc.h kchashmurmur" _kchashmurmur
  :: CString -> CSize -> IO CLLong

-- | Get the hash value by FNV hashing.
kchashfnv :: BS.ByteString -> IO Int64
kchashfnv buff =
  BS.unsafeUseAsCStringLen buff $ \(b, n) ->
    _kchashfnv b (fromIntegral n) >>= return . fromIntegral

foreign import ccall unsafe "kclangc.h kchashfnv" _kchashfnv
  :: CString -> CSize -> IO CLLong

-- | Get the quiet Not-a-Number value.
kcnan :: IO Double
kcnan = _kcnan >>= return . realToFrac

foreign import ccall unsafe "kclangc.h kcnan" _kcnan
  :: IO CDouble

-- | Get the positive infinity value.
kcinf :: IO Double
kcinf = _kcinf >>= return . realToFrac

foreign import ccall unsafe "kclangc.h kcinf" _kcinf
  :: IO CDouble

-- | Check a number is a Not-a-Number value.
kcchknan :: Double -> IO Bool
kcchknan num = do rv <- _kcchknan (realToFrac num)
                  if rv == 0 then return False else return True

foreign import ccall unsafe "kclangc.h kcchknan" _kcchknan
  :: CDouble -> IO CInt

-- | Check a number is an infinity value.
kcchkinf :: Double -> IO Bool
kcchkinf num = do rv <- _kcchkinf (realToFrac num)
                  if rv == 0 then return False else return True

foreign import ccall unsafe "kclangc.h kcchkinf" _kcchkinf
  :: CDouble -> IO CInt

-- | Get the readable string of an error code.
kcecodename :: Int -> IO String
kcecodename code = do rv <- _kcecodename (fromIntegral code); peekCAString rv

foreign import ccall unsafe "kclangc.h kcecodename" _kcecodename
  :: CInt -> IO CString

--------------------------------------------------------------------------------

-- | An exception indicating an error in a KC operation.
data KcException = KcException String KcError String
    deriving (Eq, Show, Typeable)

instance Exception KcException

kcThrow :: KcDb -> String -> IO a
kcThrow db fname = do
  err <- kcdbecode db
  msg <- kcdbemsg db
  throwIO $ KcException fname err msg

handleBoolResult :: KcDb -> String -> CInt -> IO ()
handleBoolResult db fname status =
  if status == 0 then kcThrow db fname else return ()

handleMapBoolResult :: String -> CInt -> IO ()
handleMapBoolResult fname status =
  if status == 0 then throwIO (KcException fname KCEINVALID "") else return ()

--------------------------------------------------------------------------------

-- | Polymorphic database.
newtype KcDb = KcDb { unKcDb :: ForeignPtr KCDB }
data KCDB      -- native type

-- | Create a polymorphic database object.
kcdbnew :: IO KcDb
kcdbnew = _kcdbnew >>= wrapdb

wrapdb :: Ptr KCDB -> IO KcDb
wrapdb db = do
  fp <- newForeignPtr_ db
  --addForeignPtrFinalizer _kcdbdelFunPtr fp
  return $ KcDb fp

foreign import ccall safe "kclangc.h kcdbnew" _kcdbnew
  :: IO (Ptr KCDB)

foreign import ccall safe "kclangc.h &kcdbdel" _kcdbdelFunPtr
  :: FunPtr (Ptr KCDB -> IO ())

-- | Destroy a database object.
kcdbdel :: KcDb -> IO ()
kcdbdel db = do withForeignPtr (unKcDb db) _kcdbdel; return ()

foreign import ccall safe "kclangc.h kcdbdel" _kcdbdel
  :: Ptr KCDB -> IO ()

-- | Tuning parameters for database creation.
data KcTune = KcTuneType KcTuneType
            | KcTuneLogger KcLogger
            | KcTuneLogKinds KcLogLevel
            | KcTuneLogPx String
            | KcTuneOptions [KcOption] -- ^ supported by: cache hash, cache tree, file hash, file tree, directory hash, directory tree
            | KcTuneBuckets Int -- ^ supported by: cache hash, cache tree, file hash, file tree
            | KcTuneCompressor KcCompressor -- ^ supported by: cache hash, cache tree, file hash, file tree, directory hash, directory tree
            | KcTuneZkey String -- ^ supported by: cache hash, cache tree, file hash, file tree, directory hash, directory tree
            | KcTuneCapCount Int -- ^ supported by: cache hash
            | KcTuneCapSize Int -- ^ supported by: cache hash
            | KcTunePage Int -- ^ supported by: cache tree, file tree, directory tree
            | KcTuneComparator KcComparator -- ^ supported by: cache tree, file tree, directory tree
            | KcTunePageCache Int -- ^ supported by: cache tree, file tree, directory tree
            | KcTuneAlignment Int -- ^ supported by: file hash, file tree
            | KcTuneFbp Int -- ^ supported by: file hash, file tree
            | KcTuneMap Int -- ^ supported by: file hash, file tree
            | KcTuneDefrag Int -- ^ supported by: file hash, file tree
data KcTuneType = KcTypePrototypeHashDb | KcTypePrototypeTreeDb
                | KcTypeCacheHashDb | KcTypeCacheTreeDb
                | KcTypeFileHashDb | KcTypeFileTreeDb
                | KcTypeDirectoryHashDb | KcTypeDirectoryTreeDb
data KcLogger = KcLoggerStdout | KcLoggerStderr
data KcLogLevel = KcLogDebug | KcLogInfo | KcLogWarn | KcLogError
data KcOption = KcOptionSmall | KcOptionLinear | KcOptionCompress
data KcCompressor = KcCompressorZlib | KcCompressorDeflate
                      | KcCompressorGzip | KcCompressorLzo
                      | KcCompressorLzma | KcCompressorArc
data KcComparator = KcComparatorLexical | KcComparatorDecimal

instance Show KcTune where
  show (KcTuneType t) = "type=" ++ show t
  show (KcTuneLogger l) = "log=" ++ show l
  show (KcTuneLogKinds l) = "logkinds=" ++ show l
  show (KcTuneLogPx s) = "logpx=" ++ s
  show (KcTuneOptions opts) = "opts=" ++ foldr (\o r -> show o ++ r) "" opts
  show (KcTuneBuckets n) = "bnum=" ++ show n
  show (KcTuneCompressor c) = "zcomp=" ++ show c
  show (KcTuneZkey s) = "zkey=" ++ show s
  show (KcTuneCapCount c) = "capcount=" ++ show c
  show (KcTuneCapSize n) = "capsize=" ++ show n
  show (KcTunePage n) = "psiz=" ++ show n
  show (KcTuneComparator c) = "rcomp=" ++ show c
  show (KcTunePageCache n) = "pccap=" ++ show n
  show (KcTuneAlignment n) = "apow=" ++ show n
  show (KcTuneFbp n) = "fpow=" ++ show n
  show (KcTuneMap n) = "msiz=" ++ show n
  show (KcTuneDefrag n) = "dfunit=" ++ show n

instance Show KcTuneType where
  show KcTypePrototypeHashDb = "-"
  show KcTypePrototypeTreeDb = "+"
  show KcTypeCacheHashDb = "*"
  show KcTypeCacheTreeDb = "%"
  show KcTypeFileHashDb = "kch"
  show KcTypeFileTreeDb = "kct"
  show KcTypeDirectoryHashDb = "kcd"
  show KcTypeDirectoryTreeDb = "kcf"

instance Show KcLogger where
  show KcLoggerStdout = "-"
  show KcLoggerStderr = "+"

instance Show KcLogLevel where
  show KcLogDebug = "debug"
  show KcLogInfo = "info"
  show KcLogWarn = "warn"
  show KcLogError = "error"

instance Show KcOption where
  show KcOptionSmall = "s"
  show KcOptionLinear = "l"
  show KcOptionCompress = "c"

instance Show KcCompressor where
  show KcCompressorZlib = "zlib"
  show KcCompressorDeflate = "def"
  show KcCompressorGzip = "gz"
  show KcCompressorLzo = "lzo"
  show KcCompressorLzma = "lzma"
  show KcCompressorArc = "arc"

instance Show KcComparator where
  show KcComparatorLexical = "lex"
  show KcComparatorDecimal = "dec"

-- | Open a database file.
kcdbopen ::
  KcDb
  -- ^ @db@ - a database object.
  -> String
  -- ^ @path@ - the path of a database file.
  --
  -- * If it is @-@, the database will be a prototype hash database.
  --
  -- * If it is @+@, the database will be a prototype tree database.
  --
  -- * If it is @*@, the database will be a cache hash database.
  --
  -- * If it is @%@, the database will be a cache tree database.
  --
  -- * If its suffix is @.kch@, the database will be a file hash database.
  --
  -- * If its suffix is @.kct@, the database will be a file tree database.
  --
  -- * If its suffix is @.kcd@, the database will be a directory hash database.
  --
  -- * If its suffix is @.kcf@, the database will be a directory tree database.
  --
  -- * Otherwise, this function fails.
  -> [KcTune]
  -- ^ @tune@ - tuning parameters.
  -> [KcOpenMode]
  -- ^ @mode@ - the connection mode flags.
  -- 'KCOWRITER' as a writer, 'KCOREADER' as a reader.
  -- The following flags may be added to the writer mode:
  --
  -- * 'KCOCREATE', which means it creates a new database if the file
  -- does not exist,
  --
  -- * 'KCOTRUNCATE', which means it creates a new database regardless
  -- if the file exists,
  --
  -- * 'KCOAUTOTRAN', which means each updating operation is performed
  -- in implicit transaction,
  --
  -- * 'KCOAUTOSYNC', which means each updating operation is followed
  -- by implicit synchronization with the file system.
  --
  -- The following flags may be added to both of the reader mode and the writer mode:
  --
  -- * 'KCONOLOCK', which means it opens the database file without file locking,
  --
  -- * 'KCOTRYLOCK', which means locking is performed without blocking,
  --
  -- * 'KCONOREPAIR', which means the database file is not repaired
  -- implicitly even if file destruction is detected.
  -> IO ()
  -- ^ Returns @()@ on success, throws 'KcException' on failure.
  --
  -- Every opened database must be closed by the 'kcdbclose' method
  -- when it is no longer in use.  It is not allowed for two or more
  -- database objects in the same process to keep their connections to
  -- the same database file at the same time.
kcdbopen db path tune mode =
  let tunePath = foldl (\prev t -> prev ++ "#" ++ show t) path tune in
  withForeignPtr (unKcDb db) $ \c_db ->
    withCAString tunePath $ \c_path ->
      _kcdbopen c_db c_path (numOfOpenModes mode) >>= handleBoolResult db "kcdbopen"

foreign import ccall safe "kclangc.h kcdbopen" _kcdbopen
  :: Ptr KCDB -> CString -> CUInt -> IO CInt

-- | Close the database file.
kcdbclose :: KcDb -> IO ()
kcdbclose db =
  withForeignPtr (unKcDb db) $ \c_db -> do
    _kcdbclose c_db >>= handleBoolResult db "kcdbclose"

foreign import ccall safe "kclangc.h kcdbclose" _kcdbclose
  :: Ptr KCDB -> IO CInt

-- | Get the code of the last happened error.
kcdbecode :: KcDb -> IO KcError
kcdbecode db =
  withForeignPtr (unKcDb db) $ \c_db -> do
    code <- _kcdbecode c_db
    return $ kcErrorOfNum code

foreign import ccall unsafe "kclangc.h kcdbecode" _kcdbecode
  :: Ptr KCDB -> IO CInt

-- | Get the supplement message of the last happened error.
kcdbemsg :: KcDb -> IO String
kcdbemsg db =
  withForeignPtr (unKcDb db) $ \c_db -> do
    msg <- _kcdbemsg c_db
    peekCAString msg

foreign import ccall unsafe "kclangc.h kcdbemsg" _kcdbemsg
  :: Ptr KCDB -> IO CString

-- | Accept a visitor to a record.
kcdbaccept :: KcDb
           -> BS.ByteString -- ^ key
           -> KcVisitFull -> KcVisitEmpty
           -> Bool -- ^ writable
           -> IO ()
kcdbaccept db key visitFull visitEmpty writable =
  withForeignPtr (unKcDb db) $ \c_db -> do
    BS.unsafeUseAsCStringLen key $ \(kbuf, ksiz) -> do
      vf <- wrapKCVISITFULL $ callVisitFull visitFull
      ve <- wrapKCVISITEMPTY $ callVisitEmpty visitEmpty
      rv <- _kcdbaccept c_db kbuf (fromIntegral ksiz) vf ve
              nullPtr (if writable then 1 else 0)
      freeHaskellFunPtr ve
      freeHaskellFunPtr vf
      handleBoolResult db "kcdbaccept" rv

foreign import ccall safe "kclangc.h kcdbaccept" _kcdbaccept
  :: Ptr KCDB -> CString -> CSize -> FunPtr KCVISITFULL -> FunPtr KCVISITEMPTY ->
     Ptr () -> CInt -> IO CInt

-- | Accept a visitor to multiple records at once.
kcdbacceptbulk :: KcDb
               -> [BS.ByteString] -- ^ keys
               -> KcVisitFull -> KcVisitEmpty
               -> Bool -- ^ writable
               -> IO ()
kcdbacceptbulk db keys visitFull visitEmpty writable = do
  withForeignPtr (unKcDb db) $ \c_db -> do
    let nkeys = length keys
    withKcStrArray keys $ \keyArr -> do
      vf <- wrapKCVISITFULL $ callVisitFull visitFull
      ve <- wrapKCVISITEMPTY $ callVisitEmpty visitEmpty
      rv <- _kcdbacceptbulk c_db keyArr (fromIntegral nkeys) vf ve
              nullPtr (if writable then 1 else 0)
      freeHaskellFunPtr ve
      freeHaskellFunPtr vf
      handleBoolResult db "kcdbacceptbulk" rv

foreign import ccall safe "kclangc.h kcdbacceptbulk" _kcdbacceptbulk
  :: Ptr KCDB -> Ptr KcStr -> CSize -> FunPtr KCVISITFULL -> FunPtr KCVISITEMPTY ->
     Ptr () -> CInt -> IO CInt

-- | Iterate to accept a visitor for each record.
kcdbiterate :: KcDb -> KcVisitFull
               -> Bool -- ^ writable
               -> IO ()
kcdbiterate db visitFull writable = do
  withForeignPtr (unKcDb db) $ \c_db -> do
    vf <- wrapKCVISITFULL $ callVisitFull visitFull
    rv <- _kcdbiterate c_db vf nullPtr (if writable then 1 else 0)
    freeHaskellFunPtr vf
    handleBoolResult db "kcdbiterate" rv

foreign import ccall safe "kclangc.h kcdbiterate" _kcdbiterate
  :: Ptr KCDB -> FunPtr KCVISITFULL -> Ptr () -> CInt -> IO CInt

-- | Set the value of a record.
kcdbset :: KcDb
        -> BS.ByteString -- ^ key
        -> BS.ByteString -- ^ value
        -> IO ()
kcdbset db key val =
  withForeignPtr (unKcDb db) $ \c_db -> do
    BS.unsafeUseAsCStringLen key $ \(kbuf, ksiz) ->
      BS.unsafeUseAsCStringLen val $ \(vbuf, vsiz) -> do
        _kcdbset c_db kbuf (fromIntegral ksiz) vbuf (fromIntegral vsiz)
        >>= handleBoolResult db "kcdbset"

foreign import ccall safe "kclangc.h kcdbset" _kcdbset
  :: Ptr KCDB -> CString -> CSize -> CString -> CSize -> IO CInt

-- | Add a record.
kcdbadd :: KcDb
        -> BS.ByteString -- ^ key
        -> BS.ByteString -- ^ value
        -> IO ()
kcdbadd db key val =
  withForeignPtr (unKcDb db) $ \c_db -> do
    BS.unsafeUseAsCStringLen key $ \(kbuf, ksiz) ->
      BS.unsafeUseAsCStringLen val $ \(vbuf, vsiz) -> do
        _kcdbadd c_db kbuf (fromIntegral ksiz) vbuf (fromIntegral vsiz)
        >>= handleBoolResult db "kcdbadd"

foreign import ccall safe "kclangc.h kcdbadd" _kcdbadd
  :: Ptr KCDB -> CString -> CSize -> CString -> CSize -> IO CInt

-- | Replace the value of a record.
kcdbreplace :: KcDb
            -> BS.ByteString -- ^ key
            -> BS.ByteString -- ^ value
            -> IO ()
kcdbreplace db key val =
  withForeignPtr (unKcDb db) $ \c_db -> do
    BS.unsafeUseAsCStringLen key $ \(kbuf, ksiz) ->
      BS.unsafeUseAsCStringLen val $ \(vbuf, vsiz) ->
        _kcdbreplace c_db kbuf (fromIntegral ksiz) vbuf (fromIntegral vsiz)
        >>= handleBoolResult db "kcdbreplace"

foreign import ccall safe "kclangc.h kcdbreplace" _kcdbreplace
  :: Ptr KCDB -> CString -> CSize -> CString -> CSize -> IO CInt

-- | Append the value of a record.
kcdbappend :: KcDb
           -> BS.ByteString -- ^ key
           -> BS.ByteString -- ^ value
           -> IO ()
kcdbappend db key val =
  withForeignPtr (unKcDb db) $ \c_db -> do
    BS.unsafeUseAsCStringLen key $ \(kbuf, ksiz) ->
      BS.unsafeUseAsCStringLen val $ \(vbuf, vsiz) ->
        _kcdbappend c_db kbuf (fromIntegral ksiz) vbuf (fromIntegral vsiz)
        >>= handleBoolResult db "kcdbappend"

foreign import ccall safe "kclangc.h kcdbappend" _kcdbappend
  :: Ptr KCDB -> CString -> CSize -> CString -> CSize -> IO CInt

-- | Add a number to the numeric value of a record.
kcdbincrint :: KcDb
            -> BS.ByteString -- ^ key
            -> Int64 -- ^ increment amount
            -> IO Int64
kcdbincrint db key num =
  withForeignPtr (unKcDb db) $ \c_db -> do
    BS.unsafeUseAsCStringLen key $ \(kbuf, ksiz) -> do
      rv <- _kcdbincrint c_db kbuf (fromIntegral ksiz) (fromIntegral num)
      if rv == int64_min then kcThrow db "kcdbincrint"
        else return (fromIntegral rv)

foreign import ccall safe "kclangc.h kcdbincrint" _kcdbincrint
  :: Ptr KCDB -> CString -> CSize -> CLLong -> IO CLLong

-- | Add a number to the numeric value of a record.
kcdbincrdouble :: KcDb
               -> BS.ByteString -- ^ key
               -> Double -- ^ increment amount
               -> IO Double
kcdbincrdouble db key num =
  withForeignPtr (unKcDb db) $ \c_db -> do
    BS.unsafeUseAsCStringLen key $ \(kbuf, ksiz) -> do
      rv <- _kcdbincrdouble c_db kbuf (fromIntegral ksiz) (realToFrac num)
      nan <- _kcnan
      if rv == nan then kcThrow db "kcdbincrdouble"
        else return (realToFrac rv)

foreign import ccall safe "kclangc.h kcdbincrdouble" _kcdbincrdouble
  :: Ptr KCDB -> CString -> CSize -> CDouble -> IO CDouble

-- | Perform compare-and-swap.
kcdbcas :: KcDb
        -> BS.ByteString -- ^ key
        -> BS.ByteString -- ^ old value
        -> BS.ByteString -- ^ new value
        -> IO ()
kcdbcas db key nv ov =
  withForeignPtr (unKcDb db) $ \c_db -> do
    BS.unsafeUseAsCStringLen key $ \(kbuf, ksiz) -> do
      BS.unsafeUseAsCStringLen nv $ \(nvbuf, nvsiz) -> do
        BS.unsafeUseAsCStringLen ov $ \(ovbuf, ovsiz) -> do
          _kcdbcas c_db kbuf (fromIntegral ksiz) nvbuf (fromIntegral nvsiz)
            ovbuf (fromIntegral ovsiz)
          >>= handleBoolResult db "kcdbcas"

foreign import ccall safe "kclangc.h kcdbcas" _kcdbcas
  :: Ptr KCDB -> CString -> CSize -> CString -> CSize ->
     CString -> CSize -> IO CInt

-- | Remove a record.
kcdbremove :: KcDb
           -> BS.ByteString -- ^ key
           -> IO ()
kcdbremove db key =
  withForeignPtr (unKcDb db) $ \c_db -> do
    BS.unsafeUseAsCStringLen key $ \(kbuf, ksiz) -> do
      _kcdbremove c_db kbuf (fromIntegral ksiz) >>= handleBoolResult db "kcdbremove"

foreign import ccall safe "kclangc.h kcdbremove" _kcdbremove
  :: Ptr KCDB -> CString -> CSize -> IO CInt

-- | Retrieve the value of a record.
kcdbget :: KcDb
        -> BS.ByteString -- ^ key
        -> IO (Maybe BS.ByteString)
kcdbget db key =
  withForeignPtr (unKcDb db) $ \c_db -> do
    BS.unsafeUseAsCStringLen key $ \(kbuf, ksiz) -> do
      alloca $ \ptr -> do
        cstr <- _kcdbget c_db kbuf (fromIntegral ksiz) ptr
        bsOfCString cstr ptr

foreign import ccall safe "kclangc.h kcdbget" _kcdbget
  :: Ptr KCDB -> CString -> CSize -> Ptr CSize -> IO CString

-- | Retrieve the value of a record.
kcdbgetbuf :: KcDb
           -> BS.ByteString -- ^ key
           -> Int -- ^ max chars to retrieve
           -> IO (Maybe BS.ByteString)
kcdbgetbuf db key maxElts =
  withForeignPtr (unKcDb db) $ \c_db -> do
    BS.unsafeUseAsCStringLen key $ \(kbuf, ksiz) -> do
      vbuf <- mallocArray maxElts
      n <- _kcdbgetbuf c_db kbuf (fromIntegral ksiz) vbuf (fromIntegral maxElts)
      if n == -1 then return Nothing
        else do
          bs <- BS.unsafePackCStringFinalizer (castPtr vbuf) (fromIntegral n)
                  (kcfree vbuf)
          return $ Just bs

foreign import ccall safe "kclangc.h kcdbgetbuf" _kcdbgetbuf
  :: Ptr KCDB -> CString -> CSize -> CString -> CSize -> IO CInt

-- | Store records at once.
kcdbsetbulk :: KcDb
            -> [(BS.ByteString, BS.ByteString)] -- ^ records to store
            -> Bool  -- ^ atomic
            -> IO Int64 -- ^ returns number of records stored
kcdbsetbulk db recs atomic =
  withForeignPtr (unKcDb db) $ \c_db -> do
    let nrecs = length recs
    withKcRecArray recs $ \keyArr -> do
      n <- _kcdbsetbulk c_db keyArr (fromIntegral nrecs) (if atomic then 1 else 0)
      if n == -1 then kcThrow db "kcdbsetbulk" else return (fromIntegral n)

foreign import ccall safe "kclangc.h kcdbsetbulk" _kcdbsetbulk
  :: Ptr KCDB -> Ptr KcRec -> CSize -> CInt -> IO CLLong

-- | Remove records at once.
kcdbremovebulk :: KcDb
               -> [BS.ByteString] -- ^ keys of records to remove
               -> Bool -- ^ atomic
               -> IO Int64 -- ^ returns number of records removed
kcdbremovebulk db keys atomic =
  withForeignPtr (unKcDb db) $ \c_db -> do
    let nrecs = length keys
    withKcStrArray keys $ \keyArr -> do
      n <- _kcdbremovebulk c_db keyArr (fromIntegral nrecs) (if atomic then 1 else 0)
      if n == -1 then kcThrow db "kcdbremovebulk" else return (fromIntegral n)

foreign import ccall safe "kclangc.h kcdbremovebulk" _kcdbremovebulk
  :: Ptr KCDB -> Ptr KcStr -> CSize -> CInt -> IO CLLong

-- | Retrieve records at once.
kcdbgetbulk :: KcDb
            -> [BS.ByteString] -- ^ keys of the records to retrieve
            -> Bool -- ^ atomic
            -> IO [(BS.ByteString, BS.ByteString)]
kcdbgetbulk db keys atomic =
  withForeignPtr (unKcDb db) $ \c_db -> do
    let nkeys = length keys
    withKcStrArray keys $ \keyArr -> do
      allocaArray nkeys $ \recArr -> do
        n <- _kcdbgetbulk c_db keyArr (fromIntegral nkeys) recArr
             (if atomic then 1 else 0)
        if n == -1 then kcThrow db "kcdbgetbulk"
          else do
          recList <- peekArray (fromIntegral n) recArr
          return $ map (\(KcRec p) -> p) recList

foreign import ccall safe "kclangc.h kcdbgetbulk" _kcdbgetbulk
  :: Ptr KCDB -> Ptr KcStr -> CSize -> Ptr KcRec -> CInt -> IO CLLong

-- | Remove all records.
kcdbclear :: KcDb -> IO ()
kcdbclear db =
  withForeignPtr (unKcDb db) $ \c_db -> do
    _kcdbclear c_db >>= handleBoolResult db "kcdbclear"

foreign import ccall safe "kclangc.h kcdbclear" _kcdbclear
  :: Ptr KCDB -> IO CInt

-- | Synchronize updated contents with the file and the device.
kcdbsync :: KcDb
         -> Bool -- ^ @True@ for physical synchronization, @False@ for
                 -- logical synchronization
         -> KcFileProc -- ^ postprocessor callback
         -> IO ()
kcdbsync db hard fileProc = do
  withForeignPtr (unKcDb db) $ \c_db -> do
    fp <- wrapKCFILEPROC $ callFileProc fileProc
    rv <- _kcdbsync c_db (if hard then 1 else 0) fp nullPtr
    freeHaskellFunPtr fp
    handleBoolResult db "kcdbsync" rv

foreign import ccall safe "kclangc.h kcdbsync" _kcdbsync
  :: Ptr KCDB -> CInt -> FunPtr KCFILEPROC -> Ptr () -> IO CInt

-- | Create a copy of the database file.
kcdbcopy :: KcDb
         -> FilePath -- ^ path to the destination file
         -> IO ()
kcdbcopy db dest =
  withForeignPtr (unKcDb db) $ \c_db -> do
    withCAString dest $ \c_dest -> do
      _kcdbcopy c_db c_dest >>= handleBoolResult db "kcdbcopy"

foreign import ccall safe "kclangc.h kcdbcopy" _kcdbcopy
  :: Ptr KCDB -> CString -> IO CInt

-- | Begin transaction.
kcdbbegintran :: KcDb
              -> Bool -- ^ @True@ for physical synchronization, @False@
                      -- for logical synchronization
              -> IO ()
kcdbbegintran db hard =
  withForeignPtr (unKcDb db) $ \c_db -> do
    _kcdbbegintran c_db (if hard then 1 else 0)
    >>= handleBoolResult db "kcdbbegintran"

foreign import ccall safe "kclangc.h kcdbbegintran" _kcdbbegintran
  :: Ptr KCDB -> CInt -> IO CInt

-- | Try to begin transaction.
kcdbbegintrantry :: KcDb
                 -> Bool  -- ^ @True@ for physical synchronization,
                          -- @False@ for logical synchronization
                 -> IO ()
kcdbbegintrantry db hard =
  withForeignPtr (unKcDb db) $ \c_db -> do
    _kcdbbegintrantry c_db (if hard then 1 else 0)
    >>= handleBoolResult db "kcdbbegintrantry"

foreign import ccall safe "kclangc.h kcdbbegintrantry" _kcdbbegintrantry
  :: Ptr KCDB -> CInt -> IO CInt

-- | End transaction.
kcdbendtran :: KcDb
            -> Bool  -- ^ @True@ to commit, @False@ to abort
            -> IO ()
kcdbendtran db commit =
  withForeignPtr (unKcDb db) $ \c_db -> do
    _kcdbendtran c_db (if commit then 1 else 0)
    >>= handleBoolResult db "kcdbendtran"

foreign import ccall safe "kclangc.h kcdbendtran" _kcdbendtran
  :: Ptr KCDB -> CInt -> IO CInt

-- | Dump records into a file.
kcdbdumpsnap :: KcDb
             -> FilePath -- ^ destination file
             -> IO ()
kcdbdumpsnap db dest =
  withForeignPtr (unKcDb db) $ \c_db -> do
    withCAString dest $ \c_dest -> do
      _kcdbdumpsnap c_db c_dest >>= handleBoolResult db "kcdbdumpsnap"

foreign import ccall safe "kclangc.h kcdbdumpsnap" _kcdbdumpsnap
  :: Ptr KCDB -> CString -> IO CInt

-- | Load records from a file.
kcdbloadsnap :: KcDb
             -> FilePath -- ^ source file
             -> IO ()
kcdbloadsnap db src =
  withForeignPtr (unKcDb db) $ \c_db -> do
    withCAString src $ \c_src -> do
      _kcdbloadsnap c_db c_src >>= handleBoolResult db "kcdbloadsnap"

foreign import ccall safe "kclangc.h kcdbloadsnap" _kcdbloadsnap
  :: Ptr KCDB -> CString -> IO CInt

-- | Get the number of records.
kcdbcount :: KcDb -> IO Int64
kcdbcount db =
  withForeignPtr (unKcDb db) $ \c_db -> do
    n <- _kcdbcount c_db
    if n == -1 then kcThrow db "kcdbcount" else return (fromIntegral n)

foreign import ccall safe "kclangc.h kcdbcount" _kcdbcount
  :: Ptr KCDB -> IO CLLong

-- | Get the size of the database file.
kcdbsize :: KcDb -> IO Int64
kcdbsize db =
  withForeignPtr (unKcDb db) $ \c_db -> do
    n <- _kcdbsize c_db
    if n == -1 then kcThrow db "kcdbsize" else return (fromIntegral n)

foreign import ccall unsafe "kclangc.h kcdbsize" _kcdbsize
  :: Ptr KCDB -> IO CLLong

-- | Get the path of the database file.
kcdbpath :: KcDb -> IO String
kcdbpath db =
  withForeignPtr (unKcDb db) $ \c_db -> do
    cstr <- _kcdbpath c_db
    if cstr == nullPtr then kcThrow db "kcdbpath"
      else do rv <- peekCAString cstr
              kcfree cstr
              return rv

foreign import ccall safe "kclangc.h kcdbpath" _kcdbpath
  :: Ptr KCDB -> IO CString

-- | Get the miscellaneous status information.
kcdbstatus :: KcDb -> IO String
kcdbstatus db =
  withForeignPtr (unKcDb db) $ \c_db -> do
    cstr <- _kcdbstatus c_db
    if cstr == nullPtr then kcThrow db "kcdbstatus"
      else do rv <- peekCAString cstr
              kcfree cstr
              return rv

foreign import ccall safe "kclangc.h kcdbstatus" _kcdbstatus
  :: Ptr KCDB -> IO CString

-- | Get keys matching a prefix string.
kcdbmatchprefix :: KcDb
                -> BS.ByteString -- ^ prefix
                -> Int -- ^ max elements to return
                -> IO [BS.ByteString]
kcdbmatchprefix db prefix maxElts = do
  withForeignPtr (unKcDb db) $ \c_db -> do
    BS.unsafeUseAsCString prefix $ \pre -> do
      allocaArray maxElts $ \strarr -> do
        n <- _kcdbmatchprefix c_db pre strarr (fromIntegral maxElts)
        if n == -1 then kcThrow db "kcdbmatchprefix"
          else bsListOfCStringArr strarr n

foreign import ccall safe "kclangc.h kcdbmatchprefix" _kcdbmatchprefix
  :: Ptr KCDB -> CString -> Ptr CString -> CSize -> IO CLLong

-- | Get keys matching a regular expression string.
kcdbmatchregex :: KcDb
               -> BS.ByteString -- ^ regexp
               -> Int -- ^ max elements to return
               -> IO [BS.ByteString]
kcdbmatchregex db regexp maxElts = do
  withForeignPtr (unKcDb db) $ \c_db -> do
    BS.unsafeUseAsCString regexp $ \pre -> do
      allocaArray maxElts $ \strarr -> do
        n <- _kcdbmatchregex c_db pre strarr (fromIntegral maxElts)
        if n == -1 then kcThrow db "kcdbmatchregex"
          else bsListOfCStringArr strarr n

foreign import ccall safe "kclangc.h kcdbmatchregex" _kcdbmatchregex
  :: Ptr KCDB -> CString -> Ptr CString -> CSize -> IO CLLong

-- | Merge records from other databases.
kcdbmerge :: KcDb
          -> [KcDb] -- ^ database sources
          -> KcMergeMode -- ^ merge mode:
                         --
                         -- * 'KCMSET' to overwrite the existing value,
                         --
                         -- * 'KCMADD' to keep the existing value,
                         --
                         -- * 'KCMREPLACE' to modify the existing record only,
                         --
                         -- * 'KCMAPPEND' to append the new value.
          -> IO ()
kcdbmerge db srcs mode = do
  withForeignPtr (unKcDb db) $ \c_db -> do
    let elts = length srcs
    allocaArray elts $ \srcarr -> do
      pokeArray srcarr (map (unsafeForeignPtrToPtr . unKcDb) srcs)
      let m = fromIntegral $ numOfKcMergeMode mode
      _kcdbmerge c_db srcarr (fromIntegral elts) m
      >>= handleBoolResult db "kcdbmerge"

foreign import ccall safe "kclangc.h kcdbmerge" _kcdbmerge
  :: Ptr KCDB -> Ptr (Ptr KCDB) -> CSize -> CUInt -> IO CInt

--------------------------------------------------------------------------------
-- Cursors

-- | Polymorphic cursor.
newtype KcCur = KcCur { unKcCur :: ForeignPtr KCCUR } deriving (Eq)
data KCCUR      -- native type

-- | Create a cursor object.
kcdbcursor :: KcDb -> IO KcCur
kcdbcursor db =
  withForeignPtr (unKcDb db) $ \c_db -> do
    cur <- _kcdbcursor c_db
    fp <- newForeignPtr_ cur
    --addForeignPtrFinalizer _kccurdelFunPtr fp
    return $ KcCur fp

foreign import ccall safe "kclangc.h kcdbcursor" _kcdbcursor
  :: Ptr KCDB -> IO (Ptr KCCUR)

foreign import ccall safe "kclangc.h &kccurdel" _kccurdelFunPtr
  :: FunPtr (Ptr KCCUR -> IO ())

-- | Destroy a cursor object.
kccurdel :: KcCur -> IO ()
kccurdel cur =
  withForeignPtr (unKcCur cur) $ \c_cur -> do
    _kccurdel c_cur

foreign import ccall safe "kclangc.h kccurdel" _kccurdel
  :: Ptr KCCUR -> IO ()

-- | Accept a visitor to the current record.
kccuraccept :: KcCur
            -> KcVisitFull
            -> Bool -- ^ writable
            -> Bool -- ^ step
            -> IO ()
kccuraccept cur visitFull writable step = do
  withForeignPtr (unKcCur cur) $ \c_cur -> do
    vf <- wrapKCVISITFULL $ callVisitFull visitFull
    rv <- _kccuraccept c_cur vf nullPtr (if writable then 1 else 0)
            (if step then 1 else 0)
    freeHaskellFunPtr vf
    db <- kccurdb cur
    handleBoolResult db "kccuraccept" rv

foreign import ccall safe "kclangc.h kccuraccept" _kccuraccept
  :: Ptr KCCUR -> FunPtr KCVISITFULL -> Ptr () -> CInt -> CInt -> IO CInt

-- | Remove the current record.
kccurremove :: KcCur -> IO ()
kccurremove cur = do
  withForeignPtr (unKcCur cur) $ \c_cur -> do
    rv <- _kccurremove c_cur
    db <- kccurdb cur
    handleBoolResult db "kccurremove" rv

foreign import ccall safe "kclangc.h kccurremove" _kccurremove
  :: Ptr KCCUR -> IO CInt

-- | Get the key of the current record.
kccurgetkey :: KcCur
            -> Bool -- ^ step
            -> IO (Maybe BS.ByteString)
kccurgetkey cur step = do
  withForeignPtr (unKcCur cur) $ \c_cur -> do
    alloca $ \ptr -> do
      cstr <- _kccurgetkey c_cur ptr (if step then 1 else 0)
      bsOfCString cstr ptr

foreign import ccall safe "kclangc.h kccurgetkey" _kccurgetkey
  :: Ptr KCCUR -> Ptr CSize -> CInt -> IO CString

-- | Get the value of the current record.
kccurgetvalue :: KcCur
              -> Bool -- ^ step
              -> IO (Maybe BS.ByteString)
kccurgetvalue cur step = do
  withForeignPtr (unKcCur cur) $ \c_cur -> do
    alloca $ \ptr -> do
      cstr <- _kccurgetvalue c_cur ptr (if step then 1 else 0)
      bsOfCString cstr ptr

foreign import ccall safe "kclangc.h kccurgetvalue" _kccurgetvalue
  :: Ptr KCCUR -> Ptr CSize -> CInt -> IO CString

-- | Get a pair of the key and the value of the current record.
kccurget :: KcCur
         -> Bool -- ^ step
         -> IO (Maybe (BS.ByteString, BS.ByteString))
kccurget cur step = do
  withForeignPtr (unKcCur cur) $ \c_cur -> do
    alloca $ \ksp -> do
      alloca $ \vbp -> do
        alloca $ \vsp -> do
          cstr <- _kccurget c_cur ksp vbp vsp (if step then 1 else 0)
          maybeKey <- bsOfCString cstr ksp
          case maybeKey of
            Just key -> do vb <- peek vbp
                           vs <- peek vsp
                           val <- BS.packCStringLen (vb, fromIntegral vs)
                           return $ Just (key, val)
            Nothing -> return Nothing

foreign import ccall safe "kclangc.h kccurget" _kccurget
  :: Ptr KCCUR -> Ptr CSize -> Ptr CString -> Ptr CSize -> CInt -> IO CString

-- | Jump the cursor to the first record for forward scan.
kccurjump :: KcCur -> IO ()
kccurjump cur =
  withForeignPtr (unKcCur cur) $ \c_cur -> do
    rv <- _kccurjump c_cur
    db <- kccurdb cur
    handleBoolResult db "kccurjump" rv

foreign import ccall safe "kclangc.h kccurjump" _kccurjump
  :: Ptr KCCUR -> IO CInt

-- | Jump the cursor to a record for forward scan.
kccurjumpkey :: KcCur -> BS.ByteString -> IO ()
kccurjumpkey cur key =
  withForeignPtr (unKcCur cur) $ \c_cur -> do
    BS.unsafeUseAsCStringLen key $ \(kbuf, ksiz) -> do
      rv <- _kccurjumpkey c_cur kbuf (fromIntegral ksiz)
      db <- kccurdb cur
      handleBoolResult db "kccurjumpkey" rv

foreign import ccall safe "kclangc.h kccurjumpkey" _kccurjumpkey
  :: Ptr KCCUR -> CString -> CSize -> IO CInt

-- | Jump the cursor to the last record for backward scan.
kccurjumpback :: KcCur -> IO ()
kccurjumpback cur =
  withForeignPtr (unKcCur cur) $ \c_cur -> do
    rv <- _kccurjumpback c_cur
    db <- kccurdb cur
    handleBoolResult db "kccurjumpback" rv

foreign import ccall safe "kclangc.h kccurjumpback" _kccurjumpback
  :: Ptr KCCUR -> IO CInt

-- | Jump the cursor to a record for backward scan.
kccurjumpbackkey :: KcCur -> BS.ByteString -> IO ()
kccurjumpbackkey cur key =
  withForeignPtr (unKcCur cur) $ \c_cur -> do
    BS.unsafeUseAsCStringLen key $ \(kbuf, ksiz) -> do
      rv <- _kccurjumpbackkey c_cur kbuf (fromIntegral ksiz)
      db <- kccurdb cur
      handleBoolResult db "kccurjumpbackkey" rv

foreign import ccall safe "kclangc.h kccurjumpbackkey" _kccurjumpbackkey
  :: Ptr KCCUR -> CString -> CSize -> IO CInt

-- | Step the cursor to the next record.
kccurstep :: KcCur -> IO ()
kccurstep cur =
  withForeignPtr (unKcCur cur) $ \c_cur -> do
    rv <- _kccurstep c_cur
    db <- kccurdb cur
    handleBoolResult db "kccurstep" rv

foreign import ccall safe "kclangc.h kccurstep" _kccurstep
  :: Ptr KCCUR -> IO CInt

-- | Step the cursor to the previous record.
kccurstepback :: KcCur -> IO ()
kccurstepback cur =
  withForeignPtr (unKcCur cur) $ \c_cur -> do
    rv <- _kccurstepback c_cur
    db <- kccurdb cur
    handleBoolResult db "kccurstepback" rv

foreign import ccall safe "kclangc.h kccurstepback" _kccurstepback
  :: Ptr KCCUR -> IO CInt

-- | Get the database object.
kccurdb :: KcCur -> IO KcDb
kccurdb cur =
  withForeignPtr (unKcCur cur) $ \c_cur -> do
    _kccurdb c_cur >>= wrapdb

foreign import ccall unsafe "kclangc.h kccurdb" _kccurdb
  :: Ptr KCCUR -> IO (Ptr KCDB)

-- | Get the code of the last happened error.
kccurecode :: KcCur -> IO KcError
kccurecode cur =
  withForeignPtr (unKcCur cur) $ \c_cur -> do
    code <- _kccurecode c_cur
    return $ kcErrorOfNum code

foreign import ccall unsafe "kclangc.h kccurecode" _kccurecode
  :: Ptr KCCUR -> IO CInt

-- | Get the supplement message of the last happened error.
kccuremsg :: KcCur -> IO String
kccuremsg cur =
  withForeignPtr (unKcCur cur) $ \c_cur -> do
    msg <- _kccuremsg c_cur
    peekCAString msg

foreign import ccall unsafe "kclangc.h kccuremsg" _kccuremsg
  :: Ptr KCCUR -> IO CString

--------------------------------------------------------------------------------
-- KcMap

newtype KcMap = KcMap { unKcMap :: ForeignPtr KCMAP } deriving (Eq)
data KCMAP      -- native type

-- | Create a string hash map object.
kcmapnew :: Int -- ^ the number of buckets of the hash table. If it is
                -- not more than 0, the default setting 31 is specified.
         -> IO KcMap
kcmapnew sz = do
  m <- _kcmapnew $ fromIntegral sz
  fp <- newForeignPtr_ m
  return $ KcMap fp

foreign import ccall unsafe "kclangc.h kcmapnew" _kcmapnew
  :: CSize -> IO (Ptr KCMAP)

-- | Destroy a map object.
kcmapdel :: KcMap -> IO ()
kcmapdel m = do
  withForeignPtr (unKcMap m) $ \c_map -> do
    _kcmapdel c_map

foreign import ccall unsafe "kclangc.h kcmapdel" _kcmapdel
  :: Ptr KCMAP -> IO ()

-- | Set the value of a record. If no record corresponds to the key, a
-- new record is created.  If the corresponding record exists, the
-- value is overwritten.
kcmapset :: KcMap
         -> BS.ByteString -- ^ key
         -> BS.ByteString -- ^ value
         -> IO ()
kcmapset m key val =
  withForeignPtr (unKcMap m) $ \c_map -> do
    BS.unsafeUseAsCStringLen key $ \(kbuf, ksiz) ->
      BS.unsafeUseAsCStringLen val $ \(vbuf, vsiz) -> do
        _kcmapset c_map kbuf (fromIntegral ksiz) vbuf (fromIntegral vsiz)

foreign import ccall unsafe "kclangc.h kcmapset" _kcmapset
  :: Ptr KCMAP -> CString -> CSize -> CString -> CSize -> IO ()

-- | Add a record. If no record corresponds to the key, a new record
-- is created.  If the corresponding record exists, the record is not
-- modified and false is returned.
kcmapadd :: KcMap
         -> BS.ByteString -- ^ key
         -> BS.ByteString -- ^ value
         -> IO ()
kcmapadd m key val =
  withForeignPtr (unKcMap m) $ \c_map -> do
    BS.unsafeUseAsCStringLen key $ \(kbuf, ksiz) ->
      BS.unsafeUseAsCStringLen val $ \(vbuf, vsiz) -> do
        _kcmapadd c_map kbuf (fromIntegral ksiz) vbuf (fromIntegral vsiz)
        >>= handleMapBoolResult "kcmapadd"

foreign import ccall safe "kclangc.h kcmapadd" _kcmapadd
  :: Ptr KCMAP -> CString -> CSize -> CString -> CSize -> IO CInt

-- | Replace the value of a record. If no record corresponds to the
-- key, no new record is created and false is returned. If the
-- corresponding record exists, the value is modified.
kcmapreplace :: KcMap
             -> BS.ByteString -- ^ key
             -> BS.ByteString -- ^ value
             -> IO ()
kcmapreplace m key val =
  withForeignPtr (unKcMap m) $ \c_map -> do
    BS.unsafeUseAsCStringLen key $ \(kbuf, ksiz) ->
      BS.unsafeUseAsCStringLen val $ \(vbuf, vsiz) ->
        _kcmapreplace c_map kbuf (fromIntegral ksiz) vbuf (fromIntegral vsiz)
        >>= handleMapBoolResult "kcmapreplace"

foreign import ccall safe "kclangc.h kcmapreplace" _kcmapreplace
  :: Ptr KCMAP -> CString -> CSize -> CString -> CSize -> IO CInt

-- | Append the value of a record. If no record corresponds to the
-- key, a new record is created.  If the corresponding record exists,
-- the given value is appended at the end of the existing value.
kcmapappend :: KcMap
            -> BS.ByteString -- ^ key
            -> BS.ByteString -- ^ value
            -> IO ()
kcmapappend m key val =
  withForeignPtr (unKcMap m) $ \c_map -> do
    BS.unsafeUseAsCStringLen key $ \(kbuf, ksiz) ->
      BS.unsafeUseAsCStringLen val $ \(vbuf, vsiz) ->
        _kcmapappend c_map kbuf (fromIntegral ksiz) vbuf (fromIntegral vsiz)

foreign import ccall safe "kclangc.h kcmapappend" _kcmapappend
  :: Ptr KCMAP -> CString -> CSize -> CString -> CSize -> IO ()

-- | Remove a record. If no record corresponds to the key, false is returned.
kcmapremove :: KcMap
            -> BS.ByteString -- ^ key
            -> IO ()
kcmapremove m key =
  withForeignPtr (unKcMap m) $ \c_map -> do
    BS.unsafeUseAsCStringLen key $ \(kbuf, ksiz) -> do
      _kcmapremove c_map kbuf (fromIntegral ksiz)
      >>= handleMapBoolResult "kcmapremove"

foreign import ccall safe "kclangc.h kcmapremove" _kcmapremove
  :: Ptr KCMAP -> CString -> CSize -> IO CInt

-- | Retrieve the value of a record.
kcmapget :: KcMap
         -> BS.ByteString -- ^ key
         -> IO (Maybe BS.ByteString)
kcmapget m key =
  withForeignPtr (unKcMap m) $ \c_map -> do
    BS.unsafeUseAsCStringLen key $ \(kbuf, ksiz) -> do
      alloca $ \ptr -> do
        cstr <- _kcmapget c_map kbuf (fromIntegral ksiz) ptr
        bsOfCString cstr ptr

foreign import ccall safe "kclangc.h kcmapget" _kcmapget
  :: Ptr KCMAP -> CString -> CSize -> Ptr CSize -> IO CString

-- | Remove all records.
kcmapclear :: KcMap -> IO ()
kcmapclear m =
  withForeignPtr (unKcMap m) $ \c_map -> do
    _kcmapclear c_map >>= handleMapBoolResult "kcmapclear"

foreign import ccall safe "kclangc.h kcmapclear" _kcmapclear
  :: Ptr KCMAP -> IO CInt

-- | Get the number of records.
kcmapcount :: KcMap -> IO Int
kcmapcount m =
  withForeignPtr (unKcMap m) $ \c_map -> do
    n <- _kcmapcount c_map
    if n == -1 then throwIO (KcException "kcmapcount" KCEINVALID "")
      else return (fromIntegral n)

foreign import ccall safe "kclangc.h kcmapcount" _kcmapcount
  :: Ptr KCMAP -> IO CSize

--------------------------------------------------------------------------------
-- KcMapIter

newtype KcMapIter = KcMapIter { unKcMapIter :: ForeignPtr KCMAPITER } deriving (Eq)
data KCMAPITER      -- native type

-- | Create a string hash map iterator object. The object of the
-- return value should be released with the 'kcmapiterdel' function when
-- it is no longer in use. This object will not be invalidated even
-- when the map object is updated once. However, phantom records may
-- be retrieved if they are removed after creation of each iterator.
kcmapiterator :: KcMap -> IO KcMapIter
kcmapiterator m =
  withForeignPtr (unKcMap m) $ \c_map -> do
    mi <- _kcmapiterator c_map
    fp <- newForeignPtr_ mi
    return $ KcMapIter fp

foreign import ccall safe "kclangc.h kcmapiterator" _kcmapiterator
  :: Ptr KCMAP -> IO (Ptr KCMAPITER)

-- | Destroy an iterator object.
kcmapiterdel :: KcMapIter -> IO ()
kcmapiterdel mi = withForeignPtr (unKcMapIter mi) _kcmapiterdel

foreign import ccall safe "kclangc.h kcmapiterdel" _kcmapiterdel
  :: Ptr KCMAPITER -> IO ()

-- | Get the key of the current record.
kcmapitergetkey :: KcMapIter -> Int -> IO (Maybe BS.ByteString)
kcmapitergetkey mi sz = do
  withForeignPtr (unKcMapIter mi) $ \c_mi -> do
    cstr <- _kcmapitergetkey c_mi (fromIntegral sz)
    if cstr == nullPtr then return Nothing
      else do bs <- BS.packCString cstr
              return $ Just bs

foreign import ccall safe "kclangc.h kcmapitergetkey" _kcmapitergetkey
  :: Ptr KCMAPITER -> CSize -> IO CString

-- | Get the value of the current record.
kcmapitergetvalue :: KcMapIter -> Int -> IO (Maybe BS.ByteString)
kcmapitergetvalue mi sz = do
  withForeignPtr (unKcMapIter mi) $ \c_mi -> do
    cstr <- _kcmapitergetvalue c_mi (fromIntegral sz)
    if cstr == nullPtr then return Nothing
      else do bs <- BS.packCString cstr
              return $ Just bs

foreign import ccall safe "kclangc.h kcmapitergetvalue" _kcmapitergetvalue
  :: Ptr KCMAPITER -> CSize -> IO CString

-- | Get a pair of the key and the value of the current record.
kcmapiterget :: KcMapIter -> IO (Maybe (BS.ByteString, BS.ByteString))
kcmapiterget mi = do
  withForeignPtr (unKcMapIter mi) $ \c_mi -> do
    alloca $ \ksp -> do
      alloca $ \vbp -> do
        alloca $ \vsp -> do
          cstr <- _kcmapiterget c_mi ksp vbp vsp
          maybeKey <- bsOfCString cstr ksp
          case maybeKey of
            Just key -> do vb <- peek vbp
                           vs <- peek vsp
                           val <- BS.packCStringLen (vb, fromIntegral vs)
                           return $ Just (key, val)
            Nothing -> return Nothing

foreign import ccall safe "kclangc.h kcmapiterget" _kcmapiterget
  :: Ptr KCMAPITER -> Ptr CSize -> Ptr CString -> Ptr CSize -> IO CString

-- | Step the cursor to the next record.
kcmapiterstep :: KcMapIter -> IO ()
kcmapiterstep mi = withForeignPtr (unKcMapIter mi) _kcmapiterstep

foreign import ccall safe "kclangc.h kcmapiterstep" _kcmapiterstep
  :: Ptr KCMAPITER -> IO ()

--------------------------------------------------------------------------------
-- KcMapSort

newtype KcMapSort = KcMapSort { unKcMapSort :: ForeignPtr KCMAPSORT } deriving (Eq)
data KCMAPSORT      -- native type

-- | Create a string hash map sorter object. This object will not be
-- invalidated even when the map object is updated once. However,
-- phantom records may be retrieved if they are removed after creation
-- of each sorter.
kcmapsorter :: KcMap -> IO KcMapSort
kcmapsorter m =
  withForeignPtr (unKcMap m) $ \c_map -> do
    ms <- _kcmapsorter c_map
    fp <- newForeignPtr_ ms
    return $ KcMapSort fp

foreign import ccall safe "kclangc.h kcmapsorter" _kcmapsorter
  :: Ptr KCMAP -> IO (Ptr KCMAPSORT)

kcmapsortdel :: KcMapSort -> IO ()
kcmapsortdel ms = withForeignPtr (unKcMapSort ms) _kcmapsortdel

foreign import ccall safe "kclangc.h kcmapsortdel" _kcmapsortdel
  :: Ptr KCMAPSORT -> IO ()

-- | Get the key of the current record.
kcmapsortgetkey :: KcMapSort -> IO (Maybe BS.ByteString)
kcmapsortgetkey ms =
  withForeignPtr (unKcMapSort ms) $ \c_ms -> do
    alloca $ \spp -> do
      cstr <- _kcmapsortgetkey c_ms spp
      bsOfCString cstr spp

foreign import ccall safe "kclangc.h kcmapsortgetkey" _kcmapsortgetkey
  :: Ptr KCMAPSORT -> Ptr CSize -> IO CString

-- | Get the value of the current record.
kcmapsortgetvalue :: KcMapSort -> IO (Maybe BS.ByteString)
kcmapsortgetvalue ms =
  withForeignPtr (unKcMapSort ms) $ \c_ms -> do
    alloca $ \spp -> do
      cstr <- _kcmapsortgetvalue c_ms spp
      bsOfCString cstr spp

foreign import ccall safe "kclangc.h kcmapsortgetvalue" _kcmapsortgetvalue
  :: Ptr KCMAPSORT -> Ptr CSize -> IO CString

-- | Get a pair of the key and the value of the current record.
kcmapsortget :: KcMapSort -> IO (Maybe (BS.ByteString, BS.ByteString))
kcmapsortget ms = do
  withForeignPtr (unKcMapSort ms) $ \c_ms -> do
    alloca $ \ksp -> do
      alloca $ \vbp -> do
        alloca $ \vsp -> do
          cstr <- _kcmapsortget c_ms ksp vbp vsp
          maybeKey <- bsOfCString cstr ksp
          case maybeKey of
            Just key -> do vb <- peek vbp
                           vs <- peek vsp
                           val <- BS.packCStringLen (vb, fromIntegral vs)
                           return $ Just (key, val)
            Nothing -> return Nothing

foreign import ccall safe "kclangc.h kcmapsortget" _kcmapsortget
  :: Ptr KCMAPSORT -> Ptr CSize -> Ptr CString -> Ptr CSize -> IO CString

-- | Step the sorter to the next record.
kcmapsortstep :: KcMapSort -> IO ()
kcmapsortstep ms = withForeignPtr (unKcMapSort ms) _kcmapsortstep

foreign import ccall safe "kclangc.h kcmapsortstep" _kcmapsortstep
  :: Ptr KCMAPSORT -> IO ()

--------------------------------------------------------------------------------
-- Helper Routines

-- | Brackets a db command between 'kcdbnew', 'kcdbopen', 'kcdbclose',
-- and 'kcdbdel' calls.
kcwithdbopen :: FilePath -> [KcTune] -> [KcOpenMode] -> (KcDb -> IO a) -> IO a
kcwithdbopen path tune mode action =
  bracket
    (do db <- kcdbnew
        kcdbopen db path tune mode
        return db)
    (\db -> do kcdbclose db
               kcdbdel db)
    action

-- | Brackets a cursor command between 'kcdbcursor' and 'kccurdel' calls.
kcwithdbcursor :: KcDb -> (KcCur -> IO a) -> IO a
kcwithdbcursor db action = bracket (kcdbcursor db) kccurdel action

-- | Brackets a db command between 'kcdbbegintran' and 'kcdbendtran' calls,
-- committing on successful completion of the command, and aborting on
-- exception.
kcwithdbtran :: KcDb
             -> Bool  -- ^ @True@ for physical synchronization,
                      -- @False@ for logical synchronization
             -> IO a  -- ^ db command
             -> IO a
kcwithdbtran db hard action =
  bracketOnError (kcdbbegintran db hard)
                 (const $ kcdbendtran db False)
                 (const $ do rv <- action; kcdbendtran db True; return rv)

-- | Brackets a map command between 'kcmapnew', and 'kcmapdel' calls.
kcwithmap :: Int -> (KcMap -> IO a) -> IO a
kcwithmap sz action =
  bracketOnError (kcmapnew sz) kcmapdel
                 (\m -> do rv <- action m; kcmapdel m; return rv)

-- | Brackets a map iterator command between 'kcmapiterator' and 'kcmapiterdel' calls.
kcwithmapiter :: KcMap -> (KcMapIter -> IO a) -> IO a
kcwithmapiter m action = bracket (kcmapiterator m) kcmapiterdel action

-- | Brackets a map sorter command between 'kcmapsorter' and 'kcmapsortdel' calls.
kcwithmapsort :: KcMap -> (KcMapSort -> IO a) -> IO a
kcwithmapsort m action = bracket (kcmapsorter m) kcmapsortdel action

--------------------------------------------------------------------------------
