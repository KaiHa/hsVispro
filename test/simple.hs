import Control.Applicative ((<$>))
import Control.Concurrent  (threadDelay)
import Control.Monad       (void, when)
import Data.IORef
import Data.Maybe          (fromJust)
import Foreign.Ptr         (Ptr)
import System.Exit         (exitFailure, ExitCode(..))
import System.Process      (readProcess, readProcessWithExitCode)
import Vis.Iob
import Vis.Reg
import Paths_hsVispro

main :: IO ()
main = do
  regImport =<< getDataFileName "test/test.dat"
  l <- regConnect (Hostname "localhost") "hsFoo"
  paths <- regListPaths (fromJust l) (Path "/hsVisproTest")
  print paths
  assertEqual (length paths) 2 "paths has not the expected length"
  assertEqual paths [ Path "/hsVisproTest/StringPV"
                    , Path "/hsVisproTest/IntPV"
                    ] "paths has wrong value"
  vsets <- regListVSets (fromJust l) (Path "/hsVisproTest")
  assertEqual vsets [RegObj (Path "/hsVisproTest/StringPV")
                            [VSetBool   "Bool"     True
                            ,VSetDouble "Double"   3.14159
                            ,VSetInt    "Integer"  93
                            ,VSetString "Greeting" "hallo welt"
                            ,VSetString "VarType"  "STRING"
                            ]
                    ,RegObj (Path "/hsVisproTest/IntPV") []
                    ] "vsets has wrong value"
  iobGetValue pvPath >>= print
  recv <- newIORef []
  subscription <- iobSubscribeValue (Path "/hsVisproTest/StringPV") (updateValue recv)
  pv <- iobAccess pvPath
  testGet "eins"
  testGet "zwo"
  testGet "drei"
  testSet pv "vier"
  testSet pv "fuenf"
  testSet pv "sechs"
  iobSetValue pv $ IobString "FERTIG"
  iobRelease pv
  iobRelease subscription
  readIORef recv >>= \a ->
    assertEqual a [(IobString "FERTIG","/hsVisproTest/StringPV", IobEventChangeStrg)
                  ,(IobString "sechs", "/hsVisproTest/StringPV", IobEventChangeStrg)
                  ,(IobString "fuenf", "/hsVisproTest/StringPV", IobEventChangeStrg)
                  ,(IobString "vier",  "/hsVisproTest/StringPV", IobEventChangeStrg)
                  ,(IobString "drei",  "/hsVisproTest/StringPV", IobEventChangeStrg)
                  ,(IobString "zwo",   "/hsVisproTest/StringPV", IobEventChangeStrg)
                  ,(IobString "eins",  "/hsVisproTest/StringPV", IobEventChangeStrg)
                  ] "received subscribed values are wrong"
  where
    pvPath = Path "/hsVisproTest/StringPV"
    testGet a = do
      pioSet pvPath a
      (IobString b, _) <- iobGetValue pvPath
      assertEqual a b "get returned wrong value"
    testSet pv a = do
      iobSetValue pv (IobString a) >>= \r -> assertEqual Success r ("set returned " ++ show r)
      (IobString b, _) <- iobGetValue pvPath
      assertEqual a b "set returned with success, but get did not work"
      c <- pioGet pvPath
      assertEqual a c "set had not written successful"
    updateValue ref pv e = do
      val       <- getValueFromPv pv
      Path path <- getPathFromPv pv
      readIORef ref >>= writeIORef ref . ((val, path, e):)

pioSet :: Path -> String -> IO ()
pioSet (Path p) v = callProcess "/opt/vispro/bin/pio" ["set", p, v]

pioGet :: Path -> IO String
pioGet (Path p) =
    takeWhile (/= '\'') <$> dropWhile (== '\'')
    <$> readProcess "/opt/vispro/bin/pio" ["getval", p] ""

regImport :: String -> IO ()
regImport a = callProcess "/opt/vispro/bin/reg.imp" ["localhost", a, "/"]

regCheck :: String -> IO String
regCheck a = readProcess "/opt/vispro/bin/reg.check" [] (a ++ "\nexit\n")

callProcess :: FilePath -> [String] -> IO ()
callProcess p as = void $ readProcess p as ""

assertEqual :: (Eq a, Show a) => a -> a -> String -> IO ()
assertEqual a b m =
  when (a /= b) $ error $ (show a) ++ " == " ++ (show b) ++ "--> failure\n" ++ m
