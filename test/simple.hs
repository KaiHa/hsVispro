import Control.Applicative ((<$>))
import Control.Concurrent  (threadDelay)
import Control.Monad       (void, when)
import Data.Maybe          (fromJust)
import System.Exit         (exitFailure, ExitCode(..))
import System.Process      (readProcess, readProcessWithExitCode)
import Vis.Iob
import Vis.Reg
import Paths_hsVispro

main :: IO ()
main = do
  regImport =<< getDataFileName "test/test.dat"
  l <- regConnect (Hostname "localhost") "hsFoo"
  regListPaths (fromJust l) (Path "/hsVisproTest") >>= print
  -- TODO check return value
  regListVSets (fromJust l) (Path "/hsVisproTest") >>= print
  iobGetValue pvPath >>= print
  pv <- iobAccess pvPath
  testGet "eins"
  testGet "zwo"
  testGet "drei"
  testSet pv "vier"
  testSet pv "fuenf"
  testSet pv "sechs"
  iobSetValue pv $ IobString "FERTIG"
  iobRelease pv
  where
    pvPath = Path "/Project/DP"
    testGet a = do
      pioSet pvPath a
      (IobString b, _) <- iobGetValue pvPath
      putStrLn $ show a ++ " == " ++ show b ++ " ?"
      when (a /= b) exitFailure
    testSet pv a = do
      iobSetValue pv $ IobString a
      b <- pioGet pvPath
      putStrLn $ show a ++ " == " ++ show b ++ " ?"
      when (a /= b) exitFailure

pioSet :: Path -> String -> IO ()
pioSet (Path p) v = callProcess "/opt/vispro/bin/pio" ["set", p, v]

pioGet :: Path -> IO String
pioGet (Path p) =
    takeWhile (/= '\'') <$> dropWhile (== '\'')
    <$> readProcess "/opt/vispro/bin/pio" ["getval", p] ""

regImport :: String -> IO ()
regImport a = callProcess "/opt/vispro/bin/reg.imp" ["localhost", a, "/"]

callProcess :: FilePath -> [String] -> IO ()
callProcess p as = void $ readProcess p as ""
