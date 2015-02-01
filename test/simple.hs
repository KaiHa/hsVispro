import Control.Applicative ((<$>))
import Control.Concurrent  (threadDelay)
import Control.Monad       (when)
import Data.Maybe          (fromJust)
import System.Exit         (exitFailure)
import System.Process      (readProcess)
import Vis.Iob
import Vis.Reg

main :: IO ()
main = do
  l <- regConnect (Hostname "localhost") "hsFoo"
  regListPaths (fromJust l) (Path "/") >>= print
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

pioSet :: Path -> String -> IO String
pioSet (Path p) v = readProcess "/opt/vispro/bin/pio" ["set", p, v] ""

pioGet :: Path -> IO String
pioGet (Path p) =
    takeWhile (/= '\'') <$> dropWhile (== '\'')
    <$> readProcess "/opt/vispro/bin/pio" ["getval", p] ""
