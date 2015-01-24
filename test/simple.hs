import Control.Applicative ((<$>))
import Control.Concurrent  (threadDelay)
import Control.Monad       (when)
import System.Exit         (exitFailure)
import System.Process      (readProcess)
import Vis.Iob

main :: IO ()
main = do
  iobConnect (Hostname "localhost") "hsFoo"
  -- XXX If we change the order of the following two instructions, the test
  --     no longer fails.
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
    pvPath = "/Project/DP"
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

pioSet :: String -> String -> IO String
pioSet p v = readProcess "/opt/vispro/bin/pio" ["set", p, v] ""

pioGet :: String -> IO String
pioGet p =
    takeWhile (/= '\'') <$> dropWhile (== '\'')
    <$> readProcess "/opt/vispro/bin/pio" ["getval", p] ""
