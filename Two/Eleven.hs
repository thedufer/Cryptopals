module Two.Eleven where

import Data.Word
import System.Random (randomRIO)
import Control.Monad (liftM, liftM2, liftM3)

import AES (ecbEncrypt, cbcEncrypt)
import Random (randomAES, randomByteString)
import Padding (pkcs7)
import Oracle
import OracleDetector

randomLengthRandomByteString :: (Int, Int) -> IO [Word8]
randomLengthRandomByteString range = randomRIO range >>= randomByteString

-- 5-10 bytes on each end
randomPadding :: [Word8] -> IO [Word8]
randomPadding bs = liftM concat $ sequence [randomLengthRandomByteString (5, 10), pure bs, randomLengthRandomByteString (5, 10)]

randomECB :: [Word8] -> IO [Word8]
randomECB bs = (liftM2 ecbEncrypt) randomAES (pure bs)

randomCBC :: [Word8] -> IO [Word8]
randomCBC bs = (liftM3 cbcEncrypt) randomAES (randomByteString 16) (pure bs)

randomSelect :: [a] -> IO a
randomSelect as = liftM (as !!) $ randomRIO (0, length as - 1)

-- 50/50 between ecb/cbc - keys/ivs are random 16-byte strings
randomEncrypt :: MOracle IO
randomEncrypt bs = (randomSelect [randomECB, randomCBC]) >>= ($ bs)

noisyRandomEncrypt :: MOracle IO
noisyRandomEncrypt bs = do
  (f, typ) <- randomSelect [(randomECB, ECB), (randomCBC, CBC)]
  putStrLn $ show typ
  f bs

oracle :: MOracle IO
oracle bs = noisyRandomEncrypt =<< (liftM $ pkcs7 16) (randomPadding bs)

main :: IO ()
main = putStrLn . show =<< (mIsECBOrCBC 16 oracle)
