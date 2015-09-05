module Two.Eleven where

import Crypto.Cipher.AES
import Data.Word
import System.Random (randomRIO)
import Control.Monad (replicateM, liftM, liftM2, liftM3)
import Control.Applicative (pure)

import AES (aesInit, ecbEncrypt, cbcEncrypt)
import StringByteConversion (bytesToHexString, charToByte)
import Padding (pkcs7)
import Util (groupsOfSize)

randomByte :: IO Word8
randomByte = randomRIO (0, 255)

randomByteString :: Int -> IO [Word8]
randomByteString = flip replicateM randomByte

randomLengthRandomByteString :: (Int, Int) -> IO [Word8]
randomLengthRandomByteString range = randomRIO range >>= randomByteString

-- 5-10 bytes on each end
randomPadding :: [Word8] -> IO [Word8]
randomPadding bs = liftM concat $ sequence [randomLengthRandomByteString (5, 10), pure bs, randomLengthRandomByteString (5, 10)]

randomAES :: IO AES
randomAES = (liftM aesInit) (randomByteString 16)

randomECB :: [Word8] -> IO [Word8]
randomECB bs = (liftM2 ecbEncrypt) randomAES (pure bs)

randomCBC :: [Word8] -> IO [Word8]
randomCBC bs = (liftM3 cbcEncrypt) randomAES (randomByteString 16) (pure bs)

randomSelect :: [a] -> IO a
randomSelect as = liftM (as !!) $ randomRIO (0, length as - 1)

-- 50/50 between ecb/cbc - keys/ivs are random 16-byte strings
randomEncrypt :: [Word8] -> IO [Word8]
randomEncrypt bs = (randomSelect [randomECB, randomCBC]) >>= (\f -> f bs)

noisyRandomEncrypt :: [Word8] -> IO [Word8]
noisyRandomEncrypt bs = do
  (f, str) <- randomSelect [(randomECB, "ECB"), (randomCBC, "CBC")]
  putStrLn str
  f bs

oracle :: [Word8] -> IO [Word8]
oracle bs = noisyRandomEncrypt =<< (liftM $ pkcs7 16) (randomPadding bs)

detectorString :: [Word8]
detectorString = take 48 $ repeat $ charToByte 'a'

hasEqualNeighbors :: Eq a => [a] -> Bool
hasEqualNeighbors (a1:a2:as)
  | a1 == a2  = True
  | otherwise = hasEqualNeighbors (a2:as)
hasEqualNeighbors _ = False

detectOracle :: Monad m => ([Word8] -> m [Word8]) -> m String
detectOracle f = do
  out <- f detectorString
  return $
    if hasEqualNeighbors (groupsOfSize 16 out)
      then "ECB"
      else "CBC"

main :: IO ()
main = putStrLn =<< (detectOracle oracle)
