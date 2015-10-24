module Two.Twelve where

import Crypto.Cipher.AES
import Data.Word
import Data.List (find)
import Control.Monad (liftM)

import AES (ecbEncrypt)
import StringByteConversion (base64StringToBytes, bytesToString)
import Random (randomAES)
import Padding (pkcs7)
import Util (byteStringOfLength)
import Oracle
import OracleDetector

tailString :: String
tailString = "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"

tailBytes :: [Word8]
tailBytes = base64StringToBytes tailString

pad :: [Word8] -> [Word8]
pad as = pkcs7 16 $ concat [as, tailBytes]

makeOracle :: AES -> Oracle
makeOracle aes = (ecbEncrypt aes) . pad

makeRandomOracle :: IO Oracle
makeRandomOracle = makeOracle <$> randomAES

getBlockSize :: Oracle -> Int
getBlockSize oracle = fromInteger $ tFst (head $ tail lengthTransitions) - tFst (head lengthTransitions)
  where
    lengths = map (length . oracle . byteStringOfLength 'a') [0..]
    lengthTransitions = filter (\(_, l, nl) -> nl > l) $ zip3 [0..] (head lengths:lengths) lengths
    tFst = (\(i, _, _) -> i)

decryptChar :: Int -> Oracle -> [Word8] -> Maybe Word8
decryptChar blockSize oracle prefix = liftM (toEnum . snd) $ find ((== block) . fst) $ zip cmpBlocks [0..255]
  where
    inpLength =  (-1 - length prefix `mod` blockSize) + blockSize
    inp = byteStringOfLength 'a' inpLength
    getBlock = take blockSize . drop (((length prefix + inpLength) `div` blockSize) * blockSize)
    block = getBlock $ oracle inp
    cmpBlocks = map (\x -> getBlock $ oracle $ inp ++ prefix ++ [toEnum x]) [0..255]

whileMaybe :: ([a] -> Maybe a) -> [a]
whileMaybe = flip _whileMaybe []

_whileMaybe :: ([a] -> Maybe a) -> [a] -> [a]
_whileMaybe f as = case f as of
  Just a -> _whileMaybe f (as ++ [a])
  Nothing -> as

_decryptSuffixOracle :: Int -> Oracle -> [Word8]
_decryptSuffixOracle blockSize oracle = whileMaybe (decryptChar blockSize oracle)

decryptSuffixOracle :: Oracle -> Maybe [Word8]
decryptSuffixOracle oracle =
  if typ == ECB
    then Just $ _decryptSuffixOracle blockSize oracle
    else Nothing
  where
    blockSize = getBlockSize oracle
    typ = isECBOrCBC blockSize oracle

main :: IO ()
main = do
  oracle <- makeRandomOracle
  case decryptSuffixOracle oracle of
    Just x ->
      putStrLn $ bytesToString $ x
    Nothing ->
      putStrLn "ugh"
