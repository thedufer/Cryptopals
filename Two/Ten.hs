module TwoTen where

import StringByteConversion (charToByte, base64StringToBytes, bytesToString, stringToBytes)
import Data.Word
import Util (groupsOfSize, dropNewlines)
import Crypto.Cipher.AES
import Xor (longXor)
import qualified Data.ByteString as B

key = B.pack $ stringToBytes "YELLOW SUBMARINE"
iv :: [Word8]
iv = take 16 $ repeat $ charToByte '\x00'

cbcBlockDecrypt :: AES -> [Word8] -> [Word8] -> [Word8]
cbcBlockDecrypt aes iv cipher = longXor (B.unpack $ decryptECB aes $ B.pack cipher) iv

cbcDecrypt :: AES -> [Word8] -> [Word8] -> [Word8]
cbcDecrypt aes iv cipher =
  let blockedCipher = (groupsOfSize 16 cipher)
  in concat $ map (uncurry $ cbcBlockDecrypt aes) $ zip (iv:blockedCipher) blockedCipher

main :: IO ()
main = (readFile "Two/Ten.txt") >>= putStrLn . bytesToString . (cbcDecrypt (initAES key) iv) . base64StringToBytes . dropNewlines
