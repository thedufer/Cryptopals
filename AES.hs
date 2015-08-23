module AES (aesInit, ecbDecrypt, cbcDecrypt, ecbEncrypt) where

import Crypto.Cipher.AES
import Data.Word
import Util (groupsOfSize)
import Xor (longXor)
import qualified Data.ByteString as B

aesInit :: [Word8] -> AES
aesInit = initAES . B.pack

ecbDecrypt :: AES -> [Word8] -> [Word8]
ecbDecrypt aes = B.unpack . decryptECB aes . B.pack

ecbEncrypt :: AES -> [Word8] -> [Word8]
ecbEncrypt aes = B.unpack . decryptECB aes . B.pack

cbcBlockDecrypt :: AES -> [Word8] -> [Word8] -> [Word8]
cbcBlockDecrypt aes iv cipher = longXor (ecbDecrypt aes cipher) iv

cbcDecrypt :: AES -> [Word8] -> [Word8] -> [Word8]
cbcDecrypt aes iv cipher =
  let blockedCipher = (groupsOfSize 16 cipher)
  in concat $ map (uncurry $ cbcBlockDecrypt aes) $ zip (iv:blockedCipher) blockedCipher
