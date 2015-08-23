module AES (aesInit, ecbDecrypt, cbcDecrypt, ecbEncrypt, cbcEncrypt) where

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
ecbEncrypt aes = B.unpack . encryptECB aes . B.pack

cbcBlockDecrypt :: AES -> [Word8] -> [Word8] -> [Word8]
cbcBlockDecrypt aes iv cipher = longXor (ecbDecrypt aes cipher) iv

cbcDecrypt :: AES -> [Word8] -> [Word8] -> [Word8]
cbcDecrypt aes iv cipher =
  let blockedCipher = (groupsOfSize 16 cipher)
  in concat $ map (uncurry $ cbcBlockDecrypt aes) $ zip (iv:blockedCipher) blockedCipher

cbcBlockEncrypt :: AES -> [Word8] -> [Word8] -> [Word8]
cbcBlockEncrypt aes iv plain = ecbEncrypt aes $ longXor plain iv

cbcEncrypt :: AES -> [Word8] -> [Word8] -> [Word8]
cbcEncrypt aes iv plain = concat $ drop 1 $ reverse $ foldl (\ciphers -> \plain -> (cbcBlockEncrypt aes (head ciphers) plain):ciphers) [iv] (groupsOfSize 16 plain)
