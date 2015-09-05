module Random (randomAES, randomByteString, randomByte) where

import Data.Word
import Crypto.Cipher.AES
import Control.Monad (liftM, replicateM)
import System.Random (randomRIO)

import AES

randomByte :: IO Word8
randomByte = randomRIO (0, 255)

randomByteString :: Int -> IO [Word8]
randomByteString = flip replicateM randomByte

randomAES :: IO AES
randomAES = (liftM aesInit) (randomByteString 16)
