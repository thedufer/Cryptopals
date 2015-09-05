module Two.Ten where

import AES (cbcDecrypt, aesInit)
import StringByteConversion (charToByte, base64StringToBytes, bytesToString, stringToBytes)
import Data.Word
import Util (dropNewlines)

key = stringToBytes "YELLOW SUBMARINE"
iv :: [Word8]
iv = take 16 $ repeat $ charToByte '\x00'

main :: IO ()
main = (readFile "Two/Ten.txt") >>= putStrLn . bytesToString . (cbcDecrypt (aesInit key) iv) . base64StringToBytes . dropNewlines
