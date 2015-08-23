module OneSeven where

import Crypto.Cipher.AES
import StringByteConversion
import Util (dropNewlines)
import qualified Data.ByteString as B

key = B.pack $ stringToBytes "YELLOW SUBMARINE"

main :: IO ()
main = (readFile "One/Seven.txt") >>= putStrLn . bytesToString . B.unpack . (decryptECB $ initAES key) . B.pack . base64StringToBytes . dropNewlines
