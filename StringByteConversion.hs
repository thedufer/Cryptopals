module StringByteConversion (hexStringToBytes, bytesToBase64String, bytesToHexString, byteToChar, charToByte, bytesToString, stringToBytes, base64StringToBytes) where

import Data.Word
import Data.Bits (shift, (.&.), (.|.))
import Data.List (genericIndex, elemIndex)
import Data.Maybe (fromJust)

bytesToString :: [Word8] -> String
bytesToString = map byteToChar

stringToBytes :: String -> [Word8]
stringToBytes = map charToByte

charToByte :: Char -> Word8
charToByte = toEnum . fromEnum

byteToChar :: Word8 -> Char
byteToChar = toEnum . fromEnum

hexDigitToNum :: Num a => Char -> a
hexDigitToNum '0' = 0
hexDigitToNum '1' = 1
hexDigitToNum '2' = 2
hexDigitToNum '3' = 3
hexDigitToNum '4' = 4
hexDigitToNum '5' = 5
hexDigitToNum '6' = 6
hexDigitToNum '7' = 7
hexDigitToNum '8' = 8
hexDigitToNum '9' = 9
hexDigitToNum 'a' = 10
hexDigitToNum 'b' = 11
hexDigitToNum 'c' = 12
hexDigitToNum 'd' = 13
hexDigitToNum 'e' = 14
hexDigitToNum 'f' = 15

numToHexDigit :: (Eq a, Num a) => a -> Char
numToHexDigit 0 = '0'
numToHexDigit 1 = '1'
numToHexDigit 2 = '2'
numToHexDigit 3 = '3'
numToHexDigit 4 = '4'
numToHexDigit 5 = '5'
numToHexDigit 6 = '6'
numToHexDigit 7 = '7'
numToHexDigit 8 = '8'
numToHexDigit 9 = '9'
numToHexDigit 10 = 'a'
numToHexDigit 11 = 'b'
numToHexDigit 12 = 'c'
numToHexDigit 13 = 'd'
numToHexDigit 14 = 'e'
numToHexDigit 15 = 'f'

hexPairToByte :: Char -> Char -> Word8
hexPairToByte hi lo = (shift (hexDigitToNum hi) 4) + (hexDigitToNum lo)

byteToHexPair :: Word8 -> [Char]
byteToHexPair a = map numToHexDigit [shift a (-4), a .&. 0xf]

hexStringToBytes :: String -> [Word8]
hexStringToBytes [] = []
hexStringToBytes (hi:lo:rs) = (hexPairToByte hi lo):(hexStringToBytes rs)

bytesToHexString :: [Word8] -> String
bytesToHexString [] = []
bytesToHexString (a:rs) = (byteToHexPair a) ++ (bytesToHexString rs)

base64String = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
sextetToChar :: Word8 -> Char
sextetToChar c = base64String `genericIndex` c

sextetsToChars :: [Word8] -> [Char]
sextetsToChars = map sextetToChar

firstSextet a b c = shift a (-2)
secondSextet a b c = (shift (a .&. 0x3) 4) .|. (shift b (-4))
thirdSextet a b c = (shift (b .&. 0xf) 2) .|. (shift c (-6))
fourthSextet a b c = c .&. 0x3f

bytesToBase64String :: [Word8] -> String
bytesToBase64String [] = ""
bytesToBase64String (a:[]) = (sextetsToChars [firstSextet a 0 0, secondSextet a 0 0]) ++ "=="
bytesToBase64String (a:b:[]) = (sextetsToChars [firstSextet a b 0, secondSextet a b 0, thirdSextet a b 0]) ++ "="
bytesToBase64String (a:b:c:rs) = (sextetsToChars [firstSextet a b c, secondSextet a b c, thirdSextet a b c, fourthSextet a b c]) ++ (bytesToBase64String rs)

charToSextet :: Char -> Word8
charToSextet = (toEnum . fromEnum) . fromJust . (flip elemIndex base64String)

sextetsToBytes :: [Word8] -> [Word8]
sextetsToBytes (a:b:c:d:[]) = [(shift a 2) .|. (shift b (-4)), (shift b 4) .|. (shift c (-2)), (shift c 6) .|. d]

base64StringToBytes :: String -> [Word8]
base64StringToBytes [] = []
base64StringToBytes (a:b:'=':'=':[]) = take 1 $ sextetsToBytes ((map charToSextet [a, b]) ++ [0, 0])
base64StringToBytes (a:b:c:'=':[]) = take 2 $ sextetsToBytes ((map charToSextet [a, b, c]) ++ [0])
base64StringToBytes (a:b:c:d:rs) =  sextetsToBytes (map charToSextet [a,b,c,d]) ++ base64StringToBytes rs
