module EnglishDetection (byteToEnglishScore, bytesToEnglishScore, highestScoringBytes) where

import Data.Word
import StringByteConversion (byteToChar)

highestScoringBytes :: [[Word8]] -> [Word8]
highestScoringBytes [] = []
highestScoringBytes bss = fst $ foldr1
  (\bs1 -> \bs2 ->
    if (snd bs1) > (snd bs2)
      then bs1
      else bs2
  )
  $ map
    (\bs -> (bs, bytesToEnglishScore bs))
    bss

byteToEnglishScore :: Word8 -> Integer
byteToEnglishScore = charToEnglishScore . byteToChar

bytesToEnglishScore :: [Word8] -> Integer
bytesToEnglishScore = sum . (map byteToEnglishScore)

charToEnglishScore :: Char -> Integer
charToEnglishScore 'A' = 201
charToEnglishScore 'a' = 201
charToEnglishScore 'B' = 135
charToEnglishScore 'b' = 135
charToEnglishScore 'C' = 147
charToEnglishScore 'c' = 147
charToEnglishScore 'D' = 163
charToEnglishScore 'd' = 163
charToEnglishScore 'E' = 240
charToEnglishScore 'e' = 240
charToEnglishScore 'F' = 143
charToEnglishScore 'f' = 143
charToEnglishScore 'G' = 140
charToEnglishScore 'g' = 140
charToEnglishScore 'H' = 179
charToEnglishScore 'h' = 179
charToEnglishScore 'I' = 193
charToEnglishScore 'i' = 193
charToEnglishScore 'J' = 121
charToEnglishScore 'j' = 121
charToEnglishScore 'K' = 127
charToEnglishScore 'k' = 127
charToEnglishScore 'L' = 160
charToEnglishScore 'l' = 160
charToEnglishScore 'M' = 146
charToEnglishScore 'm' = 146
charToEnglishScore 'N' = 190
charToEnglishScore 'n' = 190
charToEnglishScore 'O' = 197
charToEnglishScore 'o' = 197
charToEnglishScore 'P' = 138
charToEnglishScore 'p' = 138
charToEnglishScore 'Q' = 121
charToEnglishScore 'q' = 121
charToEnglishScore 'R' = 180
charToEnglishScore 'r' = 180
charToEnglishScore 'S' = 183
charToEnglishScore 's' = 183
charToEnglishScore 'T' = 211
charToEnglishScore 't' = 211
charToEnglishScore 'U' = 149
charToEnglishScore 'u' = 149
charToEnglishScore 'V' = 131
charToEnglishScore 'v' = 131
charToEnglishScore 'W' = 141
charToEnglishScore 'w' = 141
charToEnglishScore 'X' = 122
charToEnglishScore 'x' = 122
charToEnglishScore 'Y' = 141
charToEnglishScore 'y' = 141
charToEnglishScore 'Z' = 121
charToEnglishScore 'z' = 121
charToEnglishScore ' ' = 120
charToEnglishScore '\n' = 0
charToEnglishScore '.' = 0
charToEnglishScore '!' = 0
charToEnglishScore '@' = 0
charToEnglishScore '#' = 0
charToEnglishScore '$' = 0
charToEnglishScore '%' = 0
charToEnglishScore '^' = 0
charToEnglishScore '&' = 0
charToEnglishScore '*' = 0
charToEnglishScore '(' = 0
charToEnglishScore ')' = 0
charToEnglishScore '-' = 0
charToEnglishScore '=' = 0
charToEnglishScore '_' = 0
charToEnglishScore '+' = 0
charToEnglishScore '[' = 0
charToEnglishScore ']' = 0
charToEnglishScore '{' = 0
charToEnglishScore '}' = 0
charToEnglishScore ':' = 0
charToEnglishScore ';' = 0
charToEnglishScore '"' = 0
charToEnglishScore '\'' = 0
charToEnglishScore '<' = 0
charToEnglishScore ',' = 0
charToEnglishScore '>' = 0
charToEnglishScore '?' = 0
charToEnglishScore '/' = 0
charToEnglishScore '|' = 0
charToEnglishScore '\\' = 0
charToEnglishScore '~' = 0
charToEnglishScore '`' = 0
charToEnglishScore _ = -100
