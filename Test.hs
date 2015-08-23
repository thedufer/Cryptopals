import StringByteConversion (base64StringToBytes, bytesToBase64String)

dropNewlines :: String -> String
dropNewlines = filter (/= '\n')

main :: IO ()
main = (readFile "One/Six/input.txt") >>= putStrLn . bytesToBase64String . base64StringToBytes . dropNewlines
