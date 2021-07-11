module Main where

import MyLib

main :: IO ()
main = do
  let testJsonWithEscapes = "{\"test\": \"e\\\"wat\\n asdf\\\\ \\f \\b \\r\"}"
  case snd <$> runParser jsonValue testJsonWithEscapes of
    Just x -> putStrLn $ prettyPrint x
    Nothing -> putStrLn "Failed to parse :("