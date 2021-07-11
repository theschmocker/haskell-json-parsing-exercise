{-# LANGUAGE LambdaCase #-}

module MyLib where

import Control.Applicative
import Data.Char
import Data.List (intercalate, intersperse)
import qualified Data.Map as M

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer -- TODO: at least handle decimal numbers
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject (M.Map String JsonValue)
  deriving (Eq, Show)

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

instance Functor Parser where
  fmap f (Parser p) =
    Parser
      ( \s -> do
          (remaining, a) <- p s
          pure (remaining, f a)
      )

instance Applicative Parser where
  pure a = Parser (\s -> Just (s, a))
  (<*>) (Parser p1) (Parser p2) =
    Parser
      ( \s -> do
          (remaining, f) <- p1 s
          (remaining', a) <- p2 remaining
          pure (remaining', f a)
      )

instance Alternative Parser where
  empty = Parser $ const Nothing
  (<|>) (Parser p1) (Parser p2) =
    Parser
      ( \s -> do
          p1 s <|> p2 s
      )

prettyPrint :: JsonValue -> String
prettyPrint JsonNull = "null"
prettyPrint (JsonBool True) = "true"
prettyPrint (JsonBool False) = "false"
prettyPrint (JsonString string) = show string
prettyPrint (JsonNumber num) = show num
prettyPrint (JsonArray xs) = "[" <> intercalate ", " (map prettyPrint xs) <> "]"
-- TODO: properly indent nested objects
prettyPrint (JsonObject m) = "{\n\t" <> intercalate ",\n\t" (map printKV (M.toList m)) <> "\n}"
  where
    printKV (k, v) = "\t" <> show k <> ": " <> prettyPrint v

jsonValue :: Parser JsonValue
jsonValue =
  jsonNull
    <|> jsonNumber
    <|> jsonBool
    <|> jsonString
    <|> jsonArray
    <|> jsonObject

charP :: Char -> Parser Char
charP c = charIf (== c)

charIf :: (Char -> Bool) -> Parser Char
charIf pred =
  Parser
    ( \case
        [] -> Nothing
        (x : xs)
          | pred x -> Just (xs, x)
          | otherwise -> Nothing
    )

normalChar :: Parser Char
normalChar = charIf (liftA2 (&&) (/= '"') (/= '\\'))

escapedChar :: Parser Char
escapedChar =
  '"' <$ stringP "\\\""
    <|> '\n' <$ stringP "\\n"
    <|> '\b' <$ stringP "\\b"
    <|> '\t' <$ stringP "\\t"
    <|> '\r' <$ stringP "\\r"
    <|> '\f' <$ stringP "\\f"
    <|> '\\' <$ stringP "\\\\"

stringP :: String -> Parser String
stringP = traverse charP

sepBy :: Parser String -> Parser b -> Parser [b]
sepBy sep parser = many (parser <* (sep <|> pure []))

commaSeparated :: Parser b -> Parser [b]
commaSeparated = sepBy (whitespace *> stringP "," *> whitespace)

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"

jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber . read <$> some (charIf isDigit)

jsonBool :: Parser JsonValue
jsonBool = jsonTrue <|> jsonFalse
  where
    jsonTrue = JsonBool True <$ stringP "true"
    jsonFalse = JsonBool False <$ stringP "false"

jsonString :: Parser JsonValue
jsonString =
  JsonString
    <$> (charP '"' *> many (normalChar <|> escapedChar) <* charP '"')

whitespace :: Parser String
whitespace = many (charIf isSpace)

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> whitespace *> commaSeparated jsonValue <* whitespace <* charP ']')

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charP '{' *> whitespace *> (M.fromList <$> commaSeparated pair) <* whitespace <* charP '}')
  where
    pair = extract <$> key <*> colon <*> jsonValue
    key = charP '"' *> many (charIf (/= '"')) <* charP '"'
    colon = whitespace *> charP ':' *> whitespace
    extract k _ v = (k, v)