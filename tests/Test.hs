module Main where

import qualified Data.Map as M
import MyLib
import System.Exit (exitFailure)
import Test.Hspec

main :: IO ()
main = do
  testBasicParsers
  testJsonParsers

testBasicParsers = hspec $ do
  describe "charP" $ do
    it "Parses a single character" $ do
      runParser (charP 'a') "a" `shouldBe` Just ("", 'a')
    it "Parses a single character in a multi character string" $ do
      runParser (charP 'a') "ab" `shouldBe` Just ("b", 'a')
    it "Fails when it encounters a different character" $ do
      runParser (charP 'a') "ba" `shouldBe` Nothing

  describe "stringP" $ do
    it "Parses a string" $ do
      runParser (stringP "wowee") "wowee" `shouldBe` Just ("", "wowee")
    it "Parses a string followed by more string" $ do
      runParser (stringP "string") "stringThisStringIsMadeOf" `shouldBe` Just ("ThisStringIsMadeOf", "string")
    it "Fails on a partial match" $ do
      runParser (stringP "ohnooo") "ohno" `shouldBe` Nothing

  describe "whitespace" $ do
    it "Parses a single space" $ do
      runParser whitespace " " `shouldBe` Just ("", " ")
    it "Parses a many spaces" $ do
      runParser whitespace "                             " `shouldBe` Just ("", "                             ")
    it "Parses newlines" $ do
      runParser whitespace "\n" `shouldBe` Just ("", "\n")
    it "Parses only whitespace" $ do
      runParser whitespace "   nonwhitespace" `shouldBe` Just ("nonwhitespace", "   ")

testJsonParsers :: IO ()
testJsonParsers = hspec $ do
  describe "jsonNull" $ do
    it "Parses null" $ do
      runParser jsonNull "null" `shouldBe` Just ("", JsonNull)

  describe "jsonBool" $ do
    it "Parses true" $ do
      runParser jsonBool "true" `shouldBe` Just ("", JsonBool True)
    it "Parses false" $ do
      runParser jsonBool "false" `shouldBe` Just ("", JsonBool False)

  describe "jsonNumber" $ do
    it "Parses an integer" $ do
      runParser jsonNumber "42" `shouldBe` Just ("", JsonNumber 42)

  describe "jsonString" $ do
    it "Parses a simple string" $ do
      runParser jsonString "\"what a simple string\"" `shouldBe` Just ("", JsonString "what a simple string")
    it "Parses a string with an escaped double quote" $ do
      runParser jsonString "\"\\\"\"" `shouldBe` Just ("", JsonString ['"'])
    it "Parses a string in a string" $ do
      runParser jsonString "\"\\\"recursion\\\"\"" `shouldBe` Just ("", JsonString $ ['"'] <> "recursion" <> ['"'])
    it "Parses the other valid JSON string escape characters" $ do
      runParser jsonString "\"\\t\\r\\n\\b\\f\\\\\"" `shouldBe` Just ("", JsonString "\t\r\n\b\f\\")
    it "Parses with quotes in the middle of non-escaped characters" $ do
      runParser jsonString "\"asdf\\\"asdf\"" `shouldBe` Just ("", JsonString "asdf\"asdf")
    it "Fails when missing beginning quote" $ do
      runParser jsonString "I have no idea where to start\"" `shouldBe` Nothing
    it "Fails when missing end quote" $ do
      runParser jsonString "\"aaaaaaaaaaaaa" `shouldBe` Nothing
    it "Doesn't parse bare characters" $ do
      runParser jsonString "asdflasdf" `shouldBe` Nothing

    describe "jsonArray" $ do
      it "It parses an empty array" $ do
        runParser jsonArray "[]" `shouldBe` Just ("", JsonArray [])
      it "It parses an array with a single value" $ do
        runParser jsonArray "[1]" `shouldBe` Just ("", JsonArray [JsonNumber 1])
      it "It parses an array with multiple values" $ do
        runParser jsonArray "[1, 2, 3, 4]" `shouldBe` Just ("", JsonArray [JsonNumber 1, JsonNumber 2, JsonNumber 3, JsonNumber 4])
      it "It parses a nested array with multiple values" $ do
        runParser jsonArray "[[1, 2, 3, 4]]" `shouldBe` Just ("", JsonArray [JsonArray [JsonNumber 1, JsonNumber 2, JsonNumber 3, JsonNumber 4]])
      it "It parses an array with multiple values of different types" $ do
        runParser jsonArray "[null, true, false, 42, \"oof\", {}]"
          `shouldBe` Just
            ( "",
              JsonArray
                [ JsonNull,
                  JsonBool True,
                  JsonBool False,
                  JsonNumber 42,
                  JsonString "oof",
                  JsonObject M.empty
                ]
            )
