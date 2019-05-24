import Data.Time.Clock
import Test.Hspec
import Parser
import Data.Time.Calendar

main :: IO ()
main = hspec $ do
  describe "DateTimeParse" $ do
    it "can parse a date" $ do
      parseDateTime "1999-06-05-12:00-AM" 
      `shouldBe` 
      (Just $ ParsedDate $ UTCTime 
                      { utctDay = fromGregorian 1999 6 5,
                        utctDayTime = secondsToDiffTime 0
                      })

  describe "PhoneNumberParse" $ do
    it "can parse a phonenumber" $ do
      parsePhoneNumber "555-555-5555" `shouldBe` (Just $ ParsedPhoneNumber "5555555555")
  describe "Words" $ do
    it "can parse a word" $ do
      parseWord "Test" `shouldBe` (Just $ ParsedWord "Test")
  describe "Numbers" $ do
    it "can parse a number" $ do
      parseNumber "123" `shouldBe` (Just $ ParsedNumber 123)
  describe "Parse Everything" $ do
    it "can parse everything" $ do
       parseInput "1999-06-05-12:00-AM 555-555-5555 Test 123"
       `shouldBe`
       (Just $ [
                ParsedDate $ UTCTime { utctDay = fromGregorian 1999 6 5, utctDayTime = secondsToDiffTime 0 },
                ParsedPhoneNumber "5555555555",
                ParsedWord "Test",
                ParsedNumber 123
               ])
