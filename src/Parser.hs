module Parser
    ( 
      parse,
      parseDateTime,
      ParseResult (..),
      parsePhoneNumber,
      parseWord,
      parseNumber,
      parseInput
    ) where

import Data.Time.Format
import Control.Applicative
import Data.Time.Clock
import Text.RawString.QQ
import Text.Read
import Data.List

import Text.Regex.TDFA

parseInput :: [String] -> [ParseResult]
parseInput = foldr (\string acc -> case parse string of (Just x) -> x : acc; otherwise -> acc) []

parse :: String -> Maybe ParseResult
parse string = parseDateTime string <|> parsePhoneNumber string <|> parseWord string <|> parseNumber string

parseDateTime :: String -> Maybe ParseResult
parseDateTime dateString = ParsedDate <$> (parseTimeM True defaultTimeLocale "%F-%l:%M-%p" dateString :: Maybe UTCTime)

parsePhoneNumber :: String -> Maybe ParseResult
parsePhoneNumber phoneString =
  let stringRegex = [r|^(\([0-9]{3}\)|([0-9]{3})-)?[0-9]{3}-[0-9]{4}$|]
  in
    case phoneString =~ stringRegex :: (String, String, String) of
      ([], xs, []) -> Just $ ParsedPhoneNumber $ filterNumbers phoneString
      otherwise -> Nothing
  where
    filterNumbers :: String -> String
    filterNumbers string = concat $ (getAllTextMatches (string =~ "[0-9]+") :: [String])

parseWord :: String -> Maybe ParseResult
parseWord wordString =
  let stringRegex = "^[a-zA-Z]+$"
  in
    case wordString =~ stringRegex :: (String, String, String) of
      ([], xs, []) -> Just $ ParsedWord $ xs
      otherwise -> Nothing

parseNumber :: String -> Maybe ParseResult
parseNumber numberString = ParsedNumber <$> (readMaybe numberString :: Maybe Double)

data ParseResult = 
  ParsedDate UTCTime
  | ParsedPhoneNumber String
  | ParsedWord String
  | ParsedNumber Double
  deriving (Eq, Ord)

instance Show ParseResult where
  show (ParsedDate time) = show time
  show (ParsedPhoneNumber phoneNumber) = "(" ++ take 3 phoneNumber ++ ") " ++ (take 3 $ drop 3 phoneNumber) ++ "-" ++ drop 6 phoneNumber
  show (ParsedWord word) = word
  show (ParsedNumber number) = show number
