module Main where

import Parser
import System.Environment
import Data.Foldable
import Data.List
import Data.Ord

main :: IO ()
main = do
  args <- getArgs
  let parsedItems = parseInput args
  putStrLn "Dates:"
  let dates = sort $ filter (\case ParsedDate _ -> True; otherwise -> False) $ parsedItems
  forM_ dates print
  putStrLn "\n\nPhoneNumbers:"
  let phoneNumbers = sort $ filter (\case ParsedPhoneNumber _ -> True; otherwise -> False) $ parsedItems
  forM_ phoneNumbers print
  putStrLn "\n\nWords:"
  let words = sort $ filter (\case ParsedWord _ -> True; otherwise -> False) $ parsedItems
  forM_ words print
  putStrLn "\n\nNumbers"
  let numbers = sort $ filter (\case ParsedNumber _ -> True; otherwise -> False) $ parsedItems
  forM_ numbers print
  -- putStrLn "Dates:"
  -- forM_ parsedItems print


  -- let dates = 
  -- case parsedItems of
    -- Just xs -> foldl 

filterDates :: ParseResult -> Bool
filterDates (ParsedDate date) = True
filterDates _ = False
