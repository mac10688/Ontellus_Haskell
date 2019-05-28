module Main where

import Parser
import System.Environment
import Data.Foldable
import Data.List
import Data.Ord
import Merge

main :: IO ()
main = do
  args <- getArgs
  let parsedItems = parseInput args
  putStrLn "Dates:"
  let dates = msort $ filter (\case ParsedDate _ -> True; otherwise -> False) $ parsedItems
  forM_ dates print
  putStrLn "\n\nPhoneNumbers:"
  let phoneNumbers = msort $ filter (\case ParsedPhoneNumber _ -> True; otherwise -> False) $ parsedItems
  forM_ phoneNumbers print
  putStrLn "\n\nWords:"
  let words = msort $ filter (\case ParsedWord _ -> True; otherwise -> False) $ parsedItems
  forM_ words print
  putStrLn "\n\nNumbers"
  let numbers = msort $ filter (\case ParsedNumber _ -> True; otherwise -> False) $ parsedItems
  forM_ numbers print

