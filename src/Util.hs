{-# LANGUAGE OverloadedStrings #-}

module Util where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Text as T (drop, dropWhile, length, pack, replace, splitOn, take, unpack)
import Lib (Quote)

getConsituents :: IO [String]
getConsituents = tail . takeWhile'' . lines <$> readFile "constituents.csv"
  where
    takeWhile'' = map $ takeWhile (/= ',')

group :: Int -> [a] -> [[a]]
group _ [] = [[]]
group n xs = take n xs : group n (drop n xs)

groupByTen :: [a] -> [[a]]
groupByTen = group 10

{-
To correct this weird convertor
-}
dirtyConverter :: LB.ByteString -> [Quote]
dirtyConverter str =
  mapMaybe ((\x -> decode x :: Maybe Quote) . f') $
    T.splitOn "}," $
      T.drop 1 $
        T.take (T.length str' - 2) str'
  where
    str' = T.pack $ LB.unpack str
    f' = LB.pack . T.unpack . T.drop 9 . T.dropWhile (/= '{')