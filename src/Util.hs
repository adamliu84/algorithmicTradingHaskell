{-# LANGUAGE OverloadedStrings #-}

module Util where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.List (intercalate)
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Text as T (breakOn, concat, drop, dropWhile, length, pack, splitOn, take, unpack)
import Lib (Quote, Stats, Stock, accessTokenParam, baseUrl)
import Network.HTTP.Client (Manager, Request (method), Response (responseBody), httpLbs, parseRequest)

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
READLLY NEED TO correct this weird convertor
-}
dirtyConverter :: LB.ByteString -> [Stock]
dirtyConverter str =
  mapMaybe ((\x -> decode (LB.pack $ T.unpack x) :: Maybe Stock) . (\x -> T.concat [T.drop 1 $ snd $ T.breakOn ":" x, "}}"])) $
    init $
      T.splitOn "}}" $
        T.drop 1 $ T.take (T.length str' - 1) str'
  where
    str' = T.pack $ LB.unpack str

getiexapisData :: Manager -> [String] -> IO [Stock]
getiexapisData manager symbols = do
  let symbols' = intercalate "," symbols
  initialRequest <-
    parseRequest $
      concat [baseUrl, "/stock/market/batch/?types=stats,quote&symbols=", symbols', accessTokenParam]
  let request = initialRequest {method = "GET"}
  response <- httpLbs request manager
  return $ dirtyConverter $ responseBody response