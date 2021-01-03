{-# LANGUAGE OverloadedStrings #-}

module Momentum
  ( query,
  )
where

import Control.Monad (forM)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.List (intercalate, sortBy)
import Data.Maybe (mapMaybe)
import GHC.Float (int2Double)
import Lib (Stock, accessTokenParam, baseUrl, latestPrice, marketCap, month1ChangePercent, month3ChangePercent, month6ChangePercent, quote, stats, symbol, year1ChangePercent)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Util (dirtyConverter, getConsituents, getiexapisData, groupByTen)

tempPortfolioSize :: Double
tempPortfolioSize = 10000000

topFiftyMomentumStock :: Int
topFiftyMomentumStock = 50

data MomentumStock = MomentumStock
  { momentumSymbol :: String,
    momentumLatestPrice :: Double,
    momentumYear1ChangePercent :: Double,
    momentumMonth6ChangePercent :: Double,
    momentumMonth3ChangePercent :: Double,
    momentumMonth1ChangePercent :: Double,
    hqmScore :: Double
  }
  deriving (Show)

query :: IO ()
query = do
  manager <- newManager tlsManagerSettings
  consituents <- getConsituents
  let batchSymbol = groupByTen consituents
  stock <-
    (return . concat)
      =<< forM
        batchSymbol
        (getiexapisData manager)
  let momentumStock = mapMaybe getTimePeriodPriceReturn stock
      result =
        map
          (\s -> (momentumSymbol s, momentumLatestPrice s, calNumOfSharesToBuy topFiftyMomentumStock s))
          $ take topFiftyMomentumStock $ -- Selecting the 50 Best Momentum Stocks
            sortBy (\b a -> hqmScore a `compare` hqmScore b) $
              map (`calHqmScore` momentumStock) momentumStock
  mapM_ print result --TODO Just write into a temp csv...
  where
    calNumOfSharesToBuy i s = floor $ (tempPortfolioSize / fromIntegral i) / momentumLatestPrice s

getTimePeriodPriceReturn :: Stock -> Maybe MomentumStock
getTimePeriodPriceReturn s = do
  stats' <- stats s
  quote' <- quote s
  return $
    MomentumStock
      { momentumSymbol = symbol quote',
        momentumLatestPrice = latestPrice quote',
        momentumYear1ChangePercent = year1ChangePercent stats',
        momentumMonth6ChangePercent = month6ChangePercent stats',
        momentumMonth3ChangePercent = month3ChangePercent stats',
        momentumMonth1ChangePercent = month1ChangePercent stats',
        hqmScore = 0
      }

calHqmScore :: MomentumStock -> [MomentumStock] -> MomentumStock
calHqmScore s ms =
  let y1pt = getPercentiles (momentumYear1ChangePercent s) (map momentumYear1ChangePercent ms)
      m6pt = getPercentiles (momentumMonth6ChangePercent s) (map momentumMonth6ChangePercent ms)
      m3pt = getPercentiles (momentumMonth3ChangePercent s) (map momentumMonth3ChangePercent ms)
      m1pt = getPercentiles (momentumMonth1ChangePercent s) (map momentumMonth1ChangePercent ms)
   in s {hqmScore = sum [y1pt, m6pt, m3pt, m1pt] / 4}
  where
    getPercentiles :: Double -> [Double] -> Double
    getPercentiles c l = (fromIntegral $ length $ filter (<= c) l) / (fromIntegral $ length l) * 100
