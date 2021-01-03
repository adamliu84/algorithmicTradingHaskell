{-# LANGUAGE OverloadedStrings #-}

module SPFive
  ( query,
  )
where

import Control.Monad (forM)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import GHC.Float (int2Double)
import Lib (Quote, accessTokenParam, baseUrl, latestPrice, marketCap, quote, symbol)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Util (dirtyConverter, getConsituents, getiexapisData, groupByTen)

tempPortfolioSize :: Double
tempPortfolioSize = 10000000

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
  let indexLength = int2Double $ length stock
      result =
        mapMaybe
          ( \s -> do
              q' <- quote s
              return (symbol q', latestPrice q', calNumOfSharesToBuy indexLength q')
          )
          stock
  mapM_ print result --TODO Just write into a temp csv...
  where
    calNumOfSharesToBuy i s = floor $ (tempPortfolioSize / i) / latestPrice s