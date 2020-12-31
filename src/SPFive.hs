{-# LANGUAGE OverloadedStrings #-}

module SPFive
  ( query,
  )
where

import Control.Monad (forM)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.List (intercalate)
import GHC.Float (int2Double)
import Lib (Quote, accessTokenParam, baseUrl, latestPrice, marketCap, symbol)
import Network.HTTP.Client
  ( Request (method),
    Response (responseBody),
    httpLbs,
    newManager,
    parseRequest,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Util (dirtyConverter, getConsituents, groupByTen)

tempPortfolioSize :: Double
tempPortfolioSize = 10000000

query :: IO ()
query = do
  manager <- newManager tlsManagerSettings
  consituents <- getConsituents
  let batchSymbol = map (intercalate ",") $ groupByTen consituents
  stock <-
    (return . concat)
      =<< forM
        batchSymbol
        ( \x -> do
            initialRequest <-
              parseRequest $
                concat [baseUrl, "/stock/market/batch/?types=quote&symbols=", x, accessTokenParam]
            let request = initialRequest {method = "GET"}
            response <- httpLbs request manager
            return $ dirtyConverter $ responseBody response
        )
  let indexLength = int2Double $ length stock
      result = map (\s -> (symbol s, latestPrice s, calNumOfSharesToBuy indexLength s)) stock
  mapM_ print result --TODO Just write into a temp csv...
  where
    calNumOfSharesToBuy i s = floor $ (tempPortfolioSize / i) / latestPrice s