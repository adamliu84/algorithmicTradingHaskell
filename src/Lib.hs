{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)
import Token as Lib (accessToken)

baseUrl :: String
baseUrl = "https://sandbox.iexapis.com/stable"

accessTokenParam :: String
accessTokenParam = concat ["&token=", accessToken]

data Quote = Quote
  { symbol :: String,
    companyName :: String,
    marketCap :: Int,
    open :: Double,
    latestPrice :: Double
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data Stats = Stats
  { year1ChangePercent :: Double,
    month6ChangePercent :: Double,
    month3ChangePercent :: Double,
    month1ChangePercent :: Double
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data Stock = Stock
  { stats :: Maybe Stats,
    quote :: Maybe Quote
  }
  deriving (Show, Generic, ToJSON, FromJSON)