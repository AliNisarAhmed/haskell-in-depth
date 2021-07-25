{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module QuoteData where

import Data.ByteString.Char8 (unpack)
import Data.Csv (FromField (..), FromNamedRecord)
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import GHC.Generics (Generic)

instance FromField Day where
  parseField = parseTimeM True defaultTimeLocale "%Y-%m-%d" . unpack

data QField
  = Open
  | Closed
  | High
  | Low
  | Volume
  deriving (Eq, Ord, Show, Enum, Bounded)

field2fun :: QField -> QuoteData -> Double
field2fun Open = open
field2fun Closed = close
field2fun High = high
field2fun Low = low
field2fun Volume = fromIntegral . volume

data QuoteData = QuoteData
  { day :: Day,
    volume :: Int,
    open :: Double,
    close :: Double,
    high :: Double,
    low :: Double
  }
  deriving (Generic, FromNamedRecord)