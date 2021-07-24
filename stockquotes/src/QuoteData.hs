{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module QuoteData where

import Data.ByteString.Char8 (unpack)
import Data.Csv (FromField (..), FromNamedRecord)
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import GHC.Generics (Generic)

instance FromField Day where
  parseField = parseTimeM True defaultTimeLocale "%Y-%m-%d" . unpack

data QuoteData = QuoteData
  { day :: Day,
    volume :: Int,
    open :: Double,
    close :: Double,
    high :: Double,
    low :: Double
  }
  deriving (Generic, FromNamedRecord)