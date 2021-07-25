module Lib where

import qualified Data.ByteString.Lazy as BL
import Data.Csv (decodeByName)
import Data.Foldable (toList)
import QuoteData

readQuotes :: FilePath -> IO [QuoteData]
readQuotes fpath = do
  csvData <- BL.readFile fpath
  case decodeByName csvData of
    Left err -> error err
    Right (_, quotes) -> pure (toList quotes)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
