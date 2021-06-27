{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Control.Monad (replicateM, when)
import Data.List (nub, sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fmt (Buildable (..), fmt, fmtLn, nameF, unwordsF, (+||), (||+))
import System.Environment (getArgs)
import System.Random.Stateful
import System.Exit (exitFailure)

-- main :: IO ()
-- main = do
--   args <- getArgs
--   case args of
--     ["-r", fname, dir] -> rotateFromFile (read dir) fname
--     ["-o", fname] -> orientFromFile fname
--     _ -> putStrLn $ "Usage: locator -o filename\n" ++ "locator -r filenam direction"

main :: IO ()
main = do
  ds <- randomDirections 1000
  ts <- randomTurns 1000
  when
    (not $ and [test_allTurnsInUse, test_orientRotateAgree ds, test_rotationsMonoidAgree ts])
    exitFailure

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
  cpred :: a -> a
  cpred d
    | d == minBound = maxBound
    | otherwise = pred d

  csucc :: a -> a
  csucc d
    | d == maxBound = minBound
    | otherwise = succ d

data Direction = North | East | South | West
  deriving (Eq, Enum, Bounded, Show, CyclicEnum, Read)

data Turn = TNone | TLeft | TRight | TAround
  deriving (Eq, Enum, Bounded, Show, Read)

instance Semigroup Turn where
  TNone <> t = t
  TLeft <> TLeft = TAround
  TLeft <> TRight = TNone
  TLeft <> TAround = TRight
  TRight <> TRight = TAround
  TRight <> TAround = TLeft
  TAround <> TAround = TNone
  t1 <> t2 = t2 <> t1

instance Monoid Turn where
  mempty = TNone

instance Buildable Direction where
  build North = "N"
  build East = "E"
  build South = "S"
  build West = "W"

instance Buildable Turn where
  build TNone = "--"
  build TLeft = "<-"
  build TRight = "->"
  build TAround = "||"

deriving instance Ord Turn

instance Uniform Direction where
  uniformM rng = uniformRM (minBound, maxBound) rng

instance UniformRange Direction where
  uniformRM (lo, hi) rng = do
    res <- uniformRM (fromEnum lo :: Int, fromEnum hi) rng
    pure $ toEnum res

instance UniformRange Turn where
  uniformRM (lo, hi) rng = do
    res <- uniformRM (fromEnum lo :: Int, fromEnum hi) rng
    pure $ toEnum res

instance Uniform Turn where
  uniformM rng = uniformRM (minBound, maxBound) rng

uniformIO :: Uniform a => IO a
uniformIO = getStdRandom uniform

uniformsIO :: Uniform a => Int -> IO [a]
uniformsIO n = replicateM n uniformIO

randomTurns :: Int -> IO [Turn]
randomTurns = uniformsIO

randomDirections :: Int -> IO [Direction]
randomDirections = uniformsIO

writeRandomFile :: (RandomGen a, Show a) => Int -> (Int -> IO [a]) -> FilePath -> IO ()
writeRandomFile n gen fname = do
  xs <- gen n
  writeFile fname $ unlines $ map show xs

test_allTurnsInUse :: Bool
test_allTurnsInUse =
  sort (nub [orient d1 d2 | d1 <- every, d2 <- every]) == every

test_rotationsMonoidAgree :: [Turn] -> Bool
test_rotationsMonoidAgree ts =
  and [rotateMany d ts == rotateMany' d ts | d <- every]

test_orientRotateAgree :: [Direction] -> Bool
test_orientRotateAgree [] = True
test_orientRotateAgree ds@(d : _) = ds == rotateManySteps d (orientMany ds)

----

rotate :: Turn -> Direction -> Direction
rotate TNone = id
rotate TLeft = cpred
rotate TRight = csucc
rotate TAround = cpred . cpred

every :: (Enum a, Bounded a) => [a]
every = enumFrom minBound

orient :: Direction -> Direction -> Turn
orient d1 d2 = head $ filter (\t -> rotate t d1 == d2) every

rotateMany :: Direction -> [Turn] -> Direction
rotateMany = foldl (flip rotate)

rotateMany' :: Direction -> [Turn] -> Direction
rotateMany' dir ts = rotate (mconcat ts) dir

rotateManySteps :: Direction -> [Turn] -> [Direction]
rotateManySteps = scanl (flip rotate)

orientMany :: [Direction] -> [Turn]
orientMany ds@(_ : _) = zipWith orient ds (tail ds)
orientMany _ = []

rotateFromFile :: Direction -> FilePath -> IO ()
rotateFromFile dir fname = do
  f <- readFile fname
  let turns = map read $ lines f
      finalDir = rotateMany dir turns
      dirs = rotateManySteps dir turns
  fmtLn $ "Final direction: " +|| finalDir ||+ ""
  fmt $ nameF "Intermediate directions" (unwordsF dirs)

orientFromFile :: FilePath -> IO ()
orientFromFile = undefined
