{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fmt (Buildable (..), fmt, fmtLn, nameF, unwordsF, (+||), (||+))
import System.Environment (getArgs)
import System.Random.Stateful
import TextShow
import Control.Monad.Writer

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-r", fname, dir] -> rotateFromFile (read dir) fname
    ["-o", fname] -> orientFromFile fname
    _ -> putStrLn $ "Usage: locator -o filename\n" ++ "locator -r filenam direction"

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

-- circleArea :: (Real a, Floating b) => a -> b
-- circleArea r = pi * r * r

----

data Person = Person String (Maybe Int)

instance TextShow Person where
  showb (Person name Nothing) = fromString name
  showb (Person name (Just age)) = fromString name <> " (" <> showb age <> ")"

data Expr a
  = Lit a
  | Add (Expr a) (Expr a)
  | Mult (Expr a) (Expr a)

expr1 = Mult (Add (Lit 2) (Mult (Lit 3) (Lit 3))) (Lit 5)

instance TextShow a => TextShow (Expr a) where
  showbPrec p e =
    case e of
      Lit a -> showb a
      Add e1 e2 -> showbHelper p 5 "+" e1 e2
      Mult e1 e2 -> showbHelper p 6 "*" e1 e2
    where
      showbHelper outerPrec thisPrec op e1 e2 =
        showbParen (outerPrec > thisPrec) $ showbPrec thisPrec e1 <> op <> showbPrec thisPrec e2


sumN :: Int -> Writer String Int
sumN 0 = writer (0, "finish")
sumN n = do
  tell (show n ++ ",")
  s <- sumN (n - 1)
  pure $ n + s

addNumber :: Int -> IO String
addNumber n = pure (++) <*> pure (show n ++ " ") <*> getLine

