{-# LANGUAGE BangPatterns  #-}

module Advent where

import Control.Arrow((&&&), (|||))
import Control.Category ((>>>))
import Control.Monad (join)
import Data.Attoparsec.ByteString.Char8
import Data.Bifunctor (bimap, second)
import Data.Either (rights)
import Data.List (nub)
import Text.Read (read)
import qualified Data.ByteString.Char8        as B
import qualified Data.IntSet                  as S
import qualified Data.Map.Strict              as M

--- Day 1: Chronal Calibration ---

-- Part One:
-- Starting with a frequency of zero,
-- what is the resulting frequency after
-- all of the changes in frequency have been applied?
-- Part Two:
-- What is the first frequency your device reaches twice?
problem_one :: IO (Int, Int)
problem_one =
  B.readFile "inputs/1"
  >>= (B.lines
  >>> fmap (read . B.unpack . B.dropWhile (== '+'))
  >>> sum &&& (check mempty . scanl (+) 0 . cycle)
  >>> pure)
  where
    check !acc (a:as) =
      if S.member a acc then a else check (S.insert a acc) as

--- Day 2: Inventory Management System ---

problem_two :: IO (Int, String)
problem_two =
  B.readFile "inputs/2"
  >>= (problem_two_a &&& problem_two_b >>> pure)

-- Part One:
-- Count the number of boxes that have an ID containing exactly two of any letter
-- and then separately counting those with exactly three of any letter.
-- Multiply those two counts together to get a checksum.
problem_two_a :: B.ByteString -> Int
problem_two_a =
  B.lines
  >>> fmap (freqs mempty . B.unpack)
  >>> fmap (len 2) &&& fmap (len 3)
  >>> join bimap (length . concat . fmap nub)
  >>> uncurry (*)
  where
    add = pure . maybe 1 (+ 1)
    len n = M.elems . M.filter ((==) n)
    freqs !acc (a:as) = freqs (M.alter add a acc) as
    freqs !acc [] = acc

-- Part Two:
-- The boxes will have IDs which differ by exactly one character at the same position in both strings.
-- What letters are common between the two correct box IDs?
problem_two_b :: B.ByteString -> String
problem_two_b =
  B.lines
  >>> fmap B.unpack
  >>> check
  >>> filter (\(bid, common) -> length bid == (length common + 1))
  >>> head
  >>> snd
  where
    check ids = diffId <$> ids <*> ids
    diffId bid bid' = (bid, common) where
      common = concat $ zipWith pick bid bid'
      pick a b = if a == b then pure a else mempty

--- Day 3: No Matter How You Slice It ---

-- Part One:
-- How many square inches of fabric are within two or more claims?
-- A claim like #123 @ 3,2: 5x4 means that claim ID 123 specifies a rectangle 3 inches from the left edge,
-- 2 inches from the top edge, 5 inches wide, and 4 inches tall.
-- Part Two: What is the ID of the only claim that doesn't overlap?
problem_three :: IO (Int, Int)
problem_three =
  B.readFile "inputs/3"
  >>= (B.lines
  >>> fmap (parseOnly parseClaim)
  >>> (rights
      &&&
      (fmap
      ((show >>> error)
        ||| cDims
        >>> fmap (flip (,) 1))
      >>> concat
      >>> M.fromListWith (+)))
  >>> (snd >>> (M.filter (> 1) >>> M.size))
      &&&
      ((\(cs, m) -> flip filter cs (cDims >>> (all (\pos -> pos `M.lookup` m == Just 1))))
      >>> head
      >>> cId)
  >>> pure)

data Claim =
  Claim
  { cId   :: !Int
  , cx    :: !Int
  , cy    :: !Int
  , cw    :: !Int
  , ch    :: !Int
  , cDims :: ![(Int, Int)]
  } deriving Show

parseClaim :: Parser Claim
parseClaim = do
  char '#'
  cid <- decimal
  space
  char '@'
  space
  x <- decimal
  char ','
  y <- decimal
  char ':'
  space
  w <- decimal
  char 'x'
  h <- decimal
  let dims = (,) <$> xs <*> ys
      xs   = [ x .. x + w - 1 ]
      ys   = [ y .. y + h - 1 ]
  pure $ Claim cid x y w h dims
