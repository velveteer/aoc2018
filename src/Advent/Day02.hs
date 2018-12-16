{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Advent.Day02 where

import           Control.Arrow                       ( (&&&) )
import           Control.Category                    ( (>>>) )
import           Control.Monad                       ( join )
import           Data.Bifunctor                      ( bimap )
import           Data.List                           ( nub )
import qualified Data.ByteString.Char8              as B
import qualified Data.HashMap.Strict                as HM

--- Day 2: Inventory Management System ---

problem_two :: IO (Int, String)
problem_two = B.readFile "inputs/2" >>= (problem_two_a &&& problem_two_b >>> pure)

--- Part One ---
-- Count the number of boxes that have an ID containing exactly two of any letter
-- and then separately counting those with exactly three of any letter.
-- Multiply those two counts together to get a checksum.

problem_two_a :: B.ByteString -> Int
problem_two_a =
  B.lines
    >>> fmap (freqs mempty . B.unpack)
    >>> fmap (len 2)
    &&& fmap (len 3)
    >>> join bimap (length . concat . fmap nub)
    >>> uncurry (*)
  where
    len = (HM.elems .) . HM.filter . (==)
    freqs !acc (a : as) = freqs (HM.insertWith (const (1 +)) a (1 :: Int) acc) as
    freqs !acc [] = acc

--- Part Two ---
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
    diffId bid bid' = (bid, common)
      where
        common = concat $ zipWith pick bid bid'
        pick a b = if a == b then pure a else mempty
