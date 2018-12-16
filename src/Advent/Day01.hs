{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Advent.Day01 where

import           Control.Arrow                       ( (&&&) )
import           Control.Category                    ( (>>>) )
import qualified Data.IntSet                        as S
import qualified Data.ByteString.Char8              as B

--- Day 1: Chronal Calibration ---

--- Part One ---
-- Starting with a frequency of zero,
-- what is the resulting frequency after
-- all of the changes in frequency have been applied?

--- Part Two ---
-- What is the first frequency your device reaches twice?

problem_one :: IO (Int, Int)
problem_one =
  B.readFile "inputs/1"
    >>= (B.lines
        >>> fmap (read . B.unpack . B.dropWhile (== '+'))
        >>> sum
        &&& (check mempty . scanl (+) 0 . cycle)
        >>> pure
        )
  where
    check _ [] = error "cannot check an empty list"
    check !acc (a : as) = if S.member a acc then a else check (S.insert a acc) as
