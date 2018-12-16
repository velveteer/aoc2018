{-# LANGUAGE OverloadedStrings #-}

module Advent.Day05 where

import           Control.Arrow                       ( (&&&) )
import           Control.Category                    ( (>>>) )
import           Data.Char                           ( toLower )
import           Data.Ix                             ( range )
import           Data.List                           ( sort )
import qualified Data.ByteString.Char8              as B
import qualified Data.Text                          as T

--- Day 5: Alchemical Reduction ---

-- The polymer is formed by smaller units which, when triggered, react with each other such that
-- two adjacent units of the same type and opposite polarity are destroyed.
-- Units' types are represented by letters; units' polarity is represented by capitalization.
-- For instance, r and R are units with the same type but opposite polarity, whereas r and s are entirely different types and do not react.

-- For example:

-- In aA, a and A react, leaving nothing behind.
-- In abBA, bB destroys itself, leaving aA. As above, this then destroys itself, leaving nothing.
-- In abAB, no two adjacent units are of the same type, and so nothing happens.
-- In aabAAB, even though aa and AA are of the same type, their polarities match, and so nothing happens.

--- Part One ---
-- How many units remain after fully reacting the polymer you scanned?

problem_five :: IO (Int, Int)
problem_five =
  B.readFile "inputs/5"
    >>= (B.unpack
        >>> T.pack
        >>> T.strip
        >>> T.unpack
        >>> (reactFully >>> length)
        &&& (withoutUnits >>> fmap (reactFully >>> length) >>> sort >>> head)
        >>> pure
        )

canReact :: Char -> Char -> Bool
canReact a b = a /= b && toLower a == toLower b

cons :: Char -> String -> String
cons x (y : xs) | canReact x y = xs
                | otherwise = x : y : xs
cons x [] = [x]

reactFully :: String -> String
reactFully = foldr cons mempty

alphas :: String
alphas = range ('a', 'z')

withoutUnits :: String -> [String]
withoutUnits poly = fmap (\c -> filter (\d -> toLower d /= c) poly) alphas
