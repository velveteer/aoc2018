{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Advent.Day03 where

import           Control.Arrow                       ( (&&&)
                                                     , (|||)
                                                     )
import           Control.Category                    ( (>>>) )
import           Control.Monad                       ( void )
import           Linear.V2                           ( V2(..) )
import           Data.Attoparsec.ByteString.Char8    ( Parser
                                                     , char
                                                     , decimal
                                                     , parseOnly
                                                     , space
                                                     )
import           Data.Either                         ( rights )
import           Data.Ix                             ( range )
import           Safe                                ( headMay )
import qualified Data.ByteString.Char8              as B
import qualified Data.HashMap.Strict                as HM

--- Day 3: No Matter How You Slice It ---

--- Part One ---
-- How many square inches of fabric are within two or more claims?
-- A claim like #123 @ 3,2: 5x4 means that claim ID 123 specifies a rectangle 3 inches from the left edge,
-- 2 inches from the top edge, 5 inches wide, and 4 inches tall.

--- Part Two ---
-- What is the ID of the only claim that doesn't overlap?

problem_three :: IO (Int, Maybe Int)
problem_three =
  B.readFile "inputs/3"
    >>= (B.lines
        >>> fmap (parseOnly parseClaim)
        >>> (rights
            &&& (fmap ((show >>> error) ||| cDims >>> fmap (flip (,) (1 :: Int)))
                >>> concat
                >>> HM.fromListWith (+)
                )
            )
        >>> (snd >>> (HM.filter (> 1) >>> HM.size))
        &&& ((\(cs, m) -> filter (cDims >>> all (\pos -> HM.lookup pos m == Just 1)) cs)
            >>> headMay
            >>> fmap cId
            )
        >>> pure
        )

data Claim =
  Claim
  { cId     :: !Int
  , cOrigin :: !(V2 Int)
  , cSize   :: !(V2 Int)
  , cDims   :: ![V2 Int]
  } deriving Show

parseClaim :: Parser Claim
parseClaim = do
  void $ char '#'
  cid <- decimal
  void $ space
  void $ char '@'
  void $ space
  origin <- V2 <$> decimal <* char ',' <*> decimal
  void $ char ':'
  void $ space
  size <- V2 <$> decimal <* char 'x' <*> decimal
  let dims = range (origin, origin + size - 1)
  pure $ Claim cid origin size dims

