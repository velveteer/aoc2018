{-# LANGUAGE OverloadedStrings #-}

module Advent.Day06 where

import           Prelude                      hiding ( take )
import           Control.Arrow                       ( (&&&) )
import           Control.Category                    ( (>>>) )
import           Control.Monad                       ( join )
import           Data.Bifunctor                      ( bimap
                                                     , second
                                                     )
import qualified Data.Map                           as M
import           Data.Attoparsec.ByteString.Char8    ( Parser
                                                     , decimal
                                                     , parseOnly
                                                     , string
                                                     )
import           Linear                              ( V2(..) )
import           Data.Either                         ( rights )
import           Data.Function                       ( on )
import           Data.Ix                             ( range )
import           Data.List                           ( groupBy
                                                     , sortBy
                                                     )
import           Data.Semigroup                      ( Min(..)
                                                     , Max(..)
                                                     )
import qualified Data.ByteString.Char8              as B

--- Day 6: Chronal Coordinates ---

-- Using only the Manhattan distance, determine the area around each coordinate by-- counting the number of integer X,Y locations that are closest to that
-- coordinate (and aren't tied in distance to any other coordinate).

-- Your goal is to find the size of the largest area that isn't infinite.

--- Part One ---
-- What is the size of the largest area that isn't infinite?

--- Part Two ---
-- What is the size of the region containing all locations which have a total distance to all given coordinates of less than 10000?

problem_six :: IO (Int, Int)
problem_six =
  B.readFile "inputs/6"
    >>= (B.lines
        >>> fmap (parseOnly parsePoint)
        >>> rights
        >>> id
        &&& boundingBox
        >>> (second ((,) <$> boxPoints <*> maskPoints)
            >>> uncurry (onBoth . (=<<) . findClosestSite)
            >>> onBoth (M.fromListWith (+) . (fmap (flip (,) (1 :: Int))))
            >>> uncurry (M.intersectionWith (\x y -> if' (x == y) (Just x) Nothing))
            >>> M.mapMaybe id
            >>> maximum
            )
        &&& ((\(sites, box) ->
               filter (\p -> sum (fmap (distance p) sites) < 10000) (boxPoints box)
             )
            >>> length
            )
        >>> pure
        )

type Point = V2 Int
type Box   = V2 Point

distance :: Point -> Point -> Int
distance p = sum . abs . subtract p

parsePoint :: Parser Point
parsePoint = V2 <$> decimal <* string ", " <*> decimal

-- Neat use of semigroups to fold over all the points while accumulating a minimum and maximum for xs and ys. Justin Le gets the credit here.
boundingBox :: [Point] -> Box
boundingBox ps = V2 xMin yMin `V2` V2 xMax yMax
  where
    (Min xMin, Min yMin, Max xMax, Max yMax) =
      flip foldMap ps $ \(V2 x y) -> (Min x, Min y, Max x, Max y)

boxPoints :: Box -> [Point]
boxPoints (V2 mins maxs) = range (mins, maxs)

maskPoints :: Box -> [Point]
maskPoints (V2 mins maxs) = range (mins + 1, maxs + 1)

-- Sort the sites by their distance to the selected point in the box
-- Group them so we make sure to pattern match out a unique site for the point
-- i.e. if the group length is > 1 then the point is the same distance
-- to > 1 sites, and therefore does not count in the area for either site
-- List/concatMap could be replaced with Maybe/catMaybes
findClosestSite :: [Point] -> Point -> [Point]
findClosestSite sites p = case groups of
  [(r, _)] : _ -> [r]
  _ -> []
  where
    groups =
      groupBy ((==) `on` snd)
        . sortBy (compare `on` snd)
        $ (\site -> (site, distance p site))
        <$> sites

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

onBoth :: (a -> b) -> (a, a) -> (b, b)
onBoth = join bimap
