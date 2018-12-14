{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Advent where

import           Prelude                      hiding ( take )
import           Control.Arrow                       ( (&&&)
                                                     , (|||)
                                                     )
import           Control.Applicative                 ( (<|>)
                                                     , liftA2
                                                     )
import           Control.Category                    ( (>>>) )
import           Control.Monad                       ( join
                                                     , void
                                                     )
import           Data.Attoparsec.ByteString.Char8    ( Parser
                                                     , char
                                                     , decimal
                                                     , parseOnly
                                                     , space
                                                     , string
                                                     , take
                                                     )
import           Data.Bifunctor                      ( bimap
                                                     , first
                                                     , second
                                                     )
import           Data.Either                         ( rights )
import           Data.List                           ( group
                                                     , maximumBy
                                                     , nub
                                                     , sort
                                                     )
import           Data.List.Split                     ( chunksOf )
import           Data.Ix                             ( range )
import           Data.Ord                            ( comparing )
import           Data.Time                           ( UTCTime(..)
                                                     , TimeOfDay(..)
                                                     , parseTimeM
                                                     , timeToTimeOfDay
                                                     )
import           Data.Time.Locale.Compat             ( defaultTimeLocale )
import           Data.Tuple                          ( swap )
import           Linear.V2                           ( V2(..) )
import           Safe                                ( headMay )
import           Text.Read                           ( read )
import qualified Data.ByteString.Char8              as B
                                                     ( ByteString
                                                     , dropWhile
                                                     , lines
                                                     , readFile
                                                     , unpack
                                                     )
import qualified Data.IntSet                        as S
                                                     ( insert
                                                     , member
                                                     )
import qualified Data.HashMap.Strict                as HM
                                                     ( HashMap
                                                     , elems
                                                     , filter
                                                     , fromListWith
                                                     , insertWith
                                                     , lookup
                                                     , size
                                                     , toList
                                                     )

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
        &&& (uncurry
                (flip
                  (filter
                  . (cDims >>>)
                  . all
                  . flip flip (Just 1)
                  . ((==) .)
                  . flip HM.lookup
                  )
                )
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

--- Day 4: Repose Record ---

--- Part Two ---
-- Find the guard that has the most minutes asleep.
-- What minute does that guard spend asleep the most?
-- What is the ID of the guard you chose multiplied by the minute you chose?
-- For example, consider the following records, which have already been organized into chronological order:

-- [1518-11-01 00:00] Guard #10 begins shift
-- [1518-11-01 00:05] falls asleep
-- [1518-11-01 00:25] wakes up
-- [1518-11-01 00:30] falls asleep
-- [1518-11-01 00:55] wakes up
-- [1518-11-01 23:58] Guard #99 begins shift
-- [1518-11-02 00:40] falls asleep
-- [1518-11-02 00:50] wakes up

--- Part Two ---
-- Of all guards, which guard is most frequently asleep on the same minute?
-- What is the ID of the guard you chose multiplied by the minute you chose?

problem_four :: IO (Maybe Int, Maybe Int)
problem_four =
  B.readFile "inputs/4"
    >>= (B.lines
        >>> fmap (parseOnly parseRecord)
        >>> rights
        >>> sort
        >>> checkShifts
        >>> toEventMap mempty
        >>> fmap (pairs >>> fmap (swap >>> (uncurry minutesAsleep)) >>> concat)
        >>> HM.toList
        >>> (maximumBy (comparing (snd >>> length))
            >>> second (headMay . maximumBy (comparing length) . group . sort)
            >>> first pure
            >>> uncurry (liftA2 (*))
            )
        &&& (fmap (second (maximumBy (comparing length) . group . sort)))
        >>> second
              (maximumBy (comparing (snd >>> length))
              >>> second headMay
              >>> first pure
              >>> uncurry (liftA2 (*))
              )
        >>> pure
        )

checkShifts :: [GRecord] -> (Int, [GRecord])
checkShifts gs = case (gEvent <$> headMay gs) of
  Just (Begin rid) -> (rid, gs)
  _ -> error "shifts must start with a Begin event"

toEventMap :: HM.HashMap Int [GEvent] -> (Int, [GRecord]) -> HM.HashMap Int [GEvent]
toEventMap !acc (recId, (a : as)) = case (gEvent a) of
  Begin rid -> toEventMap acc (rid, as)
  other -> toEventMap (HM.insertWith (<>) recId [other] acc) (recId, as)
toEventMap !acc _ = acc

minutesAsleep :: GEvent -> GEvent -> [Int]
minutesAsleep (Sleep e1) (Wake e2) = minuteRange start end mempty
  where
    start = minute e1
    end = minute e2 - 1
    minute = utctDayTime >>> timeToTimeOfDay >>> todMin
minutesAsleep _ _ = error "bad pattern"

minuteRange :: Int -> Int -> [Int] -> [Int]
minuteRange cur end acc | cur > 59 = minuteRange 0 end acc
                        | cur == end = cur : acc
                        | otherwise = minuteRange (cur + 1) end (cur : acc)

pairs :: [a] -> [(a, a)]
pairs = fmap (\[a, b] -> (a, b)) . chunksOf 2

data GEvent =
  Begin   !Int
  | Wake  !UTCTime
  | Sleep !UTCTime
    deriving (Eq, Ord, Show)

data GRecord =
  GRecord
  { gTime  :: !UTCTime
  , gEvent :: !GEvent
  } deriving (Eq, Ord, Show)

parseRecord :: Parser GRecord
parseRecord = do
  void $ char '['
  tStr <- take 16
  mTime <- parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" $ B.unpack tStr
  void $ char ']'
  void $ space
  event <- parseSleep mTime <|> parseWake mTime <|> parseBegin
  pure $ GRecord mTime event

parseSleep :: UTCTime -> Parser GEvent
parseSleep time = string "falls" <* space <* string "asleep" *> pure (Sleep time)

parseWake :: UTCTime -> Parser GEvent
parseWake time = string "wakes" <* space <* string "up" *> pure (Wake time)

parseBegin :: Parser GEvent
parseBegin =
  Begin
    <$> (string "Guard"
        <* space
        <* char '#'
        *> decimal
        <* space
        <* string "begins"
        <* space
        <* string "shift"
        )
