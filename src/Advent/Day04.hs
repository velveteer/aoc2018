{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Advent.Day04 where

import           Prelude                      hiding ( take )
import           Control.Arrow                       ( (&&&) )
import           Control.Applicative                 ( (<|>)
                                                     , liftA2
                                                     )
import           Control.Category                    ( (>>>) )
import           Control.Monad                       ( void )
import           Data.Attoparsec.ByteString.Char8    ( Parser
                                                     , char
                                                     , decimal
                                                     , parseOnly
                                                     , space
                                                     , string
                                                     , take
                                                     )
import           Data.Bifunctor                      ( first
                                                     , second
                                                     )
import           Data.Either                         ( rights )
import           Data.Ix                             ( range )
import           Data.List                           ( group
                                                     , maximumBy
                                                     , sort
                                                     )
import           Data.List.Split                     ( chunksOf )
import           Data.Ord                            ( comparing )
import           Data.Time                           ( UTCTime(..)
                                                     , TimeOfDay(..)
                                                     , defaultTimeLocale
                                                     , parseTimeM
                                                     , timeToTimeOfDay
                                                     )
import           Data.Tuple                          ( swap )
import           Safe                                ( headMay )
import qualified Data.ByteString.Char8              as B
import qualified Data.HashMap.Strict                as HM

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
minutesAsleep (Sleep e1) (Wake e2) = range (start, end)
  where
    start = minute e1
    end = minute e2 - 1
    minute = utctDayTime >>> timeToTimeOfDay >>> todMin
minutesAsleep _ _ = error "bad pattern"

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
