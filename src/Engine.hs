module Engine where

import Chronos
import Conduit
import Control.Monad (when)
import Data.Attoparsec.Text
import Data.Char (isAlpha)
import Data.Foldable (for_)
import Data.Maybe (catMaybes)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Torsor

data DateFilter
  = DatePattern DatePattern
  | NotDatePattern DatePattern
  | WeekDay DayOfWeek
  | EndDate Day
  | Period Period
  deriving (Eq, Show)

data DatePattern = DatePattern_
  { year :: Maybe Year,
    month :: Maybe Month,
    day :: Maybe DayOfMonth
  }
  deriving (Eq, Show)

data Period
  = Days Word
  | Weeks Word
  | Months Word
  | Years Word
  deriving (Eq, Show)

data EventTime = EventTime
  { startTime :: TimeOfDay,
    durationMinutes :: Maybe Word,
    timezone :: Maybe Text
  }
  deriving (Eq, Show)

data Event = Event
  { startDay :: Day,
    repeatRule :: [DateFilter],
    time :: Maybe EventTime,
    description :: Text
  }
  deriving (Eq, Show)

data Recurrence a = Recurrence
  { onDay :: Day -> Bool,
    event :: a,
    minDay :: Maybe Day,
    maxDay :: Maybe Day
  }

days :: (Monad m) => Day -> Direction -> [DateFilter] -> ConduitT i Day m ()
days from direction dateFilters =
  let daysUnbounded =
        iterateC (nextDay direction) from
      boundFilter =
        case direction of
          Future ->
            (\bound -> takeWhileC (<= bound)) <$> filtersUpperBound from dateFilters
          Past ->
            (\bound -> takeWhileC (>= bound)) <$> filtersLowerBound dateFilters
   in case boundFilter of
        Nothing ->
          daysUnbounded
            .| filterC (matchesFilters from dateFilters)
        Just boundFilter' ->
          daysUnbounded
            .| boundFilter'
            .| filterC (matchesFilters from dateFilters)

-- The conduit produced by this function assumes input days are monotonically
-- increasing or decreasing in the direction of the input argument.
occurrences :: (Monad m) => Direction -> [Recurrence a] -> ConduitT Day (Day, a) m ()
occurrences direction recurrences = do
  maybeDay <- headC
  whenJust maybeDay $ \day' -> do
    let liveRecurrences = removeDeadRecurrences direction day' recurrences
    for_ liveRecurrences $ \recurrence ->
      when
        (onDay recurrence day')
        (yield (day', event recurrence))
    when
      (not (null liveRecurrences))
      (occurrences direction liveRecurrences)

whenJust :: (Applicative m) => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = pure ()
whenJust (Just x) f = f x

nextDay :: Direction -> Day -> Day
nextDay Future = succ
nextDay Past = pred

removeDeadRecurrences :: Direction -> Day -> [Recurrence a] -> [Recurrence a]
removeDeadRecurrences Future day' = filter (nothingOr (day' <=) . maxDay)
removeDeadRecurrences Past day' = filter (nothingOr (day' >=) . minDay)

nothingOr :: (a -> Bool) -> Maybe a -> Bool
nothingOr _ Nothing = True
nothingOr predicate (Just x) = predicate x

data Direction = Future | Past deriving (Show, Eq, Ord)

eventToRecurrence :: Event -> Recurrence Event
eventToRecurrence event =
  case repeatRule event of
    [] ->
      Recurrence
        { onDay = (startDay event ==),
          event = event,
          minDay = Just (startDay event),
          maxDay = Just (startDay event)
        }
    filters ->
      Recurrence
        { onDay = matchesFilters (startDay event) filters,
          event = event,
          minDay = Just (startDay event),
          maxDay = filtersUpperBound (startDay event) filters
        }

minimum' :: (Foldable f, Ord a) => f a -> Maybe a
minimum' = foldr keepLower Nothing
  where
    keepLower :: (Ord a) => a -> Maybe a -> Maybe a
    keepLower x Nothing = Just x
    keepLower x (Just y) = if x < y then Just x else Just y

maximum' :: (Foldable f, Ord a) => f a -> Maybe a
maximum' = foldr keepUpper Nothing
  where
    keepUpper :: (Ord a) => a -> Maybe a -> Maybe a
    keepUpper x Nothing = Just x
    keepUpper x (Just y) = if x > y then Just x else Just y

filtersUpperBound :: Day -> [DateFilter] -> Maybe Day
filtersUpperBound startDay = minimum' . catMaybes . fmap (filterUpperBound startDay)

filterUpperBound :: Day -> DateFilter -> Maybe Day
filterUpperBound _ (DatePattern pattern) = lastDayOfYear <$> year pattern
filterUpperBound _ (NotDatePattern _) = Nothing
filterUpperBound _ (WeekDay _) = Nothing
filterUpperBound _ (EndDate endDate) = Just endDate
filterUpperBound startDay (Period period) = Just (addPeriod startDay period)

lastDayOfYear :: Year -> Day
lastDayOfYear year =
  pred . ordinalDateToDay $ OrdinalDate (nextYear year) (DayOfYear 1)

filtersLowerBound :: [DateFilter] -> Maybe Day
filtersLowerBound = maximum' . catMaybes . fmap filterLowerBound

filterLowerBound :: DateFilter -> Maybe Day
filterLowerBound (DatePattern pattern) = firstDayOfYear <$> year pattern
filterLowerBound (NotDatePattern _) = Nothing
filterLowerBound (WeekDay _) = Nothing
filterLowerBound (EndDate _) = Nothing
filterLowerBound (Period _) = Nothing

firstDayOfYear :: Year -> Day
firstDayOfYear year =
  ordinalDateToDay $ OrdinalDate year (DayOfYear 1)

nextYear :: Year -> Year
nextYear = Year . (+ 1) . getYear

matchesFilters :: Day -> [DateFilter] -> Day -> Bool
matchesFilters startDay dateFilters day' =
  all (matchesFilter startDay day' (dayToDate day')) dateFilters

matchesFilter :: Day -> Day -> Date -> DateFilter -> Bool
matchesFilter _ _ date (DatePattern pattern) = matchesPattern date pattern
matchesFilter _ _ date (NotDatePattern pattern) = not (matchesPattern date pattern)
matchesFilter _ _ date (WeekDay weekDay) = weekDay == dateToDayOfWeek date
matchesFilter _ day' _ (EndDate endDate) = day' <= endDate
matchesFilter startDay day' _ (Period period) = day' <= addPeriod startDay period

matchesPattern :: Date -> DatePattern -> Bool
matchesPattern date pattern =
  let matches :: (Eq a) => a -> Maybe a -> Bool
      matches _ Nothing = True
      matches x (Just y) = x == y
   in matches (dateYear date) (year pattern)
        && matches (dateMonth date) (month pattern)
        && matches (dateDay date) (Engine.day pattern)

parserEvent :: Parser Event
parserEvent = do
  startDate <- parserDate
  _ <- skipSpace
  repeatRule <- option [] (parserRepeatRule)
  _ <- skipSpace
  time <- option Nothing (Just <$> parserTime)
  _ <- skipSpace
  description <- Data.Attoparsec.Text.takeWhile (/= '\n')
  pure $ Event startDate repeatRule time description

parserRepeatRule :: Parser [DateFilter]
parserRepeatRule =
  char '['
    <* skipSpace
    *> sepBy' parserRepeatStep skipSpace
    <* skipSpace
    <* char ']'

parserRepeatStep :: Parser DateFilter
parserRepeatStep =
  choice
    [ NotDatePattern <$> (char '!' *> parserDatePattern),
      EndDate <$> (string "<=" *> parserDate),
      Period <$> (char '*' *> parserPeriod),
      DatePattern <$> parserDatePattern,
      WeekDay <$> parserDayOfWeek
    ]

parserDatePattern :: Parser DatePattern
parserDatePattern = do
  year <- option Nothing (Just . Year <$> parserInt)
  _ <- char '-'
  month <- option Nothing (Just . Month . (\m -> m - 1) <$> parserInt)
  _ <- char '-'
  day_ <- option Nothing (Just . DayOfMonth <$> parserInt)
  pure $ DatePattern_ year month day_

parserInt :: (Integral i) => Parser i
parserInt = do
  next <- peekChar'
  if next == '-'
    then fail "unexpected negative number"
    else pure ()
  floatOrInteger <- floatingOrInteger <$> scientific
  case floatOrInteger of
    Left (_ :: Double) -> fail "unexpected float in DatePattern"
    Right int -> pure int

parserTime :: Parser EventTime
parserTime =
  EventTime
    <$> parserTimeOfDay
    <* skipSpace
    <*> option Nothing (Just <$> parserDuration)
    <* skipSpace
    <*> option Nothing (Just <$> parserTimezone)

parserDuration :: Parser Word
parserDuration = do
  _ <- char '+'
  hours <- parserInt
  _ <- char ':'
  minutes <- parserInt
  pure (60 * hours + minutes)

parserTimezone :: Parser Text
parserTimezone =
  char '(' *> Data.Attoparsec.Text.takeWhile isAlpha <* char ')'

parserDayOfWeek :: Parser DayOfWeek
parserDayOfWeek =
  choice
    [ string "mon" *> pure monday,
      string "tue" *> pure tuesday,
      string "wed" *> pure wednesday,
      string "thu" *> pure thursday,
      string "fri" *> pure friday,
      string "sat" *> pure saturday,
      string "sun" *> pure sunday
    ]

parserDate :: Parser Day
parserDate = dateToDay <$> parser_Ymd (Just '-')

parserPeriod :: Parser Period
parserPeriod =
  choice
    [ fmap Days $ parserInt <* string "d",
      fmap Weeks $ parserInt <* string "w",
      fmap Months $ parserInt <* string "m",
      fmap Years $ parserInt <* string "y"
    ]

parserTimeOfDay :: Parser TimeOfDay
parserTimeOfDay = parser_HMS_opt_S (Just ':')

periodToDays :: Period -> Word
periodToDays (Days n) = n
periodToDays (Weeks n) = 7 * n
periodToDays (Months n) = 31 * n
periodToDays (Years n) = 365 * n

addPeriod :: Day -> Period -> Day
addPeriod day' period = Torsor.add (fromIntegral (periodToDays period)) day'
