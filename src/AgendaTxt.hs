module AgendaTxt where

import Chronos
import Data.Attoparsec.Text
import Data.Char (isAlpha)
import Data.Scientific (floatingOrInteger)
import Data.Text
import SortedList

main :: IO ()
main = putStrLn "Hello, agenda-txt!"

data RepeatFilter
  = DatePattern DatePattern
  | NotDatePattern DatePattern
  | WeekDay DayOfWeek
  | EndDate Day
  deriving (Eq, Show)

data DatePattern = DatePattern_
  { year :: Maybe Year,
    month :: Maybe Month,
    day :: Maybe DayOfMonth
  }
  deriving (Eq, Show)

data EventTime = EventTime
  { startTime :: TimeOfDay,
    durationMinutes :: Maybe Int,
    timezone :: Maybe Text
  }
  deriving (Eq, Show)

data Event = Event
  { startDay :: Day,
    repeatRule :: [RepeatFilter],
    time :: Maybe EventTime,
    description :: Text
  }
  deriving (Eq, Show)

instance Ord Event where
  compare a b = compare (startDay a) (startDay b)

data DayRange
  = From Day Direction
  | Between Day Day

eventsFrom :: DayRange -> SortedList Event -> [(Day, Event)]
eventsFrom range events =
  let (direction, days) =
        case range of
          From day' Future -> (Future, [day' ..])
          From day' Past -> (Past, [day', pred day' ..])
          Between start end -> (if start > end then Past else Future, [start .. end])

      keepMatches :: [(Day, Event)] -> [(Day, Event)] -> [(Day, Event)]
      keepMatches [] _ = []
      keepMatches (x : xs) acc =
        case uncurry eventOnDay x of
          Match -> keepMatches xs (x : acc)
          NoMatch -> keepMatches xs acc
          NoFurtherMatches direction' ->
            if direction == direction'
              then acc
              else keepMatches xs acc
   in foldMap (\event -> keepMatches [] $ fmap (,event) days) events

data EventOnDay
  = Match
  | NoMatch
  | NoFurtherMatches Direction
  deriving (Eq, Ord)

data Direction = Future | Past deriving (Eq, Ord)

eventOnDay :: Day -> Event -> EventOnDay
eventOnDay day' event =
  case compare day' (startDay event) of
    EQ -> Match
    LT -> NoFurtherMatches Past
    GT ->
      Prelude.foldr
        (max . matchesFilter day' (dayToDate day'))
        Match
        (repeatRule event)

matchesFilter :: Day -> Date -> RepeatFilter -> EventOnDay
matchesFilter _ date (DatePattern pattern) =
  if matchesPattern date pattern then Match else NoMatch
matchesFilter _ date (NotDatePattern pattern) =
  if matchesPattern date pattern then NoMatch else Match
matchesFilter _ date (WeekDay weekDay) =
  if weekDay == dateToDayOfWeek date then Match else NoMatch
matchesFilter day' _ (EndDate endDate) =
  if day' > endDate then NoFurtherMatches Future else Match

matchesPattern :: Date -> DatePattern -> Bool
matchesPattern date pattern =
  let matches :: (Eq a) => a -> Maybe a -> Bool
      matches _ Nothing = True
      matches x (Just y) = x == y
   in matches (dateYear date) (year pattern)
        && matches (dateMonth date) (month pattern)
        && matches (dateDay date) (AgendaTxt.day pattern)

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

parserRepeatRule :: Parser [RepeatFilter]
parserRepeatRule =
  char '['
    <* skipSpace
    *> sepBy' parserRepeatStep skipSpace
    <* skipSpace
    <* char ']'

parserRepeatStep :: Parser RepeatFilter
parserRepeatStep =
  choice
    [ NotDatePattern <$> (char '!' *> parserDatePattern),
      EndDate <$> (string "<=" *> parserDate),
      DatePattern <$> parserDatePattern,
      WeekDay <$> parserDayOfWeek
    ]

parserDatePattern :: Parser DatePattern
parserDatePattern = do
  year <- option Nothing (Just . Year <$> parserInt)
  _ <- char '-'
  month <- option Nothing (Just . Month <$> parserInt)
  _ <- char '-'
  day_ <- option Nothing (Just . DayOfMonth <$> parserInt)
  pure $ DatePattern_ year month day_

parserInt :: Parser Int
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

parserDuration :: Parser Int
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

parserTimeOfDay :: Parser TimeOfDay
parserTimeOfDay = parser_HMS_opt_S (Just ':')
