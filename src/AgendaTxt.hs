module AgendaTxt where

import Chronos
import Data.Attoparsec.Text
import Data.Char (isAlpha)
import Data.List (sortOn)
import Data.Scientific (floatingOrInteger)
import Data.Text

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
    repeatRule :: Maybe [RepeatFilter],
    time :: Maybe EventTime,
    description :: Text
  }
  deriving (Eq, Show)

eventsWithin :: Day -> Day -> [Event] -> [(Day, Event)]
eventsWithin start end events =
  let events' = sortOn startDay $ Prelude.filter (\event -> startDay event > end) events
      days = [start .. end]

      keepMatches :: [(Day, Event)] -> [(Day, Event)] -> [(Day, Event)]
      keepMatches [] _ = []
      keepMatches (x : xs) acc =
        case uncurry eventOnDay x of
          Match -> keepMatches xs (x : acc)
          NoMatch -> keepMatches xs acc
          NoMatchAndFinished -> acc
   in foldMap (\event -> keepMatches [] $ fmap (,event) days) events'

data EventOnDay = Match | NoMatch | NoMatchAndFinished deriving (Eq, Ord)

eventOnDay :: Day -> Event -> EventOnDay
eventOnDay day' event =
  if day' == startDay event
    then Match
    else case repeatRule event of
      Nothing ->
        NoMatchAndFinished
      Just filters ->
        Prelude.foldr (max . matchesFilter day' (dayToDate day')) Match filters

matchesFilter :: Day -> Date -> RepeatFilter -> EventOnDay
matchesFilter _ date (DatePattern pattern) =
  if matchesPattern date pattern then Match else NoMatch
matchesFilter _ date (NotDatePattern pattern) =
  if matchesPattern date pattern then NoMatch else Match
matchesFilter _ date (WeekDay weekDay) =
  if weekDay == dateToDayOfWeek date then Match else NoMatch
matchesFilter day' _ (EndDate endDate) =
  if day' > endDate then NoMatchAndFinished else Match

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
  repeatRule <- option Nothing (Just <$> parserRepeatRule)
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
