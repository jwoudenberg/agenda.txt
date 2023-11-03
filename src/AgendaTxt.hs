module AgendaTxt where

import Chronos
import Data.Attoparsec.Text
import Data.Char (isAlpha)
import Data.Foldable (traverse_)
import Data.Scientific (floatingOrInteger)
import Data.Text
import Data.Text.IO (hGetLine)
import System.Environment (getArgs)
import System.IO (hIsEOF, stdin)
import Text.Read (readMaybe)

main :: IO ()
main = do
  parsedArgs <- parseArgs defaultParsedArgs <$> getArgs
  case parsedArgs of
    ShowHelp -> do
      putStrLn "Parse plain text agenda.txt files."
      putStrLn ""
      showHelp
    UnknownArg arg -> do
      putStrLn ("Unknown arg: " <> arg)
      putStrLn ""
      showHelp
    Parsed result ->
      run result

showHelp :: IO ()
showHelp = do
  putStrLn "Usage: cat agenda.txt | agenda-txt {flags} [patterns]"
  putStrLn ""
  putStrLn "Flags"
  putStrLn "  --help      Show this help text"
  putStrLn "  --past      Show events going back in time"
  putStrLn "  --max <n>   The maximum amount of events to show"

data ParsedArgsResult
  = ShowHelp
  | UnknownArg String
  | Parsed ParsedArgs

data ParsedArgs = ParsedArgs
  { direction :: Direction,
    maxEvents :: Word
  }

defaultParsedArgs :: ParsedArgs
defaultParsedArgs =
  ParsedArgs
    { direction = Future,
      maxEvents = 30
    }

parseArgs :: ParsedArgs -> [String] -> ParsedArgsResult
parseArgs parsed args =
  case args of
    [] -> Parsed parsed
    "--help" : _ ->
      ShowHelp
    "--past" : rest ->
      parseArgs parsed {direction = Past} rest
    "--max" : maxEventsStr : rest ->
      case readMaybe maxEventsStr of
        Just maxEvents -> parseArgs parsed {maxEvents = maxEvents} rest
        Nothing -> UnknownArg ("--max " <> maxEventsStr)
    arg : _ ->
      UnknownArg arg

run :: ParsedArgs -> IO ()
run ParsedArgs {direction, maxEvents} = do
  (errs, events) <- readEvents [] []
  traverse_ (\err -> do putStr "Warning: "; putStrLn err) errs
  today' <- today
  let firstDay = Prelude.minimum (startDay <$> events)
  let dayRange =
        case direction of
          Future -> From firstDay Future
          Past -> From today' Past
  let events' =
        Prelude.take (fromIntegral maxEvents) $
          eventsInRange
            dayRange
            (fmap (\event -> (eventOnDay event, event)) events)
  traverse_ (\event -> putStrLn (show event)) events'

readEvents :: [String] -> [Event] -> IO ([String], [Event])
readEvents errs events = do
  isEOF <- hIsEOF stdin
  if isEOF
    then pure (errs, events)
    else do
      line <- hGetLine stdin
      case parseOnly (parserEvent <* endOfInput) line of
        Left err -> readEvents (err : errs) events
        Right event -> readEvents errs (event : events)

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
    durationMinutes :: Maybe Word,
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

eventsInRange :: DayRange -> [(Day -> EventOnDay, a)] -> [(Day, a)]
eventsInRange range events =
  let (direction, days) =
        case range of
          From day' Future -> (Future, [day' ..])
          From day' Past -> (Past, [day', pred day' ..])
          Between start end -> (if start > end then Past else Future, [start .. end])

      keepMatches ::
        [(Day -> EventOnDay, a)] ->
        [Day] ->
        [(Day -> EventOnDay, a)] ->
        [(Day, a)]
      keepMatches _ [] _ = []
      keepMatches [] _ [] = []
      keepMatches nextDayEvents (_ : nextDays) [] =
        keepMatches [] nextDays (Prelude.reverse nextDayEvents)
      keepMatches nextDayEvents (day' : nextDays) (event : nextEvents) =
        case fst event day' of
          Match ->
            -- Intentionally do not use tail-recursion here, so we can lazily
            -- consume from the output of this function.
            (day', snd event) : keepMatches (event : nextDayEvents) (day' : nextDays) nextEvents
          NoMatch ->
            keepMatches (event : nextDayEvents) (day' : nextDays) nextEvents
          NoFurtherMatches direction' ->
            if direction == direction'
              then keepMatches nextDayEvents (day' : nextDays) nextEvents
              else keepMatches (event : nextDayEvents) (day' : nextDays) nextEvents
   in keepMatches [] days events

data EventOnDay
  = Match
  | NoMatch
  | NoFurtherMatches Direction
  deriving (Eq, Ord)

data Direction = Future | Past deriving (Show, Eq, Ord)

eventOnDay :: Event -> Day -> EventOnDay
eventOnDay event day' =
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

parserTimeOfDay :: Parser TimeOfDay
parserTimeOfDay = parser_HMS_opt_S (Just ':')
