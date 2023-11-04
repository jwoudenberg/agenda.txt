module AgendaTxt where

import Chronos
import Data.Attoparsec.Text
import Data.Char (isAlpha)
import Data.Foldable (traverse_)
import Data.Maybe (catMaybes)
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
  case minimum' (startDay <$> events) of
    Nothing -> pure ()
    Just firstDay ->
      let dayRange =
            case direction of
              Future -> From firstDay Future
              Past -> From today' Past
          events' =
            Prelude.take (fromIntegral maxEvents) $
              occurrences
                dayRange
                (fmap eventToRecurrence events)
       in traverse_ (\event -> putStrLn (show event)) events'

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

data Recurrence a = Recurrence
  { onDay :: Day -> Bool,
    event :: a,
    minDay :: Maybe Day,
    maxDay :: Maybe Day
  }

occurrences :: DayRange -> [Recurrence a] -> [(Day, a)]
occurrences range recurrences =
  gen [] days recurrences
  where
    (direction, days) =
      case range of
        From day' Future -> (Future, [day' ..])
        From day' Past -> (Past, [day', pred day' ..])
        Between start end -> (if start > end then Past else Future, [start .. end])

    gen ::
      [Recurrence a] ->
      [Day] ->
      [Recurrence a] ->
      [(Day, a)]
    gen _ [] _ = []
    gen [] _ [] = []
    gen nextDayRecurrences (_ : nextDays) [] =
      gen [] nextDays (Prelude.reverse nextDayRecurrences)
    gen nextDayRecurrences days'@(day' : _) (recurrence : nextRecurrences)
      | onDay recurrence day' =
          -- Intentionally do not use tail-recursion here, so we can lazily
          -- consume from the output of this function.
          (day', event recurrence) : gen (recurrence : nextDayRecurrences) days' nextRecurrences
    gen nextDayRecurrences days'@(day' : _) (recurrence : nextRecurrences)
      | direction == Past,
        Just minDay' <- minDay recurrence,
        day' < minDay' =
          gen nextDayRecurrences days' nextRecurrences
    gen nextDayRecurrences days'@(day' : _) (recurrence : nextRecurrences)
      | direction == Future,
        Just maxDay' <- maxDay recurrence,
        day' > maxDay' =
          gen nextDayRecurrences days' nextRecurrences
    gen nextDayRecurrences days' (recurrence : nextRecurrences) =
      gen (recurrence : nextDayRecurrences) days' nextRecurrences

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
    rules ->
      Recurrence
        { onDay =
            \day' ->
              Prelude.all
                (matchesFilter day' (dayToDate day'))
                rules,
          event = event,
          minDay = Just (startDay event),
          maxDay = maximum' $ catMaybes $ fmap filterUpperBound $ rules
        }

minimum' :: (Foldable f, Ord a) => f a -> Maybe a
minimum' = Prelude.foldr keepLower Nothing
  where
    keepLower :: (Ord a) => a -> Maybe a -> Maybe a
    keepLower x Nothing = Just x
    keepLower x (Just y) = if x < y then Just x else Just y

maximum' :: (Foldable f, Ord a) => f a -> Maybe a
maximum' = Prelude.foldr keepUpper Nothing
  where
    keepUpper :: (Ord a) => a -> Maybe a -> Maybe a
    keepUpper x Nothing = Just x
    keepUpper x (Just y) = if x > y then Just x else Just y

filterUpperBound :: RepeatFilter -> Maybe Day
filterUpperBound = undefined

matchesFilter :: Day -> Date -> RepeatFilter -> Bool
matchesFilter _ date (DatePattern pattern) = matchesPattern date pattern
matchesFilter _ date (NotDatePattern pattern) = not (matchesPattern date pattern)
matchesFilter _ date (WeekDay weekDay) = weekDay == dateToDayOfWeek date
matchesFilter day' _ (EndDate endDate) = day' <= endDate

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
