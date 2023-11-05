module AgendaTxt where

import Chronos
import Conduit
import Control.Monad (when)
import Data.Attoparsec.Text
import Data.Char (isAlpha)
import Data.Foldable (for_)
import Data.Maybe (catMaybes)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text, pack)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import System.Environment (getArgs)
import qualified System.Exit
import System.IO (Handle, hPutStrLn, stderr, stdout)
import Text.Read (readMaybe)
import qualified Torsor

main :: IO ()
main = do
  parsedArgs <- parseArgs <$> defaultParsedArgs <*> getArgs
  case parsedArgs of
    ShowHelp -> do
      putStrLn "Parse plain text agenda.txt files."
      putStrLn ""
      showHelp stdout
    ParseError err -> do
      hPutStrLn stderr err
      hPutStrLn stderr ""
      showHelp stderr
      System.Exit.exitFailure
    Parsed result ->
      run result

showHelp :: Handle -> IO ()
showHelp h = do
  hPutStrLn h "Usage: cat agenda.txt | agenda-txt {flags} [patterns]"
  hPutStrLn h ""
  hPutStrLn h "Flags:"
  hPutStrLn h "  --help              Show this help text"
  hPutStrLn h "  --past              Show past instead of future events"
  hPutStrLn h "  --from YYYY-MM-DD   Choose a different starting date than today"
  hPutStrLn h ""
  hPutStrLn h "Patterns:"
  hPutStrLn h "  YYYY-MM-DD     Matches a particular date. Year, month, or day"
  hPutStrLn h "                 can be left out, for instance:"
  hPutStrLn h "                 1989-1-11   Matches only January 11th 1989"
  hPutStrLn h "                 --1-11      Matches January 11th every year"
  hPutStrLn h "                 --1-        Matches every day in January"
  hPutStrLn h ""
  hPutStrLn h "  !YYYY-MM-DD    Exclude a particular date. Year, month, or day"
  hPutStrLn h "                 can be left out as in the pattern above."
  hPutStrLn h ""
  hPutStrLn h "  <=YYYY-MM-DD   Matches all days before a particular date."
  hPutStrLn h ""
  hPutStrLn h "  mon .. sun     Matches a particular day of the week"

data ParsedArgsResult
  = ShowHelp
  | ParseError String
  | Parsed ParsedArgs

data ParsedArgs = ParsedArgs
  { from :: Day,
    direction :: Direction,
    dateFilters :: [DateFilter],
    maxResults :: Int,
    maxIntervalDays :: Int
  }

defaultParsedArgs :: IO ParsedArgs
defaultParsedArgs = do
  from <- today
  pure
    ParsedArgs
      { from = from,
        direction = Future,
        dateFilters = [],
        maxResults = 10_000,
        maxIntervalDays = 365_00 -- One century
      }

parseArgs :: ParsedArgs -> [String] -> ParsedArgsResult
parseArgs parsed args =
  case args of
    [] -> Parsed parsed
    "--help" : _ ->
      ShowHelp
    "--past" : rest ->
      parseArgs parsed {direction = Past} rest
    "--from" : dateString : rest ->
      case parseOnly (parser_Ymd (Just '-') <* endOfInput) (pack dateString) of
        Right date -> parseArgs parsed {from = dateToDay date} rest
        Left _ -> ParseError ("Can't parse --from date: " <> dateString)
    -- These options are intentionally not documented in the API. I have them
    -- for testing purposes only. Should they be used for real, I'd like to
    -- reconsider my design rather than making these 'official'.
    "--max-results" : amountString : rest ->
      case readMaybe amountString of
        Just amount -> parseArgs parsed {maxResults = amount} rest
        Nothing -> ParseError ("Can't parse amount in --max-results parameter: " <> amountString)
    "--max-interval-days" : amountString : rest ->
      case readMaybe amountString of
        Just amount -> parseArgs parsed {maxIntervalDays = amount} rest
        Nothing -> ParseError ("Can't parse amount in --max-interval-days parameter: " <> amountString)
    arg : rest ->
      case parseOnly (parserRepeatStep <* endOfInput) (pack arg) of
        Right dateFilter -> parseArgs parsed {dateFilters = dateFilter : (dateFilters parsed)} rest
        Left _ -> ParseError ("Unknown argument: " <> arg)

run :: ParsedArgs -> IO ()
run ParsedArgs {direction, from, dateFilters, maxResults, maxIntervalDays} = runConduit $ do
  let maxDay = Torsor.add maxIntervalDays from
  let minDay = Torsor.add (-maxIntervalDays) from
  recurrences <-
    stdinC
      .| decodeUtf8LenientC
      .| linesUnboundedC
      .| concatMapMC (eventOrWarning . parseLine)
      .| mapC eventToRecurrence
      .| sinkList

  days from direction dateFilters
    .| occurrences direction recurrences
    .| takeC maxResults
    .| takeWhileC (\(day', _) -> day' >= minDay && day' <= maxDay)
    .| printOccurrences
    .| encodeUtf8C
    .| stdoutC

eventOrWarning :: Either String Event -> IO (Maybe Event)
eventOrWarning (Left warning) = do
  hPutStrLn stderr $ "Warning: " <> warning
  pure Nothing
eventOrWarning (Right event) = pure (Just event)

parseLine :: Text -> Either String Event
parseLine = parseOnly (parserEvent <* endOfInput)

printOccurrences :: (Monad m) => ConduitT (Day, Event) Text m ()
printOccurrences = do
  occurrence <- headC
  whenJust occurrence $ \(day', event) -> do
    yield . toStrict . toLazyText $ builder_Ymd (Just '-') (dayToDate day')
    yield " "
    yield $ description event
    yield "\n"
    printOccurrences

data DateFilter
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
            (\bound -> takeWhileC (<= bound)) <$> filtersUpperBound dateFilters
          Past ->
            (\bound -> takeWhileC (>= bound)) <$> filtersLowerBound dateFilters
   in case boundFilter of
        Nothing ->
          daysUnbounded
            .| filterC (matchesFilters dateFilters)
        Just boundFilter' ->
          daysUnbounded
            .| boundFilter'
            .| filterC (matchesFilters dateFilters)

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
        { onDay = matchesFilters filters,
          event = event,
          minDay = Just (startDay event),
          maxDay = filtersUpperBound filters
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

filtersUpperBound :: [DateFilter] -> Maybe Day
filtersUpperBound = minimum' . catMaybes . fmap filterUpperBound

filterUpperBound :: DateFilter -> Maybe Day
filterUpperBound (DatePattern pattern) = lastDayOfYear <$> year pattern
filterUpperBound (NotDatePattern _) = Nothing
filterUpperBound (WeekDay _) = Nothing
filterUpperBound (EndDate endDate) = Just endDate

lastDayOfYear :: Year -> Day
lastDayOfYear year =
  pred . ordinalDateToDay $ OrdinalDate (nextYear year) (DayOfYear 1)

filtersLowerBound :: [DateFilter] -> Maybe Day
filtersLowerBound = maximum' . catMaybes . fmap filterUpperBound

filterLowerBound :: DateFilter -> Maybe Day
filterLowerBound (DatePattern pattern) = firstDayOfYear <$> year pattern
filterLowerBound (NotDatePattern _) = Nothing
filterLowerBound (WeekDay _) = Nothing
filterLowerBound (EndDate _) = Nothing

firstDayOfYear :: Year -> Day
firstDayOfYear year =
  ordinalDateToDay $ OrdinalDate year (DayOfYear 1)

nextYear :: Year -> Year
nextYear = Year . (+ 1) . getYear

matchesFilters :: [DateFilter] -> Day -> Bool
matchesFilters dateFilters day' =
  all (matchesFilter day' (dayToDate day')) dateFilters

matchesFilter :: Day -> Date -> DateFilter -> Bool
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

parserTimeOfDay :: Parser TimeOfDay
parserTimeOfDay = parser_HMS_opt_S (Just ':')
