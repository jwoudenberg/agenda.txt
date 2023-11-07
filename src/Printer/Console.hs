module Printer.Console (run) where

import Chronos
import Conduit
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.Builder.Int as Builder.Int
import Engine

run :: ConduitT (Day, Event) Void IO ()
run = loop .| encodeUtf8C .| stdoutC
  where
    loop :: (Monad m) => ConduitT (Day, Event) Text m ()
    loop = do
      occurrence <- headC
      whenJust occurrence $ \(day', event) -> do
        let date = dayToDate day'
        yield "["
        yield $ caseDayOfWeek shortDayOfWeek (dateToDayOfWeek date)
        yield " "
        yield . intToText . getDayOfMonth $ dateDay date
        yield " "
        yield $ caseMonth shortMonth (dateMonth date)
        yield " "
        yield . intToText . getYear $ dateYear date
        whenJust (time event) $ \time' -> do
          yield " "
          yield . intToText . timeOfDayHour $ startTime time'
          yield ":"
          yield . intToText2 . timeOfDayMinute $ startTime time'
          whenJust (durationMinutes time') $ \durationMinutes' -> do
            yield "+"
            yield . intToText . fromIntegral $ durationMinutes' `div` 60
            yield ":"
            yield . intToText2 . fromIntegral $ durationMinutes' `mod` 60
          whenJust (timezone time') $ \timezone' -> do
            yield " ("
            yield timezone'
            yield ")"
        yield "] "
        yield $ description event
        yield "\n"
        loop

intToText :: Int -> Text
intToText = toStrict . toLazyText . Builder.Int.decimal

intToText2 :: Int -> Text
intToText2 n =
  toStrict . toLazyText $
    if n < 10
      then "0" <> Builder.Int.decimal n
      else Builder.Int.decimal n

shortDayOfWeek :: DayOfWeekMatch Text
shortDayOfWeek = buildDayOfWeekMatch "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"

shortMonth :: MonthMatch Text
shortMonth = buildMonthMatch "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"
