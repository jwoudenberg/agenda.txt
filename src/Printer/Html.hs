module Printer.Html (run) where

import Engine
import Chronos
import Conduit
import Control.Monad (when)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Text.Blaze as Blaze
import Text.Blaze.Html5 as Html5
import Text.Blaze.Renderer.Utf8 (renderMarkupToByteStringIO)

run :: ConduitT (Day, Event) Void IO ()
run = do
  occurrenceElems <- fst <$> foldlC occurrenceToHtml (mempty, Nothing)
  liftIO . renderMarkupToByteStringIO ByteString.putStr . html $ do
    Html5.head $ do
      title "agenda.txt"
    body occurrenceElems

occurrenceToHtml :: (Html, Maybe Date) -> (Day, Event) -> (Html, Maybe Date)
occurrenceToHtml (prevHtml, prevDate) (day', event) =
  ( prevHtml <> newHtml,
    Just date
  )
  where
    date = dayToDate day'
    newMonth =
      nothingOr (/= dateYear date) (dateYear <$> prevDate)
        || nothingOr (/= dateMonth date) (dateMonth <$> prevDate)
    weekNo d = (getDay d + 2) `Prelude.div` 7
    newWeek = nothingOr (/= weekNo day') (weekNo . dateToDay <$> prevDate)
    newHtml = do
      when newMonth $ h2 $ do
        Blaze.toMarkup $ caseMonth shortMonth (dateMonth date)
        Blaze.string " "
        Blaze.toMarkup . getYear $ dateYear date
      when (not newMonth && newWeek) hr
      p $ do
        Blaze.string "["
        Blaze.toMarkup $ caseDayOfWeek shortDayOfWeek (dateToDayOfWeek date)
        Blaze.string " "
        Blaze.toMarkup . getDayOfMonth $ dateDay date
        whenJust (Engine.time event) $ \time' -> do
          Blaze.string " "
          Blaze.toMarkup . timeOfDayHour $ startTime time'
          Blaze.string ":"
          Blaze.toMarkup . timeOfDayMinute $ startTime time'
          whenJust (durationMinutes time') $ \durationMinutes' -> do
            Blaze.string "+"
            Blaze.toMarkup $ durationMinutes' `Prelude.div` 60
            Blaze.string ":"
            Blaze.toMarkup $ durationMinutes' `mod` 60
          whenJust (timezone time') $ \timezone' -> do
            Blaze.string " ("
            Blaze.toMarkup timezone'
            Blaze.string ")"
        Blaze.toMarkup $ description event

shortDayOfWeek :: DayOfWeekMatch Text
shortDayOfWeek = buildDayOfWeekMatch "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"

shortMonth :: MonthMatch Text
shortMonth = buildMonthMatch "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Set" "Oct" "Nov" "Dec"
