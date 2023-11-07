module Printer.Html (run) where

import Chronos
import Conduit
import Control.Monad (when)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder.Int as Builder.Int
import Engine
import Text.Blaze
import Text.Blaze.Html5 as Html5
import Text.Blaze.Html5.Attributes as Attr
import Text.Blaze.Renderer.Utf8 (renderMarkupToByteStringIO)

run :: ConduitT (Day, Event) Void IO ()
run = do
  occurrenceElems <- fst <$> foldlC occurrenceToHtml (mempty, Nothing)
  liftIO . renderMarkupToByteStringIO ByteString.putStr $
    html ! lang "en-US" $ do
      Html5.head $ do
        Html5.meta ! charset "UTF-8"
        Html5.title "agenda.txt"
        Html5.style css
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
    newDay = nothingOr (/= date) prevDate
    newHtml = do
      when newMonth $ h2 ! class_ "month-header" $ do
        toMarkup $ caseMonth shortMonth (dateMonth date)
        " "
        toMarkup . getYear $ dateYear date
      when (not newMonth && newWeek) (hr ! class_ "week-seperator")
      let dayClass = if (not newMonth && not newWeek && newDay) then "event new-day" else "event"
      p ! class_ dayClass $ do
        Html5.time ! datetimeForEvent event ! class_ "datetime" $ do
          Html5.span ! class_ "date" $ do
            toMarkup $ caseDayOfWeek shortDayOfWeek (dateToDayOfWeek date)
            " "
            toMarkup . getDayOfMonth $ dateDay date
          whenJust (Engine.time event) $ \time' -> do
            Html5.span ! class_ "time" $ do
              toMarkup . timeOfDayHour $ startTime time'
              ":"
              toMarkup . twoDigitInt . timeOfDayMinute $ startTime time'
              whenJust (timezone time') $ \timezone' -> do
                " ("
                toMarkup timezone'
                ")"
        whenJust (durationMinutes =<< Engine.time event) $ \durationMinutes' -> do
          Html5.time ! datetimeForDuration durationMinutes' ! class_ "duration" $ do
            "+"
            toMarkup $ durationMinutes' `Prelude.div` 60
            ":"
            toMarkup . twoDigitInt . fromIntegral $ durationMinutes' `mod` 60
        Html5.span ! class_ "description" $ toMarkup (description event)

datetimeForEvent :: Event -> Attribute
datetimeForEvent event =
  datetime $
    toValue (builder_Ymd (Just ':') (dayToDate (startDay event)))
      <> case Engine.time event of
        Nothing -> mempty
        Just time' ->
          "T"
            <> toValue (twoDigitInt (timeOfDayHour (startTime time')))
            <> ":"
            <> toValue (twoDigitInt (timeOfDayMinute (startTime time')))

-- This encodes a duration as a standard duration string. See docs here:
-- https://html.spec.whatwg.org/multipage/common-microsyntaxes.html#valid-duration-string
datetimeForDuration :: Word -> Attribute
datetimeForDuration minutes =
  datetime $ "PT" <> toValue minutes <> "M"

twoDigitInt :: Int -> Builder
twoDigitInt n =
  if n < 10
    then "0" <> Builder.Int.decimal n
    else Builder.Int.decimal n

shortDayOfWeek :: DayOfWeekMatch Text
shortDayOfWeek = buildDayOfWeekMatch "sun" "mon" "tue" "wed" "thu" "fri" "sat"

shortMonth :: MonthMatch Text
shortMonth =
  buildMonthMatch
    "January"
    "February"
    "March"
    "April"
    "May"
    "June"
    "July"
    "August"
    "September"
    "October"
    "November"
    "December"

css :: Markup
css = do
  "body {"
  "  font-family: helvetica;"
  "  max-width: 30rem;"
  "  margin: auto;"
  "  padding: 0.5rem;"
  "}"
  ".month-header {"
  "  font-size: 1em;"
  "  margin-bottom: 0.3rem;"
  "}"
  "* + .month-header {"
  "  margin-top: 2rem;"
  "}"
  ".week-seperator {"
  "  height: 1px;"
  "  border: none;"
  "  background-color: #ccc;"
  "}"
  ".event {"
  "  margin: 0;"
  "  margin-left: 4rem;"
  "  font-variant-numeric: oldstyle-nums;"
  "}"
  ".event + .event.new-day {"
  "  margin-top: 0.5rem;"
  "}"
  ".date {"
  "  width: 4rem;"
  "  margin-left: -4rem;"
  "  display: inline-block;"
  "}"
  ".time, .duration {"
  "  margin-right: 0.3rem;"
  "  font-style: italic;"
  "}"
