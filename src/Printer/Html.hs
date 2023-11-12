module Printer.Html (run) where

import Chronos
import Conduit
import Control.Exception (mask_)
import Control.Monad (void, when)
import Data.ByteString (putStr)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder.Int as Builder.Int
import Engine
import Text.Blaze
import Text.Blaze.Html5 as Html5
import Text.Blaze.Html5.Attributes as Attr
import Text.Blaze.Renderer.Utf8 (renderMarkupToByteStringIO)
import Prelude hiding (putStr)

run :: ConduitT (Day, Event) Void (ResourceT IO) ()
run =
  bracketP
    ( do
        putStr "<!doctype html><html lang=\"en-US\"><head>"
        putStr "<meta charset=\"utf-8\">"
        putStr "<meta name=\"viewport\" content=\"width=device-width\">"
        putStr "<title>agenda.txt</title>"
        putStr "<style>"
        css
        putStr "</style></head><body>"
    )
    (\_ -> putStr "</body></html>")
    ( \_ ->
        void $
          foldMC
            ( \prevDay (day', event) -> do
                let html' = occurrenceToHtml day' event prevDay
                liftIO $ mask_ $ renderMarkupToByteStringIO putStr html'
                pure day'
            )
            (Day 0)
    )

occurrenceToHtml :: Day -> Event -> Day -> Html
occurrenceToHtml day' event prevDay =
  let date = dayToDate day'
      prevDate = dayToDate prevDay
      newMonth =
        dateYear prevDate /= dateYear date
          || (dateMonth prevDate /= dateMonth date)
      weekNo d = (getDay d + 2) `Prelude.div` 7
      newWeek = weekNo prevDay /= weekNo day'
      newDay = prevDate /= date

      -- Calculating the week number, which the ISO standard dictates is the
      -- first week that contains at least 4 days of that year. It follows that
      -- January 4th is always in the first week of the year.
      year = dateYear (dayToDate day')
      jan4 = ordinalDateToDay $ OrdinalDate year (DayOfYear 4)
      yearWeekNo = 1 + ((52 + weekNo day' - weekNo jan4) `mod` 52)
   in do
        when newMonth $ h2 ! class_ "month-header" $ do
          toMarkup $ caseMonth shortMonth (dateMonth date)
          " "
          toMarkup . getYear $ dateYear date
        when newWeek $ Html5.div ! class_ "week-seperator" $ toMarkup yearWeekNo
        let dayClass = if newDay then "event new-day" else "event"
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
    toValue (builder_Ymd (Just '-') (dayToDate (startDay event)))
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

css :: IO ()
css = do
  putStr "body {"
  putStr "  font-family: helvetica;"
  putStr "  max-width: 30rem;"
  putStr "  margin: auto;"
  putStr "  padding: 0.5rem;"
  putStr "}"
  putStr ".month-header {"
  putStr "  font-size: 1em;"
  putStr "  margin-bottom: 0.3rem;"
  putStr "}"
  putStr "* + .month-header {"
  putStr "  margin-top: 2rem;"
  putStr "}"
  putStr ".week-seperator {"
  putStr "  margin: 0.4rem 0;"
  putStr "  display: flex;"
  putStr "  justify-content: space-between;"
  putStr "  align-items: center;"
  putStr "  gap: 0.5rem;"
  putStr "  height: 1px;"
  putStr "  font-size: 0.8em;"
  putStr "}"
  putStr ".week-seperator::before {"
  putStr "  content: ' ';"
  putStr "  flex-grow: 1;"
  putStr "  height: 1px;"
  putStr "  border: none;"
  putStr "  background-color: #ccc;"
  putStr "}"
  putStr ".event {"
  putStr "  margin: 0;"
  putStr "  margin-left: 4rem;"
  putStr "  font-variant-numeric: oldstyle-nums;"
  putStr "}"
  putStr ".event:not(.new-day) .date {"
  putStr "  visibility: hidden;"
  putStr "}"
  putStr ".date {"
  putStr "  width: 4rem;"
  putStr "  margin-left: -4rem;"
  putStr "  display: inline-block;"
  putStr "}"
  putStr ".time, .duration {"
  putStr "  margin-right: 0.3rem;"
  putStr "  font-style: italic;"
  putStr "}"
