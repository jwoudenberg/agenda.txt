module Test (Test.main) where

import AgendaTxt
import Chronos hiding (day)
import Data.Attoparsec.Text (parseOnly)
import Data.List (intersperse, sort)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text.Lazy
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Main (defaultMain)
import Hedgehog.Range as Range
import qualified Torsor

main :: IO ()
main =
  defaultMain
    [ checkParallel (Group "parserEvent" [("fuzzed events", fuzzedEventsTest)]),
      checkParallel
        ( Group
            "eventsInRange"
            [ ("closed date ranges", closedDateRangesTest),
              ("open-ended future date ranges", openEndedFutureDateRangesTest),
              ("open-ended past date ranges", openEndedPastDateRangesTest)
            ]
        )
    ]

closedDateRangesTest :: Property
closedDateRangesTest = property $ do
  start <- forAll genDay
  end <- forAll $ flip Torsor.add start <$> int (linearFrom 0 0 100)
  events <-
    forAll $
      list (linear 0 10) $
        getTestEvent
          ()
          (Torsor.add (-100) start)
          (Torsor.add 100 end)
  let allDuringDays =
        Prelude.filter (\day' -> day' >= start && day' <= end) $
          foldMap (Set.toList . eventDays) events
  let results = eventsInRange (Between start end) (eventValue <$> events)
  fmap fst results === sort allDuringDays

openEndedFutureDateRangesTest :: Property
openEndedFutureDateRangesTest = property $ do
  start <- forAll genDay
  events <-
    forAll $
      list (linear 0 10) $
        getTestEvent
          ()
          (Torsor.add (-100) start)
          (Torsor.add 100 start)
  let allOnOrAfterDays = Prelude.filter (>= start) $ foldMap (Set.toList . eventDays) events
  let results = eventsInRange (From start Future) (eventValue <$> events)
  fmap fst results === sort allOnOrAfterDays

openEndedPastDateRangesTest :: Property
openEndedPastDateRangesTest = property $ do
  start <- forAll genDay
  events <-
    forAll $
      list (linear 0 10) $
        getTestEvent
          ()
          (Torsor.add (-100) start)
          (Torsor.add 100 start)
  let allOnOrBeforeDays = Prelude.filter (<= start) $ foldMap (Set.toList . eventDays) events
  let results = eventsInRange (From start Past) (eventValue <$> events)
  fmap fst results === reverse (sort allOnOrBeforeDays)

data TestEvent a = TestEvent
  { eventTag :: a,
    eventDays :: Set.Set Day
  }
  deriving (Show)

getTestEvent :: a -> Day -> Day -> Gen (TestEvent a)
getTestEvent tag start end = do
  TestEvent tag <$> set (linear 1 10) (enum start end)

eventValue :: TestEvent a -> (Day -> EventOnDay, a)
eventValue TestEvent {eventTag, eventDays} =
  let isMatch day' | day' < Set.findMin eventDays = NoFurtherMatches Past
      isMatch day' | day' > Set.findMax eventDays = NoFurtherMatches Future
      isMatch day' | Set.member day' eventDays = Match
      isMatch _ = NoMatch
   in (isMatch, eventTag)

fuzzedEventsTest :: Property
fuzzedEventsTest = property $ do
  event <- forAll genEvent
  eventLine <- forAll (builderEvent event)
  let parseResult = parseOnly parserEvent $ Data.Text.Lazy.toStrict (Builder.toLazyText eventLine)
  case parseResult of
    Left message -> do
      annotateShow message
      failure
    Right parsedEvent ->
      event === parsedEvent

builderEvent :: Event -> Gen Builder
builderEvent event =
  builderDay (startDay event)
    <> builderSpacing
    <> builderRepeatRule (repeatRule event)
    <> builderSpacing1
    <> Prelude.maybe mempty builderTime (time event)
    <> builderSpacing
    <> pure (Builder.fromText (description event))

builderSpacing :: Gen Builder
builderSpacing = Builder.fromText <$> Gen.text (Range.linear 0 2) (pure ' ')

builderSpacing1 :: Gen Builder
builderSpacing1 = Builder.fromText <$> Gen.text (Range.linear 1 2) (pure ' ')

builderDay :: Day -> Gen Builder
builderDay day =
  pure $ builder_Ymd (Just '-') (dayToDate day)

builderRepeatRule :: [RepeatFilter] -> Gen Builder
builderRepeatRule [] = element ["", "[]", "[ ]"]
builderRepeatRule repeatFilters =
  pure "["
    <> builderSpacing
    <> mconcat (intersperse builderSpacing1 $ fmap builderRepeatFilter repeatFilters)
    <> builderSpacing
    <> pure "]"

builderRepeatFilter :: RepeatFilter -> Gen Builder
builderRepeatFilter repeatFilter =
  case repeatFilter of
    DatePattern pattern -> builderDatePattern pattern
    NotDatePattern pattern -> pure "!" <> builderDatePattern pattern
    WeekDay weekDay ->
      pure $
        caseDayOfWeek
          (buildDayOfWeekMatch "sun" "mon" "tue" "wed" "thu" "fri" "sat")
          weekDay
    EndDate day -> pure "<=" <> builderDay day

builderDatePattern :: DatePattern -> Gen Builder
builderDatePattern datePattern =
  Prelude.maybe mempty (builderInt . getYear) (year datePattern)
    <> pure "-"
    <> Prelude.maybe mempty (builderInt . getMonth) (month datePattern)
    <> pure "-"
    <> Prelude.maybe mempty (builderInt . getDayOfMonth) (day datePattern)

builderInt :: Int -> Gen Builder
builderInt = pure . fromString . show

builderTime :: EventTime -> Gen Builder
builderTime eventTime =
  builderTimeOfDay (startTime eventTime)
    <> builderSpacing
    <> Prelude.maybe mempty builderDuration (durationMinutes eventTime)
    <> builderSpacing
    <> Prelude.maybe mempty builderTimezone (timezone eventTime)

builderTimeOfDay :: TimeOfDay -> Gen Builder
builderTimeOfDay timeOfDay =
  pure $ builder_HMS SubsecondPrecisionAuto (Just ':') timeOfDay

builderDuration :: Int -> Gen Builder
builderDuration minutes =
  pure "+"
    <> builderInt (minutes `div` 60)
    <> pure ":"
    <> builderInt (minutes `mod` 60)

builderTimezone :: Text -> Gen Builder
builderTimezone timezone =
  pure $ "(" <> Builder.fromText timezone <> ")"

genEvent :: Gen Event
genEvent =
  Event
    <$> genDay
    <*> list (linear 0 4) genRepeatFilter
    <*> Gen.maybe genEventTime
    <*> text (linear 0 20) unicode

genDay :: Gen Day
genDay = Day <$> int (linear 0 10000)

genRepeatFilter :: Gen RepeatFilter
genRepeatFilter =
  choice
    [ DatePattern <$> genDatePattern,
      NotDatePattern <$> genDatePattern,
      WeekDay <$> genDayOfWeek,
      EndDate <$> genDay
    ]

genDatePattern :: Gen DatePattern
genDatePattern =
  DatePattern_
    <$> Gen.maybe (Year <$> int (Range.linear 1800 2400))
    <*> Gen.maybe (Month <$> int (Range.linear 0 11))
    <*> Gen.maybe (DayOfMonth <$> int (Range.linear 0 31))

genDayOfWeek :: Gen DayOfWeek
genDayOfWeek = element [monday, tuesday, wednesday, thursday, friday, saturday, sunday]

genEventTime :: Gen EventTime
genEventTime =
  EventTime
    <$> genTimeOfDay
    <*> Gen.maybe (int (Range.linear 0 1400))
    <*> Gen.maybe (text (Range.linear 2 4) upper)

genTimeOfDay :: Gen TimeOfDay
genTimeOfDay =
  TimeOfDay
    <$> Gen.int (Range.linear 0 23)
    <*> Gen.int (Range.linear 0 59)
    <*> pure 0
