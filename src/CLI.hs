module CLI where

import Chronos
import Conduit
import Control.Monad (when)
import Data.Attoparsec.Text
import Data.List (sortOn)
import Data.Text (Text, pack)
import Engine
import qualified Printer.Console
import qualified Printer.Html
import System.Environment (getArgs, lookupEnv)
import qualified System.Exit
import System.IO (Handle, hIsTerminalDevice, hPutStrLn, stderr, stdin, stdout)

main :: IO ()
main = do
  parsedArgs <- parseArgs <$> defaultParsedArgs <*> getArgs
  case parsedArgs of
    ShowHelp -> do
      putStrLn "Parse plain text agenda.txt files."
      putStrLn ""
      showHelp stdout
      putStrLn ""
      showStdinHelp stdout
    ParseError err -> do
      hPutStrLn stderr err
      hPutStrLn stderr ""
      showHelp stderr
      System.Exit.exitFailure
    Parsed result -> do
      isTTY <- hIsTerminalDevice stdin
      when isTTY $ do
        hPutStrLn stderr "Missing agenda.txt input on stdin."
        hPutStrLn stderr ""
        showHelp stderr
        System.Exit.exitFailure
      run result

showHelp :: Handle -> IO ()
showHelp h = do
  hPutStrLn h "Usage: cat agenda.txt | agenda-txt {flags} [patterns]"
  hPutStrLn h ""
  hPutStrLn h "Flags:"
  hPutStrLn h "  --help         Show this help text"
  hPutStrLn h "  --past         Show past instead of future events"
  hPutStrLn h "  --html         Output events in HTML format"
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

showStdinHelp :: Handle -> IO ()
showStdinHelp h = do
  hPutStrLn h "Stdin:"
  hPutStrLn h "  Stdin expects one event per line, with the following format:"
  hPutStrLn h "    2023-11-01 [wed <=2023-12-20] 19:30 +2:00 (CET) Dancing class"
  hPutStrLn h ""
  hPutStrLn h "  We see, in order:"
  hPutStrLn h "    2023-11-01           First occurence of an event (required)"
  hPutStrLn h "    [wed <=2023-12-20]   Extra event days, using CLI patterns"
  hPutStrLn h "    19:30                Event starting time in 24 hour clock"
  hPutStrLn h "    +2:00                Event duration"
  hPutStrLn h "    (CET)                Timezone of the starting time"
  hPutStrLn h "    Dancing class        Event description"
  hPutStrLn h "  All elements but the first occurence date are optional."

data ParsedArgsResult
  = ShowHelp
  | ParseError String
  | Parsed ParsedArgs

data ParsedArgs = ParsedArgs
  { from :: Day,
    direction :: Direction,
    output :: Output,
    dateFilters :: [DateFilter]
  }

data Output = Console | Html

defaultParsedArgs :: IO ParsedArgs
defaultParsedArgs = do
  from' <- maybe "" pack <$> lookupEnv "DEBUG_AGENDA_TXT_TODAY"
  from <-
    case parseOnly (parser_Ymd (Just '-') <* endOfInput) from' of
      Right date -> pure (dateToDay date)
      Left _ -> today
  pure
    ParsedArgs
      { from = from,
        direction = Future,
        output = Console,
        dateFilters = []
      }

parseArgs :: ParsedArgs -> [String] -> ParsedArgsResult
parseArgs parsed args =
  case args of
    [] -> Parsed parsed
    "--help" : _ ->
      ShowHelp
    "--past" : rest ->
      parseArgs parsed {direction = Past} rest
    "--html" : rest ->
      parseArgs parsed {output = Html} rest
    -- These options are intentionally not documented in the API. I have them
    -- for testing purposes only. Should they be used for real, I'd like to
    -- reconsider my design rather than making these 'official'.
    arg : rest ->
      case parseOnly (parserRepeatStep <* endOfInput) (pack arg) of
        Right dateFilter -> parseArgs parsed {dateFilters = dateFilter : (dateFilters parsed)} rest
        Left _ -> ParseError ("Unknown argument: " <> arg)

run :: ParsedArgs -> IO ()
run ParsedArgs {direction, output, from, dateFilters} =
  runResourceT . runConduit $ do
    events <-
      stdinC
        .| decodeUtf8LenientC
        .| linesUnboundedC
        .| concatMapMC (eventOrWarning . parseLine)
        .| sinkList

    let recurrences = fmap eventToRecurrence $ sortOn (fmap startTime . time) events

    days from direction dateFilters
      .| occurrences direction recurrences
      .| case output of
        Console -> Printer.Console.run
        Html -> Printer.Html.run

eventOrWarning :: (MonadIO m) => Either String Event -> m (Maybe Event)
eventOrWarning (Left warning) = do
  liftIO $ hPutStrLn stderr $ "Warning: " <> warning
  pure Nothing
eventOrWarning (Right event) = pure (Just event)

parseLine :: Text -> Either String Event
parseLine = parseOnly (parserEvent <* endOfInput)
