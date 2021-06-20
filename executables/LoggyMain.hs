module Main where

import Options.Applicative
import LoggyCore (mergeLogLines, DateFormat, MergeResult(MkMergeResult), WarnCode(InvalidLogLine))
import Control.Monad (when)
import System.IO (hPutStrLn, stderr)


type FileName = String
type InputArg = String

data LoggyArg = LoggyArg
  { argFilename      :: FileName
  , argDateFormat    :: DateFormat
  } deriving Show

parseInputArgs :: Parser [InputArg]
parseInputArgs = some parseInputArg
  where
    parseInputArg:: Parser InputArg
    parseInputArg = strOption
        (short 'i'
        <> metavar "FILE(with optional @DATE_FORMAT)"
        <> help "Name of the file and its optional date format separated by @. Repeatable arg.")

parseCommonDateFormatArg :: Parser DateFormat
parseCommonDateFormatArg = strOption
      (long "format"
      <> metavar "DATE_FORMAT"
      <> help "common date format")

parseLoggyArgs :: Parser [LoggyArg]
parseLoggyArgs = map.makeLoggyArg <$> parseCommonDateFormatArg <*> parseInputArgs
  where fileName   = takeWhile (/='@')
        dateFormat = tail . dropWhile (/='@')
        makeLoggyArg :: DateFormat -> InputArg -> LoggyArg
        makeLoggyArg fmt inpt = if '@'`elem`inpt then
                                 LoggyArg (fileName inpt) (dateFormat inpt)
                                else
                                 LoggyArg inpt fmt  

main :: IO ()
main = runMain =<< execParser opts
  where
    opts = info (helper <*> parseLoggyArgs)
      ( fullDesc
     <> progDesc "Merge and manipulate log files."
     <> header "loggy" )

runMain :: [LoggyArg] -> IO ()
runMain loggyArgs = do
  linesPerFile <- (fmap.fmap) lines (mapM readFile fileNames)
  let dFmtFileLinesPairs = zip dateFormats linesPerFile
  let MkMergeResult mergedLines warnCode = mergeLogLines dFmtFileLinesPairs
  when (warnCode == InvalidLogLine) $ hPutStrLn stderr "[warn] Invalid log line(s) in input, ignoring."
  putStrLn $ unlines mergedLines
    where
      fileNames = argFilename <$> loggyArgs
      dateFormats = argDateFormat <$> loggyArgs
