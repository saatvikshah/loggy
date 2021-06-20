module LoggyCore where

import Data.Time
import GHC.Exts (sortWith)

type LogLine = String
type LogFileLines = [LogLine]
type DateFormat = String

extractTimestamp :: DateFormat -> LogLine -> UTCTime
extractTimestamp dformat line = parsedTs
    where
        parsedTs = case timeParser line of
            [(ts, _)] -> ts
            _ -> error "Invalid log line!"
        timeParser = readSTime True defaultTimeLocale dformat  

mergeLogLines :: DateFormat -> [LogFileLines] -> [LogLine]
mergeLogLines dformat lfiles = mergeLogLines' dFormatFilePairs
    where
        dFormatFilePairs = zip dFormatPerFile lfiles
        dFormatPerFile = replicate numFiles dformat
        numFiles = length lfiles

mergeLogLines' :: [(DateFormat, LogFileLines)] -> [LogLine]
mergeLogLines' dfmtFilePairs = sortedLogLines
    where
        sortedLogLines = snd <$> sortedLogTuples
        sortedLogTuples = sortWith fst $ zip logTimestamps logLines
        logTimestamps = concat $ zipWith ($) (fmap.extractTimestamp <$> dformats) linesPerLf
        logLines = concat linesPerLf
        dformats = map fst dfmtFilePairs
        linesPerLf = map snd dfmtFilePairs