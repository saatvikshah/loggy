module LoggyCore where

import Data.Time
import Data.Maybe
import GHC.Exts (sortWith)

type LogLine = String
type LogFileLines = [LogLine]
type DateFormat = String
data WarnCode = NoWarning | InvalidLogLine deriving (Eq, Show)
data MergeResult = MkMergeResult
    {
        mergeResultOutput   :: LogFileLines,
        mergeResultWarning  :: WarnCode
    }  deriving (Eq, Show)

extractTimestamp :: DateFormat -> LogLine -> Maybe UTCTime
extractTimestamp dformat line = parsedTs
    where
        parsedTs = case timeParser line of
            [(ts, _)] -> Just ts
            _ -> Nothing
        timeParser = readSTime True defaultTimeLocale dformat

mergeLogLines :: [(DateFormat, LogFileLines)] -> MergeResult
mergeLogLines dfmtFilePairs = MkMergeResult sortedLogLines warnCode
    where
        warnCode :: WarnCode
        warnCode
            | length sortedLogLines /= length tsLineTuples = InvalidLogLine
            | otherwise                                    = NoWarning
        sortedLogLines = snd <$> sortedLogTuples
        sortedLogTuples = sortWith fst tsLineTuplesFiltered
        tsLineTuplesFiltered = filter (isJust.fst) tsLineTuples
        tsLineTuples = zip logTimestamps logLines
        logTimestamps = concat $ zipWith ($) (fmap.extractTimestamp <$> dformats) linesPerLf
        logLines = concat linesPerLf
        dformats = map fst dfmtFilePairs
        linesPerLf = map snd dfmtFilePairs