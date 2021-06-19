module LoggyCore where

import Data.Time
import GHC.Exts (sortWith)

type LogLine = String
type DateFormat = String

extractTimestamp :: DateFormat -> LogLine -> UTCTime
extractTimestamp dformat line = parsedTs
    where
        parsedTs = case timeParser line of
            [(ts, _)] -> ts
            _ -> error "Invalid log line!"
        timeParser = readSTime True defaultTimeLocale dformat  

mergeLogLines :: DateFormat -> [LogLine] -> [LogLine] -> [LogLine]
mergeLogLines dformat l1 l2 = sortedLogLines
    where
        sortedLogLines = snd <$> sortedLogTuples
        sortedLogTuples = sortWith fst $ zip logTimestamps l3
        logTimestamps = extractTimestamp dformat <$> l3
        l3 = l1 ++ l2