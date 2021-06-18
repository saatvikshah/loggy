module LoggyCore where

import Data.Time

type DateFormat = String

extract_timestamp :: DateFormat -> String -> Maybe UTCTime
extract_timestamp dformat line = parsed_ts
    where
        parsed_ts = case time_parser line of
            [(ts, _)] -> Just ts
            _ -> Nothing
        time_parser = readSTime True defaultTimeLocale dformat