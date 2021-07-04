module Test.LoggyCore (loggycore) where

import Test.Hspec (shouldBe, Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog, (===), forAll, Gen)
import Data.Time
import Data.Time.Clock.System
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Gen as Gen
import Data.List (sort)
import Control.Monad (msum)
import Data.Maybe (catMaybes)

import LoggyCore

inputTime :: String
inputTime = "01:15:00"

inputTimeWith :: String -> String
inputTimeWith = (inputTime ++) 

inputDiffTime :: Maybe DiffTime 
inputDiffTime = Just 4500

dateFormat :: DateFormat 
dateFormat = "%H:%M:%S"

testMergeLogs :: DateFormat -> [LogFileLines] -> MergeResult
testMergeLogs dformat lfiles = mergeLogLines dFormatFilePairs
    where
        dFormatFilePairs = zip dFormatPerFile lfiles
        dFormatPerFile = replicate numFiles dformat
        numFiles = length lfiles

loggycore :: Spec
loggycore = describe "LoggyCoreTest" $ do
    extractTimestampTest
    mergeLogLinesTest

extractTimestampTest :: Spec
extractTimestampTest = describe "LoggyCoreTest: extractTimestampTest" $ do
    it "simpleTimestamp" $ extractTsHelper inputTime `shouldBe` inputDiffTime
    it "simpleTimestampWithSpaces" $ extractTsHelper (inputTimeWith " ") `shouldBe` inputDiffTime
    it "simpleTimestampWithExtraChars" $ extractTsHelper (inputTimeWith " random extra chars") `shouldBe` inputDiffTime   
    it "simpleTimestampWithRepeatedTimestamp" $ extractTsHelper (inputTimeWith $ " " ++ inputTime) `shouldBe` inputDiffTime
    it "invalidLogLine" $ extractTsHelper "invalid input" `shouldBe` Nothing
    it "extractTimestampPropertyTest" $ hedgehog $ do
        logText <- forAll $ Gen.string (Range.linear 0 1000) Gen.ascii 
        unixTimeSeconds <- forAll $ Gen.int64 (Range.linear 0 1000)
        let utcTime = systemToUTCTime $ MkSystemTime unixTimeSeconds 0
        let utcTimeStr = formatTime defaultTimeLocale  dateFormat utcTime
        let logLine = utcTimeStr ++ " " ++ logText
        Just utcTime === extractTimestamp dateFormat logLine
    where
        extractTsHelper tsLog = utctDayTime <$> extractTimestamp dateFormat tsLog 

mergeLogLinesTest :: Spec
mergeLogLinesTest = describe "LoggyCoreTest: mergeLogLines" $ do
    it "bothEmpty" $ testMergeLogs dateFormat [[],[]] `shouldBe` MkMergeResult [] NoWarning
    it "singleEmpty" $ testMergeLogs dateFormat
                            [[inputTimeWith " sample log line"],[]]
                `shouldBe` MkMergeResult [inputTimeWith " sample log line"] NoWarning
    it "sameTimeLogs" $ testMergeLogs dateFormat 
                            [[inputTimeWith " sample log line 1"],[inputTimeWith " sample log line 2"]] 
                `shouldBe` MkMergeResult [inputTimeWith " sample log line 1", inputTimeWith " sample log line 2"] NoWarning
    it "mergeSingleLineLogs" $ testMergeLogs dateFormat 
                            [[inputTimeWith " from file 1"],["01:18:00 from file 2"]]
                `shouldBe` MkMergeResult [inputTimeWith " from file 1", "01:18:00 from file 2"] NoWarning
    it "mergeMultiLineLogs" $ testMergeLogs dateFormat 
                            [[inputTimeWith " from file 1", "01:23:00 from file 1"],
                             ["01:18:00 from file 2", "01:25:55 from file 2"]]
                `shouldBe` MkMergeResult [inputTimeWith " from file 1", "01:18:00 from file 2", 
                            "01:23:00 from file 1", "01:25:55 from file 2"] NoWarning
    it "mergeSingleLineLogsDifferentTimeFormats" $ 
            mergeLogLines [(dateFormat, [inputTimeWith " from file 1", "01:23:00 from file 1"]),
                            (dateFormat ++ "XYZ", ["01:18:00XYZ from file 2"])]
        `shouldBe` MkMergeResult [inputTimeWith " from file 1", "01:18:00XYZ from file 2", 
                                  "01:23:00 from file 1"] NoWarning
    it "invalidLogLine" $ testMergeLogs dateFormat [[" invalid log line"]]
                `shouldBe` MkMergeResult [] InvalidLogLine
    it "invalidLogLines" $ testMergeLogs dateFormat [[" invalid log line", inputTimeWith " from file 1"]]
                    `shouldBe` MkMergeResult [inputTimeWith " from file 1"] InvalidLogLine
    it "mergeLogLinesPropertyTest" $ hedgehog $ do
        numFiles <- forAll $ Gen.int (Range.linear 1 10)
        fileDateFormats <- forAll $ genDateFormats numFiles
        logFiles <- forAll $ mapM genFile fileDateFormats
        let MkMergeResult mergedLines mergeStatus = mergeLogLines (zip fileDateFormats logFiles)
        length mergedLines === sum (map length logFiles)
        mergeStatus === NoWarning
        let maybeTsPerLogLine = map (dateFormats `extractTimestamp'`) mergedLines 
        let tsPerLogLine = catMaybes maybeTsPerLogLine
        length tsPerLogLine === length mergedLines
        isSorted tsPerLogLine === True
        where
            dateFormats :: [DateFormat]
            dateFormats = ["%H:%M:%S", "%H-%M-%S", "%H/%M/%S", "%H.%M.%S"]
            genDateFormats ::Int -> Gen [DateFormat]
            genDateFormats numFiles = Gen.list (Range.singleton numFiles) (Gen.element dateFormats)
            genFile :: DateFormat -> Gen LogFileLines
            genFile dFormat = do
                numLogLines <- Gen.int (Range.linear 0 100)
                logTxtLines <- Gen.list (Range.singleton numLogLines) (Gen.string (Range.linear 0 100) Gen.ascii)
                logTsLst <- genSortedDates numLogLines
                let logTsFormattedLst = map (formatTime defaultTimeLocale dFormat) logTsLst
                let logLines = zipWith (\ts logLine -> ts ++ " " ++ logLine) logTsFormattedLst logTxtLines
                return logLines
            genSortedDates :: Int -> Gen [UTCTime]
            genSortedDates numLines = do
                unixTsLst <- Gen.list (Range.singleton numLines) (Gen.int64 (Range.linear 0 1000))
                let sortedUnixTsLst = sort unixTsLst
                let utcTimeLst = map (\ts -> systemToUTCTime $ MkSystemTime ts 0) sortedUnixTsLst
                return utcTimeLst    
            extractTimestamp' :: [DateFormat] -> LogLine -> Maybe UTCTime
             -- Brute force try all formats till one matches
            extractTimestamp' dFs logLine = tsForLogLine
                where
                    tsForLogLine = msum tsPerDateFormat
                    tsPerDateFormat = map (`extractTimestamp` logLine) dFs
            isSorted :: (Ord a) => [a] -> Bool
            isSorted []       = True
            isSorted [_]      = True
            isSorted (x:y:xs) = x <= y && isSorted (y:xs)
        
