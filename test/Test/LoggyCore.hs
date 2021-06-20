module Test.LoggyCore (loggycore) where

import Test.Hspec (shouldBe, Spec, describe, it)
import Data.Time

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
extractTimestampTest = describe "LoggyCoreTest: extractTimestamp" $ do
    it "simpleTimestamp" $ extractTsHelper inputTime `shouldBe` inputDiffTime
    it "simpleTimestampWithSpaces" $ extractTsHelper (inputTimeWith " ") `shouldBe` inputDiffTime
    it "simpleTimestampWithExtraChars" $ extractTsHelper (inputTimeWith " random extra chars") `shouldBe` inputDiffTime   
    it "simpleTimestampWithRepeatedTimestamp" $ extractTsHelper (inputTimeWith $ " " ++ inputTime) `shouldBe` inputDiffTime
    it "invalidLogLine" $ extractTsHelper "invalid input" `shouldBe` Nothing
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