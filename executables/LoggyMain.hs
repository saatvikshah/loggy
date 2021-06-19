module Main where

import Options.Applicative

data LoggyArgs = LoggyArgs
  { fstFile      :: String
  , sndFile      :: String
  , dateFormat   :: String}

parseLoggyArgs :: Parser LoggyArgs
parseLoggyArgs = LoggyArgs
      <$> strOption
          ( long "text1"
         <> metavar "TEXT_1"
         <> help "Text to merge." )
      <*> strOption
          ( long "text2"
         <> metavar "TEXT_2"
         <> help "Text to merge." )
      <*> strOption
          ( long "format"
         <> metavar "FORMAT"
         <> value "%Y:%M:%D-%H%m%s"
         <> help "Date format to be parsed." )  

main :: IO ()
main = runMain =<< execParser opts
  where
    opts = info (helper <*> parseLoggyArgs)
      ( fullDesc
     <> progDesc "This is the text from progDesc"
     <> header "This is the text from header" )

runMain :: LoggyArgs -> IO ()
runMain (LoggyArgs txt1 txt2 dfmt) = print [txt1,txt2,dfmt]

