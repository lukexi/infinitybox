{-# LANGUAGE OverloadedStrings #-}
import Data.List.Split
import Control.Monad
import Control.Arrow

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List

import System.Process
import System.Environment
import System.FilePath
import System.Directory

preprocessorPragma = "{-# OPTIONS_GHC -F -pgmF strip-thease #-}"

main = do
  [origFileName, _, outputFileName] <- getArgs
  let origBaseName = dropExtension origFileName

  -- First, write a new version of the file with the
  -- preprocessor pragma stripped so we can run GHC ourselves
  originalFile <- T.readFile origFileName
  let noPreprocessor = T.replace preprocessorPragma "" originalFile
      noPreprocessFileName = origBaseName <.> "no-pre.hs"
  _ <- T.writeFile noPreprocessFileName noPreprocessor

  -- Next, generate a splices dump file
  rawSystem "stack" 
    (words "exec -- ghc -ddump-splices -ddump-to-file -isrc -outputdir build" ++ [noPreprocessFileName])

  let splicesDumpFileName = origBaseName <.> "no-pre.dump-splices"
  splicesDump <- T.readFile splicesDumpFileName
  
  let textLines = T.lines splicesDump
      -- Split wherever there is a line with no leading whitespace;
      -- this will be something like:
      -- "src/Types.hs:46:1-19: Splicing declarations"
      -- This will give us groups of 
      -- [ [<original splice definition>, "  ======>",
      --   , "splice output line 1", " line 2", ...
      --   ]
      -- , ..
      -- ]
      rawSplices = tail $ splitWhen (\x -> T.head x /= ' ') textLines
      -- Clean things up into [(<original splice def>, <splice output>)]
      cleanSplices = map 
        ( first head     -- Drop the "======>" separator
        . second T.unlines -- Turn list of output lines into a \n-separated string
        . splitAt 2      -- Split into the definition and the output
        . map (T.drop 4))     -- Remove leading whitespace
        rawSplices

  let final = foldl' (\accum (spliceDef, spliceOut) -> 
                T.replace spliceDef spliceOut accum
              ) originalFile cleanSplices 
  T.writeFile outputFileName final
  -- forM_ rawSplices $ \s -> T.putStrLn final >> print ""
  


