{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe
import Lib
import System.Exit
import System.Process

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.ICU as RE

main :: IO ()
main = do
  projectType <- getProjectType
  (_, output, errorOutput) <-
    case projectType of
      Just Rails -> do
        T.putStrLn "Running RSpec. This may take a minute....."
        readProcessWithExitCode "bin/rake" [ "spec" ] ""
      Just Phoenix -> do
        T.putStrLn "Running ExUnit. This may take a minute....."
        readProcessWithExitCode "mix" [ "test" ] ""
      _ -> do
        T.putStrLn "\nThe project type for the unit tests was not recognized."
        T.putStrLn "At this time, only Ruby on Rails and Elixir projects are recognized.\n"
        return (ExitFailure 1, "", "")
        
  let outputLines = T.lines $ T.pack output
      outputErrorLines = T.lines $ T.pack errorOutput
  mapM_ T.putStrLn (outputLines ++ outputErrorLines)

  if any containsZeroFailures outputLines
    then T.putStrLn "\nAll tests passed! Committing...\n" >> exitSuccess
    else do
      T.putStrLn "\n1 or more test failures detected. Commit cancelled."
      T.putStrLn "Please fix the failing tests and try your commit again.\n"
      exitFailure
  where containsZeroFailures line =
          let successRE = RE.regex [ RE.CaseInsensitive ] "0\\s+failures"
          in isJust $ RE.find successRE line >>= RE.group 0
