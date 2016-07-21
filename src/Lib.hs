{-# LANGUAGE OverloadedStrings #-}

module Lib (ProjectType(..), getProjectType) where

import Control.Monad
import System.Directory
import System.FilePath

import qualified Data.Set as Set
import qualified Data.Text as T

data ProjectType = Rails | Phoenix

instance Show ProjectType where
  show Rails = "Rails"
  show Phoenix = "Phoenix"


getProjectType :: IO (Maybe ProjectType)
getProjectType = do
  currentDir <- getCurrentDirectory
  files <- (getDirectoryContents currentDir) >>= return . map (currentDir </>)
                                             >>= filterM doesFileExist
                                             >>= return . map (T.toLower . T.pack . takeFileName)
                                             >>= return . Set.fromList
  let filesToLookFor = Set.map T.pack (Set.fromList [ "gemfile", "mix.exs" ])
      filesFound = Set.intersection files filesToLookFor
  if Set.member "gemfile" filesFound
    then return $ Just Rails
    else if Set.member "mix.exs" filesFound
           then return $ Just Phoenix
           else return Nothing
