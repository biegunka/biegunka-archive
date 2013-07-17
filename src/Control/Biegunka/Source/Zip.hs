{-# LANGUAGE DataKinds #-}
{-# OPTIONS_HADDOCK prune #-}
-- | Biegunka.Source.Zip - functions to work with .zip archives as sources
module Control.Biegunka.Source.Zip
  ( -- * Source layer
    zip, zip_
  ) where

import Prelude hiding (zip)

import Codec.Archive.Zip (toArchive, extractFilesFromArchive)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, setCurrentDirectory)

import Control.Biegunka.Language
import Control.Biegunka.Script (Script, sourced)
import Control.Biegunka.Source.Archive (update)


-- | Download and extract zip archive from the given url to specified path.
-- Also executes attached script
--
-- > zip "https://example.com/archive.zip" "zip/archive" $ do
-- >   register "some/not/so/long/path"
-- >   link "important.file" ".config"
--
--  * download and extract archive from https:\/\/example.com\/archive.zip to ${HOME}\/zip\/archive
--
--  * link ${HOME}\/zip\/archive to ${HOME}\/some\/not\/so\/long\/path
--
--  * link ${HOME}\/zip\/archive\/important.file to ${HOME}\/.config
zip :: String -> FilePath -> Script Actions () -> Script Sources ()
zip url path script = sourced "zip" url path script (updateZip url)


-- | Download and extract zip archive from the given url to specified path.
--
-- > zip_ "https://example.com/archive.zip" "zip/archive"
--
--  * download and extract archive from https:\/\/example.com\/archive.zip to ${HOME}\/zip\/archive
zip_ :: String -> FilePath -> Script Sources ()
zip_ url path = zip url path $ return ()


updateZip :: String -> FilePath -> IO ()
updateZip url path = update url path (with path . extractFilesFromArchive [] . toArchive)


with :: FilePath -> IO a -> IO ()
with path action = do
  saved <- getCurrentDirectory
  createDirectoryIfMissing True path
  setCurrentDirectory path
  action
  setCurrentDirectory saved
