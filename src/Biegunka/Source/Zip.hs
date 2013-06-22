{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK prune #-}
-- | Biegunka.Source.Zip - functions to work with .zip archives as sources
module Biegunka.Source.Zip
  ( -- * Source layer
    zip, zip_
  ) where

import Prelude hiding (zip)

import Codec.Archive.Zip (toArchive, extractFilesFromArchive)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, setCurrentDirectory)

import Biegunka.Language
import Biegunka.Script (Script, sourced)
import Biegunka.Source.Archive (update)


-- | Download and extract zip archive from the given url to specified path.
-- Also executes attached script
--
-- > zip "https://example.com/archive.zip" "git/archive" $ do
-- >   registerAt "some/not/so/long/path"
-- >   link "important.file" ".config"
--
--  * download and extract archive from https:\/\/example.com\/archive.zip to ${HOME}\/git\/archive
--
--  * link ${HOME}\/git\/archive to ${HOME}\/some\/not\/so\/long\/path
--
--  * link ${HOME}\/git\/archive\/important.file to ${HOME}\/.config
zip ∷ String → FilePath → Script Actions () → Script Sources ()
zip url path script = sourced "zip" url path script (updateZip url)


-- | Download and extract zip archive from the given url to specified path.
--
-- > zip_ "https://example.com/archive.zip" "git/archive"
--
--  * download and extract archive from https:\/\/example.com\/archive.zip to ${HOME}\/git\/archive
zip_ ∷ String → FilePath → Script Sources ()
zip_ url path = zip url path $ return ()


updateZip ∷ String → FilePath → IO ()
updateZip url path = update url path (with path . extractFilesFromArchive [] . toArchive)


with ∷ FilePath → IO a → IO ()
with path action = do
  saved ← getCurrentDirectory
  createDirectoryIfMissing True path
  setCurrentDirectory path
  action
  setCurrentDirectory saved
