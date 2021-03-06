{-# LANGUAGE DataKinds #-}
{-# OPTIONS_HADDOCK prune #-}
-- | Biegunka.Source.Tar - functions to work with [.tar, .tar.gz, .tar.bz2] archives as sources
module Control.Biegunka.Source.Tar
  ( -- * Source layer
    tar, tar_
  ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip (decompress)
import qualified Codec.Compression.BZip as BZip (decompress)
import           Data.ByteString.Lazy (ByteString)
import           System.FilePath (takeExtension)

import Control.Biegunka.Language
import Control.Biegunka.Script (Script, sourced)
import Control.Biegunka.Source.Archive (update)


-- | Download and extract tar archive (possibly with compression)
-- from the given url to specified path. Also executes attached script
--
-- > tar "https://example.com/archive.tar.gz" "tar/archive" $ do
-- >   register "some/not/so/long/path"
-- >   link "important.file" ".config"
--
--  * download and extract archive from https:\/\/example.com\/archive.tar.gz to ${HOME}\/tar\/archive
--
--  * link ${HOME}\/tar\/archive to ${HOME}\/some\/not\/so\/long\/path
--
--  * link ${HOME}\/tar\/archive\/important.file to ${HOME}\/.config
tar :: String -> FilePath -> Script Actions () -> Script Sources ()
tar url path script = sourced "tar" url path script (updateTar url)


-- | Download and extract tar archive (possibly with compression)
-- from the given url to specified path.
--
-- > tar_ "https://example.com/archive.tar.gz" "tar/archive"
--
--  * download and extract archive from https:\/\/example.com\/archive.tar.gz to ${HOME}\/tar\/archive
tar_ :: String -> FilePath -> Script Sources ()
tar_ url path = tar url path $ return ()


updateTar :: String -> FilePath -> IO ()
updateTar url path = update url path (Tar.unpack path . Tar.read . decompress url)


decompress :: String -> ByteString -> ByteString
decompress url = case takeExtension url of
  ".gz" -> GZip.decompress
  ".bz2" -> BZip.decompress
  _ -> id
