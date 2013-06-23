{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Exit (exitFailure, exitSuccess)
import System.IO.Error

import Biegunka
import Biegunka.Source.Tar
import Control.Lens
import System.Directory (getHomeDirectory)
import System.Directory.Layout
import Test.HUnit


main :: IO ()
main = do
  z <- runTestTT tests
  if errors z + failures z > 0
    then exitFailure
    else exitSuccess
 where
  tests = TestList
    [ TestLabel "basic" basic
    , TestLabel "advanced" advanced
    , TestLabel "compressed" compressed
    ]


-- | Test basic .tar handling
-- Assumes ~/tar directory does exist
basic :: Test
basic = TestCase $ do
  w <- getHomeDirectory

  -- Unpack test tar archive and check layout is correct
  helper w b l []
  -- Delete everything
  helper w b' l'
    [ DE doesNotExistErrorType "tar/test"
    , FE doesNotExistErrorType ".biegunka/biegunka-tar-test"
    ]
 where
  b =
    tar_ "http://budueba.com/biegunka-tar-test.tar" "tar/test"
  l = do
    directory "tar" $
      directory "test" $ do
        directory "x" $ do
          directory "y" $ do
            file "s" "test1\n"
            file "t" "test2\n"
          directory "z" $
            file_ "v"
        directory "w" $
          file_ "u"
        file_ "q"
    directory ".biegunka" $
      file_ "biegunka-tar-test"

  b' = return ()
  l' = do
    directory "tar" $
      directory_ "test"
    directory ".biegunka" $
      file_ "biegunka-tar-test"


-- | Test advanced .tar handling
-- Assumes ~/tar directory does exist
-- Assumes ~/sandbox/tar directory does exist
advanced :: Test
advanced = TestCase $ do
  w <- getHomeDirectory
  -- Unpack test tar archive, copy some things
  -- and check layout is correct
  helper w b l []
  -- Delete everything
  helper w b' l'
    [ DE doesNotExistErrorType "tar/test"
    , RF doesNotExistErrorType "sandbox/tar/s" "test1\n"
    , RF doesNotExistErrorType "sandbox/tar/t" "test2\n"
    , FE doesNotExistErrorType ".biegunka/biegunka-tar-test"
    ]
 where
  b =
    tar "http://budueba.com/biegunka-tar-test.tar" "tar/test" $ do
      copy "x/y/s" "sandbox/tar/s"
      copy "x/y/t" "sandbox/tar/t"
  l = do
    directory "tar" $
      directory "test" $ do
        directory "x" $ do
          directory "y" $ do
            file "s" "test1\n"
            file "t" "test2\n"
          directory "z" $
            file_ "v"
        directory "w" $
          file_ "u"
        file_ "q"
    directory "sandbox" $
      directory "tar" $ do
        file "s" "test1\n"
        file "t" "test2\n"
    directory ".biegunka" $
      file_ "biegunka-tar-test"

  b' = return ()
  l' = do
    directory "tar" $
      directory_ "test"
    directory "sandbox" $
      directory "tar" $ do
        file "s" "test1\n"
        file "t" "test2\n"
    directory ".biegunka" $
      file_ "biegunka-tar-test"


-- | Test compressed .tar handling
-- Assumes ~/tar directory does exist
compressed :: Test
compressed = TestCase $ do
  w <- getHomeDirectory

  -- Uncompress and unpack gzipped tar archive and check layout is correct
  helper w bgz l []
  -- Delete everything
  helper w b' l'
    [ DE doesNotExistErrorType "tar/test"
    , FE doesNotExistErrorType ".biegunka/biegunka-tar-test"
    ]
  -- Uncompress and unpack bzipped tar archive and check layout is correct
  helper w bbz2 l []
  -- Delete everything
  helper w b' l'
    [ DE doesNotExistErrorType "tar/test"
    , FE doesNotExistErrorType ".biegunka/biegunka-tar-test"
    ]
 where
  bgz =
    tar_ "http://budueba.com/biegunka-tar-test.tar.gz" "tar/test"
  bbz2 =
    tar_ "http://budueba.com/biegunka-tar-test.tar.bz2" "tar/test"
  l = do
    directory "tar" $
      directory "test" $ do
        directory "x" $ do
          directory "y" $ do
            file "s" "test1\n"
            file "t" "test2\n"
          directory "z" $
            file_ "v"
        directory "w" $
          file_ "u"
        file_ "q"
    directory ".biegunka" $
      file_ "biegunka-tar-test"

  b' = return ()
  l' = do
    directory "tar" $
      directory_ "test"
    directory ".biegunka" $
      file_ "biegunka-tar-test"


helper :: FilePath -> Script Sources () -> Layout -> [LayoutException] -> IO ()
helper d s l xs = do
  biegunka (set root "~") (execute id) (profile "biegunka-tar-test" s)
  xs' <- check l d
  assertEqual "tar-tests" xs xs'
