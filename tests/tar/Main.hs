{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Exit (exitFailure, exitSuccess)
import System.IO.Error

import Control.Biegunka hiding (check)
import Control.Biegunka.Source.Tar
import Control.Lens
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
  -- Unpack test tar archive and check layout is correct
  helper "/tmp" b l []
  -- Delete everything
  helper "/tmp" b' l'
    [ DE doesNotExistErrorType "tar/test"
    , FE doesNotExistErrorType ".biegunka/profiles/tar.profile"
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
      directory "profiles" $
        file_ "tar.profile"

  b' = return ()
  l' = do
    directory "tar" $
      directory_ "test"
    directory ".biegunka" $
      directory "profiles" $
        file_ "tar.profile"


-- | Test advanced .tar handling
-- Assumes ~/tar directory does exist
-- Assumes ~/sandbox/tar directory does exist
advanced :: Test
advanced = TestCase $ do
  -- Unpack test tar archive, copy some things
  -- and check layout is correct
  helper "/tmp" b l []
  -- Delete everything
  helper "/tmp" b' l'
    [ DE doesNotExistErrorType "tar/test"
    , RF doesNotExistErrorType "sandbox/tar/s" "test1\n"
    , RF doesNotExistErrorType "sandbox/tar/t" "test2\n"
    , FE doesNotExistErrorType ".biegunka/profiles/tar.profile"
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
      directory "profiles" $
        file_ "tar.profile"

  b' = return ()
  l' = do
    directory "tar" $
      directory_ "test"
    directory "sandbox" $
      directory "tar" $ do
        file "s" "test1\n"
        file "t" "test2\n"
    directory ".biegunka" $
      directory "profiles" $
        file_ "tar.profile"


-- | Test compressed .tar handling
-- Assumes ~/tar directory does exist
compressed :: Test
compressed = TestCase $ do
  -- Uncompress and unpack gzipped tar archive and check layout is correct
  helper "/tmp" bgz l []
  -- Delete everything
  helper "/tmp" b' l'
    [ DE doesNotExistErrorType "tar/test"
    , FE doesNotExistErrorType ".biegunka/profiles/tar.profile"
    ]
  -- Uncompress and unpack bzipped tar archive and check layout is correct
  helper "/tmp" bbz2 l []
  -- Delete everything
  helper "/tmp" b' l'
    [ DE doesNotExistErrorType "tar/test"
    , FE doesNotExistErrorType ".biegunka/profiles/tar.profile"
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
      directory "profiles" $
        file_ "tar.profile"

  b' = return ()
  l' = do
    directory "tar" $
      directory_ "test"
    directory ".biegunka" $
      directory "profiles" $
        file_ "tar.profile"


helper :: FilePath -> Script Sources () -> Layout -> [LayoutException] -> IO ()
helper d s l xs = do
  biegunka (set root "/tmp" . set appData "/tmp/.biegunka") (run id) (profile "tar" s)
  xs' <- check l d
  assertEqual "tar-tests" xs xs'
