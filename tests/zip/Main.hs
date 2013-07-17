{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (zip)
import System.Exit (exitFailure, exitSuccess)
import System.IO.Error

import Control.Biegunka hiding (check)
import Control.Biegunka.Source.Zip
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
    ]


-- | Test basic .zip handling
-- Assumes ~/zip directory does exist
basic :: Test
basic = TestCase $ do
  -- Unpack test zip archive and check layout is correct
  helper "/tmp" b l []
  -- Delete everything
  helper "/tmp" b' l'
    [ DE doesNotExistErrorType "zip/test"
    , FE doesNotExistErrorType ".biegunka/profiles/zip.profile"
    ]
 where
  b =
    zip_ "http://budueba.com/biegunka-zip-test.zip" "zip/test"
  l = do
    directory "zip" $
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
        file_ "zip.profile"

  b' = return ()
  l' = do
    directory "zip" $
      directory_ "test"
    directory ".biegunka" $
      directory "profiles" $
        file_ "zip.profile"


-- | Test advanced .zip handling
-- Assumes ~/zip directory does exist
-- Assumes ~/sandbox/zip directory does exist
advanced :: Test
advanced = TestCase $ do
  -- Unpack test zip archive, copy some things
  -- and check layout is correct
  helper "/tmp" b l []
  -- Delete everything
  helper "/tmp" b' l'
    [ DE doesNotExistErrorType "zip/test"
    , RF doesNotExistErrorType "sandbox/zip/s" "test1\n"
    , RF doesNotExistErrorType "sandbox/zip/t" "test2\n"
    , FE doesNotExistErrorType ".biegunka/profiles/zip.profile"
    ]

 where
  b =
    zip "http://budueba.com/biegunka-zip-test.zip" "zip/test" $ do
      copy "x/y/s" "sandbox/zip/s"
      copy "x/y/t" "sandbox/zip/t"
  l = do
    directory "zip" $
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
      directory "zip" $ do
        file "s" "test1\n"
        file "t" "test2\n"
    directory ".biegunka" $
      directory "profiles" $
        file_ "zip.profile"

  b' = return ()
  l' = do
    directory "zip" $
      directory_ "test"
    directory "sandbox" $
      directory "zip" $ do
        file "s" "test1\n"
        file "t" "test2\n"
    directory ".biegunka" $
      directory "profiles" $
        file_ "zip.profile"


helper :: FilePath -> Script Sources () -> Layout -> [LayoutException] -> IO ()
helper d s l xs = do
  biegunka (set root "/tmp" . set appData "/tmp/.biegunka") (run id) (profile "zip" s)
  xs' <- check l d
  assertEqual "zip-tests" xs xs'
