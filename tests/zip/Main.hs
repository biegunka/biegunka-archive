{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (zip)
import System.Exit (exitFailure, exitSuccess)
import System.IO.Error

import Biegunka
import Biegunka.Source.Zip
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
    ]


-- | Test basic .zip handling
-- Assumes ~/zip directory does exist
basic :: Test
basic = TestCase $ do
  w <- getHomeDirectory

  -- Unpack test zip archive and check layout is correct
  helper w b l []
  -- Delete everything
  helper w b' l' [DE doesNotExistErrorType "zip/test", FE doesNotExistErrorType ".biegunka/biegunka-zip-test"]
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
      file_ "biegunka-zip-test"

  b' = return ()
  l' = do
    directory "zip" $
      directory_ "test"
    directory ".biegunka" $
      file_ "biegunka-zip-test"


-- | Test advanced .zip handling
-- Assumes ~/zip directory does exist
-- Assumes ~/sandbox/zip directory does exist
advanced :: Test
advanced = TestCase $ do
  w <- getHomeDirectory
  -- Unpack test zip archive, copy some things
  -- and check layout is correct
  helper w b l []
  -- Delete everything
  helper w b' l'
    [ DE doesNotExistErrorType "zip/test"
    , RF doesNotExistErrorType "sandbox/zip/s" "test1\n"
    , RF doesNotExistErrorType "sandbox/zip/t" "test2\n"
    , FE doesNotExistErrorType ".biegunka/biegunka-zip-test"
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
      file_ "biegunka-zip-test"

  b' = return ()
  l' = do
    directory "zip" $
      directory_ "test"
    directory "sandbox" $
      directory "zip" $ do
        file "s" "test1\n"
        file "t" "test2\n"
    directory ".biegunka" $
      file_ "biegunka-zip-test"


helper :: FilePath -> Script Sources () -> Layout -> [LayoutException] -> IO ()
helper d s l xs = do
  biegunka (set root "~") (profile "biegunka-zip-test" s) (execute id)
  xs' <- check l d
  assertEqual "zip-tests" xs xs'
