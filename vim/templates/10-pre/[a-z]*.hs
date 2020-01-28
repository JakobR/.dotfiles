#!/usr/bin/env stack
{- stack script
  --resolver %HASKELLRESOLVER%
  --package rio
-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wcompat #-}
{-# OPTIONS_GHC -Widentities #-}
{-# OPTIONS_GHC -Wincomplete-record-updates #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wpartial-fields #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}
{-# OPTIONS_GHC -Wnoncanonical-monad-instances #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- rio
import RIO
import qualified RIO.Text as T


main :: IO ()
main = do
  %HERE%putStrLn "Hi"
