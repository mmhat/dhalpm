{-# LANGUAGE TemplateHaskell #-}

module Dhalpm.Main (main) where

import Dhalpm.Import

import Control.Exception.Uncaught (displayUncaughtException)

import Dhalpm.Internal
import Dhalpm.Options

main :: IO ()
main = displayUncaughtException $ do
    (options, command) <- Options.parse
    pure ()
