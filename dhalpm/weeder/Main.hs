{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Prelude

import Language.Haskell.TH.Syntax qualified as TH
import System.FilePath ((</>))
import Weeder.Main qualified

main :: IO ()
main = Weeder.Main.main

$( do
    root <- TH.getPackageRoot
    TH.addDependentFile (root </> "weeder.toml")
    pure []
 )
