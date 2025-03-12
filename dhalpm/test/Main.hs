module Main (main) where

import Prelude

import System.Environment (getArgs, getProgName, withArgs)
import Test.Tasty (defaultIngredients, defaultMainWithIngredients)
import Test.Tasty.Ingredients.Rerun (rerunningTests)

import qualified Discover

main :: IO ()
main = do
    name <- getProgName
    args <- getArgs
    let
        defaultArgs = ["--rerun", "--rerun-log-file", ".tasty-rerun-log." <> name]
    tests <- Discover.tests
    withArgs (defaultArgs <> args)
        $ defaultMainWithIngredients [rerunningTests defaultIngredients] tests
