module Main where

import qualified Prix.Cli as Cli
import System.Exit (exitWith)


main :: IO ()
main = Cli.cli >>= exitWith
