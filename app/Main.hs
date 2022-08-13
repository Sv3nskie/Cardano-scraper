module Main where

import Lib
import Dex.Arguments
import Options.Applicative

main :: IO ()
main = do
  args <- execParser mainOpts
  startApp args
