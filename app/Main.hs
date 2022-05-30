module Main where

import Codegen
import Lower1
import Parser
import Typechecking

main :: IO ()
main = do
  putStrLn "Lang2 0.1.0"
