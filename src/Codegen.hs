{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Codegen
  ( gen1) where

import AST

gen1 :: IO ()
gen1 = do
    putStrLn "public static void main()"
