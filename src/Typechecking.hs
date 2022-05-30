{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Typechecking
  ( typecheck1) where

import AST

typecheck1 :: IO ()
typecheck1 = do
    putStrLn "Typechecking stuff"
