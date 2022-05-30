{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Lower1
  ( lower1) where

import AST

lower1 :: IO ()
lower1 = do
    putStrLn "Lowering stuff"
