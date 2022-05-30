{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Symbols
  ( buildSymbolTable
  ) where

import AST (Module(..))

buildSymbolTable :: Module -> IO ()
buildSymbolTable (Mod name decls) = do
  putStrLn $ "Generating symbol table for module " ++ name
  putStrLn $ " --> " ++ show (length decls)
