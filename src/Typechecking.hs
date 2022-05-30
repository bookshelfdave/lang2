{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Typechecking
  ( typecheck1
  ) where

import AST

typecheck1 :: IO ()
typecheck1 = do
  putStrLn "typecheck1"
-- data Expr
--   = Var String
--   | Int Int
--   | Bool Bool
--   | Negation Expr
--   | BoolNegation Expr
--   | Sum Expr Expr
--   | Subtr Expr Expr
--   | Product Expr Expr
--   | Division Expr Expr
--   | FnCall String [Expr]
--   | And Expr Expr
--   | Or Expr Expr
--   | Equality Expr Expr
--   | NotEquality Expr Expr
--   deriving (Eq, Ord, Show)
