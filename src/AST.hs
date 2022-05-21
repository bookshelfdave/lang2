{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module AST
    ( 
    Expr(..),
    FormalParam(..),
    ParamName,
    TypeName
    ) where

data Expr
  = Var String
  | Int Int
  | Negation Expr
  | Sum      Expr Expr
  | Subtr    Expr Expr
  | Product  Expr Expr
  | Division Expr Expr
  deriving (Eq, Ord, Show)


type ParamName = String
type TypeName = String

data FormalParam = FormalParam ParamName TypeName
    deriving (Show, Eq)
