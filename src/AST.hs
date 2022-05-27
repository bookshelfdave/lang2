{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module AST
  ( 
    Decl(..)
  , Expr(..)
  , FnDef(..)
  , FormalParam(..)
  , ParamName
  , Stmt(..)
  , TypeName
  ) where

data Decl
  = VarDecl FormalParam Expr
  | FnDecl FnDef
  deriving (Show)


data Stmt
  = Block [Stmt]
  | Decl Decl
  | Expr Expr
  | For
  | IfElse
  | Return Expr
  deriving (Show)

data Expr
  = Var String
  | Int Int
  | Negation Expr
  | Sum Expr Expr
  | Subtr Expr Expr
  | Product Expr Expr
  | Division Expr Expr
  | FnCall String [Expr]
  deriving (Eq, Ord, Show)

type ParamName = String

type TypeName = String

data FormalParam =
  FormalParam ParamName TypeName
  deriving (Eq)

instance Show FormalParam where
  show (FormalParam n t) = n ++ ":" ++ t

data FnDef =
  FnDef
    { fName :: String
    , fReturnType :: TypeName
    , fParams :: [FormalParam]
    , fBody :: [Stmt]
    }

instance Show FnDef where
  show f = "<fn " ++ fName f ++ "(" ++ showParams ++ ") -> " ++ fReturnType f ++ ">"
    where
      showParams = show (fParams f)
