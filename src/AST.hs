{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module AST
  ( Decl(..)
  , Expr(..)
  , FnDef(..)
  , Module(..)
  , ParamName
  , Stmt(..)
  , TypeName
  , TypedParam(..)
  ) where

data Module =
  Mod String [Decl]
  deriving (Eq, Show, Ord)

data Decl
  = VarDecl TypedParam Expr
  | FnDecl FnDef
  deriving (Eq, Show, Ord)

data Stmt
  = Block [Stmt]
  | Decl Decl
  | Expr Expr
  | For
  | IfElse Expr Stmt Stmt
  | Return Expr
  deriving (Eq, Show, Ord)

data Expr
  = Var String
  | Int Int
  | Bool Bool
  | Negation Expr
  | BoolNegation Expr
  | Sum Expr Expr
  | Subtr Expr Expr
  | Product Expr Expr
  | Division Expr Expr
  | FnCall String [Expr]
  | And Expr Expr
  | Or Expr Expr
  | Equality Expr Expr
  | NotEquality Expr Expr
  deriving (Eq, Show, Ord)

type ParamName = String

type TypeName = String

data TypedParam =
  TypedParam ParamName TypeName
  deriving (Eq, Show, Ord)

-- instance Show TypedParam where
--   show (TypedParam n t) = n ++ ":" ++ t
data FnDef =
  FnDef
    { fName :: String
    , fReturnType :: TypeName
    , fParams :: [TypedParam]
    , fBody :: Stmt -- a single block statement
    }
  deriving (Eq, Show, Ord) -- instance Show FnDef where
--   show f =
--     "<fn " ++ fName f ++ "(" ++ showParams ++ ") -> " ++ fReturnType f ++ ">"
--     where
--       showParams = show (fParams f)
