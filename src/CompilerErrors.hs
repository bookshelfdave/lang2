{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module CompilerErrors
    ( 
    CustomParseErrors(..)
    ) where

import qualified Data.Text as T
import Text.Megaparsec.Error

data CustomParseErrors = 
    ReservedKeyword T.Text | 
    UndefinedType T.Text
  deriving (Eq, Show, Ord)

instance ShowErrorComponent CustomParseErrors where
  showErrorComponent (ReservedKeyword txt) = T.unpack txt ++ " is a reserved keyword"
  showErrorComponent (UndefinedType txt) = "The type " ++ T.unpack txt ++ " is not defined"
