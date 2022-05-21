{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Types
    ( 
    builtInTypes,
    typeInScope
    ) where

builtInTypes = [
    "String",
    "Bool",
    "Int",
    "UInt",
    "Int64",
    "UInt64",
    "Byte",
    "Unit"]



typeInScope :: String -> String -> Bool
typeInScope typeId _scope = typeId `elem` builtInTypes


