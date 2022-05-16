{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Lib
    ( 
    formalParam,
    name, 
    parseMod, 
    someFunc, 
    typeId, 
    undefinedTypeError,
    CustomParseErrors, 
    FormalParam(..),
    ParamName,
    TypeName,
    Parser
    ) where


import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L -- (1)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import qualified Control.Applicative as A

import Types

data CustomParseErrors = 
    ReservedKeyword Text | 
    UndefinedType Text
  deriving (Eq, Show, Ord)

instance ShowErrorComponent CustomParseErrors where
  showErrorComponent (ReservedKeyword txt) = T.unpack txt ++ " is a reserved keyword"
  showErrorComponent (UndefinedType txt) = "The type " ++ T.unpack txt ++ " is not defined"


type Parser = Parsec CustomParseErrors Text

type ParamName = String
type TypeName = String

data FormalParam = FormalParam ParamName TypeName
    deriving (Show, Eq)

data AST =
    ModuleDef String [AST] |
    FnDef String [FormalParam]
    deriving (Show)

-- Errors
reservedError :: Text -> Parser a
reservedError = customFailure . ReservedKeyword

undefinedTypeError :: Text -> Parser a
undefinedTypeError = customFailure . UndefinedType


-- char utils
ws :: Parser ()
ws = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

ch :: Char -> Parser Char
ch = lexeme . char

st :: Text -> Parser Text
st = lexeme . string

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

-- token types
name :: Parser String
name = do
        first <- lowerChar
        rest <- many (alphaNumChar <|> char '_'); ws
        pure (first : rest)

typeId :: Parser String
typeId = do
        first <- upperChar
        rest <- many lowerChar; ws
        pure (first : rest)

formalParam :: Parser FormalParam
formalParam = do
            varName <- name
            _ <- ch ':'
            typeName <- typeId; ws
            if not (typeInScope typeName "") then
                undefinedTypeError $ T.pack typeName
            else 
                return $ FormalParam varName typeName

fnDef :: Parser ()
fnDef = do
            st "fn" <?> "function keyword"
            fnName <- name; ws
            ch '('
            params <- formalParam `sepBy` char ','
            ch ')'
            return ()


-- if fnName == "butt" then reservedError $ T.pack fnName
-- else return $ FnDef fnName params


moduleDef :: Parser ()
moduleDef = do
                st "module";
                modName <- lexeme $ (some lowerChar <?> "module name")
                ch ';'
                functions <- lexeme $ many fnDef
                eof
                return () -- $ ModuleDef modName functions

--parseMod :: Text -> Either String AST
parseMod input = parse moduleDef "(unknown)" input

someFunc :: IO ()
someFunc = do
            putStrLn "testingParseMod"
            case parseMod "module xyz; fn butt(x:String,y:String)" of
                Left err -> do
                        putStrLn $ errorBundlePretty err
                Right result -> print result
            return ()


{-
module Stuff


-}