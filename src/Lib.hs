{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Lib
    ( 
    formalParam,
    name, 
    parseExpr,
    parseMod, 
    someFunc, 
    typeId, 
    undefinedTypeError,
    CustomParseErrors(..), 
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
import Control.Monad.Combinators.Expr

import AST
import Errors
import Types


type Parser = Parsec CustomParseErrors Text


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

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary  oname f = InfixL  (f <$ st oname)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix  oname f = Prefix  (f <$ st oname)
postfix oname f = Postfix (f <$ st oname)

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "-" Negation
    , prefix "+" id
    ]
  , [ binary "*" Product
    , binary "/" Division
    ]
  , [ binary "+" Sum
    , binary "-" Subtr
    ]
  ]

pVariable :: Parser Expr
pVariable = Var <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pInteger :: Parser Expr
pInteger = Int <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (st "(") (st ")")

pTerm :: Parser Expr
pTerm = choice
  [ parens pExpr
  , pVariable
  , pInteger
  ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable



--parseMod :: Text -> Either String AST
parseMod input = parse moduleDef "(unknown)" input

parseExpr input = parse pExpr "(unknown)" input


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