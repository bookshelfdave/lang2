{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Lib
  ( fnDef
  , formalParam
  , name
  , parseExpr
  , parseFnDef
  , someFunc
  , typeId
  , undefinedTypeError
  , varDecl
  , CustomParseErrors(..)
  , Expr(..)
  , FormalParam(..)
  , ParamName
  , TypeName
  , Parser
  ) where

import qualified Control.Applicative as A
import Control.Monad.Combinators.Expr
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L -- (1)

import AST
import CompilerErrors
import Types

type Parser = Parsec CustomParseErrors Text

-- Errors
reservedError :: Text -> Parser a
reservedError = customFailure . ReservedKeyword

undefinedTypeError :: Text -> Parser a
undefinedTypeError = customFailure . UndefinedType

-- char utils
ws :: Parser ()
ws =
  L.space
    space1 -- (2)
    (L.skipLineComment "//") -- (3)
    (L.skipBlockComment "/*" "*/") -- (4)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

-- Parse a single character, ignore trailing whitespace
ch :: Char -> Parser Char
ch = lexeme . char

-- Parse a string, ignore trailing whitespace
st :: Text -> Parser Text
st = lexeme . string

semi :: Parser Char
semi = ch ';'

-- parse a character literal: ex 'x'
charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

-- parse a string literal: ex "foo"
stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

-- things that aren't types that have a name
name :: Parser String
name = do
  first <- lowerChar
  rest <- many (alphaNumChar <|> char '_')
  ws
  pure (first : rest)

-- all types start with an uppercase char
typeId :: Parser String
typeId = do
  first <- upperChar
  rest <- many lowerChar
  ws
  pure (first : rest)

-- a name:type pair
-- foo:Type
formalParam :: Parser FormalParam
formalParam = do
  varName <- name
  _ <- ch ':'
  typeName <- typeId
  ws
  if not (typeInScope typeName "")
    then undefinedTypeError $ T.pack typeName
    else return $ FormalParam varName typeName

-- if fnName == "butt" then reservedError $ T.pack fnName
-- else return $ FnDef fnName params
-- moduleDef :: Parser ()
-- moduleDef = do
--                 st "module";
--                 modName <- lexeme $ (some lowerChar <?> "module name")
--                 ch ';'
--                 functions <- lexeme $ many fnDef
--                 eof
--                 return () -- $ ModuleDef modName functions
-- expressions
binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary oname f = InfixL (f <$ st oname)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix oname f = Prefix (f <$ st oname)

postfix oname f = Postfix (f <$ st oname)

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [prefix "-" Negation, prefix "+" id]
  , [binary "*" Product, binary "/" Division]
  , [binary "+" Sum, binary "-" Subtr]
  ]

pVariable :: Parser Expr
pVariable =
  Var <$> lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pInteger :: Parser Expr
pInteger = Int <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (st "(") (st ")")

braces :: Parser a -> Parser a
braces = between (st "{") (st "}")

pFnCall :: Parser Expr
pFnCall = do
  -- TODO: name vs pVariable
  fnname <- name
  params <- parens $ pExpr `sepBy` ch ','
  return $ FnCall fnname params

pTerm :: Parser Expr
pTerm = choice [parens pExpr, try pFnCall, pVariable, pInteger]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

--- variable declarations
varDecl :: Parser Decl
varDecl = do
  st "var"
  param <- formalParam
  ch '='
  expr <- pExpr
  semi
  return $ VarDecl param expr

--- functions
fnReturnType :: Parser String
fnReturnType = do
  ch '-'
  ch '>'
  typeId

fnFormalParams :: Parser [FormalParam]
fnFormalParams = do
  formalParam `sepBy` ch ','


-- data Stmt
--   = Block 
--   | Decl Decl
--   | Expr Expr
--   | For
--   | IfElse
--   | Return

fnReturn :: Parser Stmt
fnReturn = do
  st "return"
  Return <$> pExpr

fnBodyStmt :: Parser Stmt
fnBodyStmt = do
  stmt <- choice [Decl <$> varDecl, Expr <$> pExpr, fnReturn]
  semi
  return stmt

fnBody :: Parser [Stmt]
fnBody = do
  some fnBodyStmt

fnDef :: Parser FnDef
fnDef = do
  st "fn" <?> "function definition"
  fnName <- name; ws
  params <- parens fnFormalParams
  returnType <- optional fnReturnType
  body <- braces fnBody
  return
    FnDef
      { fName = fnName
      , fReturnType = fromMaybe "Unit" returnType
      , fParams = params
      , fBody = body
      }

--parseMod :: Text -> Either String AST
--parseMod input = parse moduleDef "(unknown)" input
parseExpr input = parse pExpr "(unknown)" input
parseFnDef input = parse fnDef "(unknown)" input

someFunc :: IO ()
someFunc = do
  putStrLn "Lang2 compiler 0.1.0"
            -- putStrLn "testingParseMod"
            -- case parseMod "module xyz; fn butt(x:String,y:String)" of
            --     Left err -> do
            --             putStrLn $ errorBundlePretty err
            --     Right result -> print result
            -- return ()
