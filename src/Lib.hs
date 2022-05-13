{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc, parseMod
    ) where

-- import Text.Megaparsec hiding(State, SourcePos)
-- import Text.Megaparsec.Char
-- import Text.Megaparsec.Error
-- import Data.Text (Text)
-- import Data.Void
-- import Control.Monad.Identity


import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L -- (1)
import Data.Text (Text)
import Data.Void
-- import Data.Text hiding(singleton)
-- import Control.Monad.Combinators
-- import Control.Monad.State
-- import Data.Sequence

type Parser = Parsec Void Text

type ParamName = String
type TypeName = String

data FormalParam = FormalParam ParamName TypeName
    deriving (Show)

data AST =
    ModuleDef String [AST] |
    FnDef String [FormalParam]
    deriving (Show)

{-
module Main
fn test()
fn foo()
-}
ws :: Parser String
ws = many (try (char ' ')
                <|> try (char '\n')
                <|> try (char '\r')
                <|> char '\t')

sc :: Parser ()
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

-- stringLiteral :: Parser String
-- stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

-- manyTill :: Alternative m => m a -> m end -> m [a]
-- manyTill p end = go
--   where
--     go = ([] <$ end) <|> ((:) <$> p <*> go)

name :: Parser String
name = do
        first <- lowerChar
        rest <- many lowerChar
        pure (first : rest)

typeId :: Parser String
typeId = do
        first <- upperChar
        rest <- many lowerChar
        pure (first : rest)


keywords = ["module"]

formalParam :: Parser FormalParam
formalParam = do
            varName <- name
            _ <- char ':'
            typeName <- typeId
            let p = FormalParam varName typeName
            pure p

fnDef :: Parser AST
fnDef = do
            string "fn"; ws
            fnName <- name; ws
            char '('; ws
            params <- formalParam `sepBy` char ','
            char ')'; ws
            return $ FnDef fnName params


moduleDef :: Parser AST
moduleDef = do
                string "module"; ws
                modName <- some lowerChar; ws
                char ';'; ws
                functions <- many fnDef; ws
                eof
                return $ ModuleDef modName functions

--parseMod :: Text -> Either String AST
parseMod input = parse moduleDef "(unknown)" input

someFunc :: IO ()
someFunc = do
            putStrLn "testingParseMod"
            let result = parseMod "module xyz; fn foo(x:String,y:String)"
            putStrLn $ show result
            return ()