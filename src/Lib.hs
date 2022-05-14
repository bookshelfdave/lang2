{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Lib
    ( someFunc, parseMod, name, Parser, CustomParseErrors
    ) where


import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L -- (1)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import qualified Control.Applicative as A

data CustomParseErrors = ReservedKeyword Text
  deriving (Eq, Show, Ord)

instance ShowErrorComponent CustomParseErrors where
  showErrorComponent (ReservedKeyword txt) = T.unpack txt ++ " is a reserved keyword"


type Parser = Parsec CustomParseErrors Text

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
-- ws :: Parser String
-- ws = many (try (char ' ')
--                 <|> try (char '\n')
--                 <|> try (char '\r')
--                 <|> char '\t')


-- Errors
reservedError :: Text -> Parser a
reservedError = customFailure . ReservedKeyword


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
        rest <- many lowerChar; ws
        pure (first : rest)

typeId :: Parser String
typeId = do
        first <- upperChar
        rest <- many lowerChar
        pure (first : rest)

formalParam :: Parser FormalParam
formalParam = do
            varName <- name
            _ <- ch ':'
            typeName <- typeId
            let p = FormalParam varName typeName
            pure p


fnDef :: Parser AST
fnDef = do
            st "fn" <?> "function keyword"
            fnName <- name; ws
            ch '('
            params <- formalParam `sepBy` char ','
            ch ')'
            if fnName == "butt" then reservedError $ T.pack fnName
            else return $ FnDef fnName params

-- trace :: String -> a -> a
-- trace string expr = unsafePerformIO $ do
--     putStrLn string
--     return expr

moduleDef :: Parser AST
moduleDef = do
                st "module";
                modName <- lexeme $ (some lowerChar <?> "module name")
                ch ';'
                functions <- lexeme $ many fnDef
                eof
                return $ ModuleDef modName functions

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
