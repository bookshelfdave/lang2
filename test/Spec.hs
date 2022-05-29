{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import System.IO

import AST (Decl(..), FnDef(..), Stmt(..), TypedParam(..))

--import Test.HUnit
import Data.Void
import Lib
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error

-- test1 = TestCase (assertEqual "for (foo 3)," (1,2) (1,2))
-- tests = TestList [TestLabel "test1" test1]
--main :: IO ()
-- main = do
--         runTestTT tests
--         return ()
testName :: IO ()
testName =
  hspec $ do
    describe "name parser" $ do
      it "can parse a simple name" $ does "foo" `shouldParse` "foo"
      it "can parse a simple name with trailing whitespace" $
        does "foo  " `shouldParse` "foo"
      it "can eat the whitespace" $
        runParser' name (initialState "foo  ") `succeedsLeaving` ""
      it "can contain numbers" $ does "foo123" `shouldParse` "foo123"
      it "can contain underscores" $
        does "foo123_abc_123" `shouldParse` "foo123_abc_123"
    describe "name parser should fail" $ do
      it "can't start with an uppercase letter" $ does `shouldFailOn` "Foo"
      it "can't start with whitespace" $ does `shouldFailOn` "  foo"
      it "can't start with a number" $ does `shouldFailOn` "9foo"
      it "can't start with a underscore" $ does `shouldFailOn` "_foo"
  where
    does = parse (name :: Parser String) ""

testTypeId :: IO ()
testTypeId =
  hspec $ do
    describe "type parser" $ do
      it "can parse a simple type" $ does "Int" `shouldParse` "Int"
      it "can parse a simple type with trailing whitespace" $
        does "Int  " `shouldParse` "Int"
      it "can eat the whitespace" $
        runParser' typeId (initialState "Int  ") `succeedsLeaving` ""
    describe "type parser should fail" $ do
      it "can parse a type starting with lowercase letter" $
        does `shouldFailOn` "foo"
      it "can parse a type starting with whitespace" $
        does `shouldFailOn` "  foo"
      it "can't parse an invalid type" $
        does "foo" `shouldFailWith`
        err 0 (utok 'f' <> elabel "uppercase letter")
  where
    does = parse (typeId :: Parser String) ""

-- save this!
--parse (typeId :: Parser String) "test" "foo" `shouldFailWith` errFancy 1 (fancy $ ErrorCustom $ UndefinedType "foo")
testFormalParam :: IO ()
testFormalParam =
  hspec $ do
    describe "type parser" $ do
      it "can parse a simple type" $
        does "foo:Int" `shouldParse` TypedParam "foo" "Int"
      it "can parse a simple type with whitespace" $
        does "foo : Int " `shouldParse` TypedParam "foo" "Int"
    describe "formalParam parser should fail" $ do
      it "can't start with a typeId" $ does `shouldFailOn` "Foo:Int"
      it "typeId can't be a name" $ does `shouldFailOn` "foo:int"
  where
    does = parse (formalParam :: Parser TypedParam) ""

testFnDef :: IO ()
testFnDef =
  hspec $ do
    describe "function def" $ do
      it "can parse a function body without return type" $
        does `shouldSucceedOn` "fn test(a:Int) { 1+2+a; }"
      it "can parse a function body with return type" $
        does `shouldSucceedOn` "fn test(a:Int) -> Int { 1+2+a; }"
  where
    does = parse (fnDef :: Parser FnDef) ""

testVarDecl :: IO ()
testVarDecl =
  hspec $ do
    describe "variable declarations" $ do
      it "can should parse a simple expression" $
        does `shouldSucceedOn` "var x:Int = 1;"
      it "can should parse a complex expression" $
        does `shouldSucceedOn` "var x:Int = 1 + 2 + (5*4);"
  where
    does = parse (varDecl :: Parser Decl) ""

testExprParse :: IO ()
testExprParse =
  hspec $ do
    describe "expressions" $ do
      it "can contain a single integer" $ does `shouldSucceedOn` "1"
      it "can contain a single variable" $ does `shouldSucceedOn` "a"
      it "can parse a Type name" $ does `shouldFailOn` "Foo"
      it "can parse simple arithmetic" $
        does `shouldSucceedOn` "1 + 1 + (5 - 3) * (2 / 5)"
      it "can parse simple arithmetic with comments" $
        does `shouldSucceedOn` "1 + 1 + (5 - 3) /* test */ * (2 / 5 /* test */)"
      it "can contain function calls" $ does `shouldSucceedOn` "1 + length(a)"
      it "can fail to parse function calls separated by spaces" $
        does `shouldFailOn` "length(a) length(b)"
      it "can contain function calls and variable references (a)" $
        does `shouldSucceedOn` "length(a) + b + 1"
      it "can contain function calls and variable references (b)" $
        does `shouldSucceedOn` "b + length(a) + 1"
      it "can contain true" $ does `shouldSucceedOn` "true"
      it "can contain false" $ does `shouldSucceedOn` "false"
      it "can use && and ||" $ does `shouldSucceedOn` "true || false && a"
      it "can use == and !=" $ does `shouldSucceedOn` "a != b && b == c"
  where
    does = parse (pExpr <* eof :: Parser Expr) ""

testExprTree :: IO ()
testExprTree =
  hspec $ do
    describe "expression trees" $ do
      
      it "returns a Var" $ does "a" `shouldParse` Var "a"
      it "returns an Int" $ does "100" `shouldParse` Int 100
      it "returns a negated Int" $ does "-100" `shouldParse` Negation (Int 100)
      it "returns Bool True" $ does "true" `shouldParse` Bool True
      it "returns Bool False" $ does "false" `shouldParse` Bool False
      it "returns a negated bool" $
        does "!true" `shouldParse` BoolNegation (Bool True)
      -- TODO: fix descriptions below!
      it "can parse addition" $ does "5 + 4" `shouldParse` Sum (Int 5) (Int 4)
      it "can parse subtraction" $
        does "5 - 4" `shouldParse` Subtr (Int 5) (Int 4)
      it "can parse multiplication" $
        does "5 * 4" `shouldParse` Product (Int 5) (Int 4)
      it "can parse division" $
        does "5 / 4" `shouldParse` Division (Int 5) (Int 4)
      it "can parse and" $ does "a && b" `shouldParse` And (Var "a") (Var "b")
      it "can parse and" $ does "a || b" `shouldParse` Or (Var "a") (Var "b")
      it "can parse equality" $
        does "a == 1" `shouldParse` Equality (Var "a") (Int 1)
      it "can parse equality" $
        does "a != 1" `shouldParse` NotEquality (Var "a") (Int 1)
      it "can parse function calls" $
        does "a(1 , 2, b)" `shouldParse` FnCall "a" [Int 1, Int 2, Var "b"]
      it "can parse complex expressions (1)" $
        does "true || tree(a, 1*5)" `shouldParse`
        Or (Bool True) (FnCall "tree" [Var "a", Product (Int 1) (Int 5)])
      it "can parse my dear aunt sally" $
        does "1+4*5-10/2" `shouldParse`
        Subtr
          (Sum (Int 1) (Product (Int 4) (Int 5)))
          (Division (Int 10) (Int 2))
  where
    does = parse (pExpr <* eof :: Parser Expr) ""

testFnBody :: IO ()
testFnBody =
  hspec $ do
    describe "Function bodies" $ do
      it "can contain at least one statement" $
        does `shouldFailOn` "fn foo(x:Int, y:Int) -> Int {}"
      it "can contain a single statement being a return" $
        does `shouldSucceedOn` "fn foo(x:Int, y:Int) -> Int { return 1;}"
      it "needs a ; after a statement" $
        does `shouldFailOn` "fn foo(x:Int, y:Int) -> Int { return 1}"
      it "can contain blocks" $
        does `shouldSucceedOn`
        "fn foo(x:Int, y:Int) -> Int { var x:Int = 1; { var y:Int = 2;}}"
      it "can return true" $
        does `shouldSucceedOn` "fn foo(x:Int, y:Int) -> Int { return true;}"
      it "can return false" $
        does `shouldSucceedOn` "fn foo(x:Int, y:Int) -> Int { return false;}"
      it "can contain variable decls, return statements, expressions" $
        does `shouldSucceedOn`
        "fn foo(x:Int, y:Int) -> Int { \
         \ var a:Int = 1; \
         \ print(a); \
         \  return 1; \
         \  }"
      it "can contain if statements" $
        does `shouldSucceedOn`
        "fn foo(x:Int, y:Int) -> Int { \
         \ if true { var x:Int = 1;} \
         \ else { var y: Int = 1;} \
         \ return 1; \
         \ }"
  where
    does = parse (fnDef :: Parser FnDef) ""

-- fnBodyStmt :: Parser Stmt
-- fnBodyStmt = do
--   choice
--     [ Decl <$> varDecl <* semi
--     , try stmtReturn <* semi
--     , try stmtIfElse
--     , Expr <$> pExpr <* semi
--     , block
-- data Decl
--   = VarDecl FormalParam Expr
--   | FnDecl FnDef
--   deriving (Show)
-- data Stmt
--   = Block [Stmt]
--   | Decl Decl
--   | Expr Expr
--   | For
--   | IfElse Expr Stmt Stmt
--   | Return Expr
--   deriving (Show)
testFnBodyStmt :: IO ()
testFnBodyStmt =
  hspec $ do
    describe "Statements" $ do
      -- TODO: fix descriptions
      it "can parse variable declarations" $ does "var a:Int = 1;" `shouldParse` 
        Decl (VarDecl (TypedParam "a" "Int") (Int 1))
      it "can parse a variable" $ does "var a:Int = 1;" `shouldParse` 
        Decl (VarDecl (TypedParam "a" "Int") (Int 1))

  where
    does = parse (fnBodyStmt :: Parser Stmt) ""

main :: IO ()
main = do
  testName
  testTypeId
  testFormalParam
  testFnDef
  testVarDecl
  testFnBody
  testExprParse
  testExprTree
  testFnBodyStmt
