{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import System.IO

--import Test.HUnit
import Data.Void
import AST (Decl(..), FnDef(..), Stmt(..))
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
      it "should parse a simple name" $
        does "foo" `shouldParse` "foo"
      it "should parse a simple name with trailing whitespace" $
        does "foo  " `shouldParse` "foo"
      it "should eat the whitespace" $
        runParser' name (initialState "foo  ") `succeedsLeaving` ""
      it "can contain numbers" $
        does "foo123" `shouldParse` "foo123"
      it "can contain underscores" $
        does "foo123_abc_123" `shouldParse` "foo123_abc_123"
    describe "name parser should fail" $ do
      it "can't start with an uppercase letter" $
        does `shouldFailOn` "Foo"
      it "can't start with whitespace" $
        does `shouldFailOn` "  foo"
      it "can't start with a number" $
        does `shouldFailOn` "9foo"
      it "can't start with a underscore" $
        does `shouldFailOn` "_foo"
      where does = parse (name :: Parser String) ""

testTypeId :: IO ()
testTypeId =
  hspec $ do
    describe "type parser" $ do
      it "should parse a simple type" $
        does "Int" `shouldParse` "Int"
      it "should parse a simple type with trailing whitespace" $
        does "Int  " `shouldParse` "Int"
      it "should eat the whitespace" $
        runParser' typeId (initialState "Int  ") `succeedsLeaving` ""
    describe "type parser should fail" $ do
      it "type starting with lowercase letter" $
        does `shouldFailOn` "foo"
      it "type starting with whitespace" $
        does `shouldFailOn` "  foo"
      it "invalid type" $
        does "foo" `shouldFailWith`
        err 0 (utok 'f' <> elabel "uppercase letter")
      where does = parse (typeId :: Parser String) "" 

-- save this!
--parse (typeId :: Parser String) "test" "foo" `shouldFailWith` errFancy 1 (fancy $ ErrorCustom $ UndefinedType "foo")

testFormalParam :: IO ()
testFormalParam =
  hspec $ do
    describe "type parser" $ do
      it "should parse a simple type" $
        does "foo:Int" `shouldParse` FormalParam "foo" "Int"
      it "should parse a simple type with whitespace" $
        does "foo : Int " `shouldParse` FormalParam "foo" "Int"
    describe "formalParam parser should fail" $ do
      it "can't start with a typeId" $
        does `shouldFailOn` "Foo:Int"
      it "typeId can't be a name" $
        does `shouldFailOn` "foo:int"
      where does = parse (formalParam :: Parser FormalParam) ""

testFnDef :: IO ()
testFnDef =
  hspec $ do
    describe "function def" $ do
      it "should parse a function body without return type" $
        does `shouldSucceedOn`
        "fn test(a:Int) { 1+2+a; }"
      it "should parse a function body with return type" $
        does `shouldSucceedOn`
        "fn test(a:Int) -> Int { 1+2+a; }"
      where does = parse (fnDef :: Parser FnDef) ""

testVarDecl :: IO ()
testVarDecl =
  hspec $ do
    describe "variable declarations" $ do
      it "should should parse a simple expression" $
        does `shouldSucceedOn` "var x:Int = 1;"
      it "should should parse a complex expression" $
        does `shouldSucceedOn` "var x:Int = 1 + 2 + (5*4);"
      where does = parse (varDecl :: Parser Decl) ""

testFnBody :: IO ()
testFnBody =
  hspec $ do
    describe "Function bodies" $ do
      it "should contain at least one statement" $
        does `shouldFailOn` "fn foo(x:Int, y:Int) -> Int {}"
      it "can contain a single statement being a return" $
        does `shouldSucceedOn` "fn foo(x:Int, y:Int) -> Int { return 1;}"
      it "needs a ; after a statement" $
        does `shouldFailOn` "fn foo(x:Int, y:Int) -> Int { return 1}"
      it "can contain variable decls, return statements, expressions" $
        does `shouldSucceedOn`
        "fn foo(x:Int, y:Int) -> Int { \
         \ var a:Int = 1; \
         \ print(a); \
         \  return 1; \
         \  }"
      where does = parse (fnDef :: Parser FnDef) ""
testExpr :: IO ()
testExpr =
  hspec $ do
    describe "expressions" $ do
      it "can contain a single integer" $
        does `shouldSucceedOn` "1"
      it "can contain a single variable" $
        does `shouldSucceedOn` "a"
      it "should parse a Type name" $
        does `shouldFailOn` "Foo"
      it "can parse simple arithmetic" $
        does `shouldSucceedOn` "1 + 1 + (5 - 3) * (2 / 5)"
      it "can parse simple arithmetic with comments" $
        does `shouldSucceedOn` "1 + 1 + (5 - 3) /* test */ * (2 / 5 /* test */)"
      it "can contain function calls" $
        does `shouldSucceedOn` "1 + length(a)"
      it "should fail to parse function calls separated by spaces" $
        does `shouldFailOn` "length(a) length(b)"
      it "can contain function calls and variable references (a)" $
        does `shouldSucceedOn` "length(a) + b + 1"
      it "can contain function calls and variable references (b)" $
        does `shouldSucceedOn` "b + length(a) + 1"
      where does = parse (pExpr <* eof:: Parser Expr) "" 

main :: IO ()
main = do
  testName
  testTypeId
  testFormalParam
  testFnDef
  testVarDecl
  testFnBody
  testExpr