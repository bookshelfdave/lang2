{-# LANGUAGE OverloadedStrings #-}
--import Test.HUnit
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error
import Data.Text
import Lib
import Lib (FormalParam(..), CustomParseErrors (UndefinedType))


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
                parse (name :: Parser String) "test" "foo" `shouldParse` "foo"
            it "should parse a simple name with trailing whitespace" $
                parse (name :: Parser String) "test" "foo  " `shouldParse` "foo"
            it "should eat the whitespace" $
                runParser' name (initialState "foo  ") `succeedsLeaving` ""
            it "can contain numbers" $
                parse (name :: Parser String) "test" "foo123" `shouldParse` "foo123"
            it "can contain underscores" $
                parse (name :: Parser String) "test" "foo123_abc_123" `shouldParse` "foo123_abc_123"

        describe "name parser should fail" $ do
            it "can't start with an uppercase letter" $
                parse (name :: Parser String) "test" `shouldFailOn` "Foo"
            it "can't start with whitespace" $
                parse (name :: Parser String) "test" `shouldFailOn` "  foo"
            it "can't start with a number" $
                parse (name :: Parser String) "test" `shouldFailOn` "9foo"
            it "can't start with a underscore" $
                parse (name :: Parser String) "test" `shouldFailOn` "_foo"


testTypeId :: IO ()
testTypeId = 
    hspec $ do
        describe "type parser" $ do
            it "should parse a simple type" $
                parse (typeId :: Parser String) "test" "Int" `shouldParse` "Int"
            it "should parse a simple type with trailing whitespace" $
                parse (typeId :: Parser String) "test" "Int  " `shouldParse` "Int"
            it "should eat the whitespace" $
                runParser' typeId (initialState "Int  ") `succeedsLeaving` ""
        describe "type parser should fail" $ do
            it "type starting with lowercase letter" $
                parse (typeId :: Parser String) "test" `shouldFailOn` "foo"
            it "type starting with whitespace" $
                parse (typeId :: Parser String) "test" `shouldFailOn` "  foo"
            it "invalid type" $
                parse (typeId :: Parser String) "test" "foo" `shouldFailWith` err 0 (utok 'f' <> elabel "uppercase letter")
                --parse (typeId :: Parser String) "test" "foo" `shouldFailWith` errFancy 1 (fancy $ ErrorCustom $ UndefinedType "foo")




testFormalParam :: IO ()
testFormalParam = 
    hspec $ do
        describe "type parser" $ do
            it "should parse a simple type" $
                parse (formalParam :: Parser FormalParam) "" "foo:Int" 
                    `shouldParse` FormalParam "foo" "Int"
            it "should parse a simple type with whitespace" $
                parse (formalParam :: Parser FormalParam) "" "foo : Int " 
                    `shouldParse` FormalParam "foo" "Int"
        describe "formalParam parser should fail" $ do
            it "can't start with a typeId" $
                parse (formalParam :: Parser FormalParam) "test" `shouldFailOn` "Foo:Int"
            it "typeId can't be a name" $
                parse (formalParam :: Parser FormalParam) "test" `shouldFailOn` "foo:int"

testFnDef :: IO ()
testFnDef = 
    hspec $ do
        describe "function def" $ do
            it "should parse a function body" $
                parse (fnDef :: Parser [Expr]) ""  
                    `shouldSucceedOn` "fn test(a:Int) { 1+2+a; }" 

main :: IO ()
main = do
        testName
        testTypeId
        testFormalParam
        testFnDef