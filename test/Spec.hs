{-# LANGUAGE OverloadedStrings #-}
--import Test.HUnit
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text
import Lib




-- test1 = TestCase (assertEqual "for (foo 3)," (1,2) (1,2))
-- tests = TestList [TestLabel "test1" test1]

--main :: IO ()
-- main = do
--         runTestTT tests
--         return ()

main :: IO ()
main = hspec $ do
  describe "name parser" $ do
    it "should parse a simple name" $
      parse (name :: Parser String) "test" "foo" `shouldParse` "foo"
    it "should parse a simple name with trailing whitespace" $
      parse (name :: Parser String) "test" "foo  " `shouldParse` "foo"
    it "should eat the whitespace" $
      runParser' name (initialState "foo  ") `succeedsLeaving` ""
  describe "shouldFail" $
    it "invalid parse" $
      parse (name :: Parser String) "test" `shouldFailOn` "Foo"