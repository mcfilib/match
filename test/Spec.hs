{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.Hspec

import Lib

main :: IO ()
main = do
  a <- testSpec "Parser Specs" parserSpecs
  b <- testSpec "Compiler Specs" compilerSpecs
  c <- testSpec "Match Specs" matchSpecs
  d <- testSpec "Sanitisation Specs" sanitisationSpecs
  e <- testSpec "Integration Specs" integrationSpecs
  f <- testSpec "Feedback Specs" feedbackSpecs
  defaultMain (tests $ testGroup "All specs" [ a, b, c, d, e, f ])

tests :: TestTree -> TestTree
tests specs = testGroup "Engine Tests" [ specs ]

parserSpecs :: Spec
parserSpecs = describe "Parser" $
  describe "decode" $ do
    it "should parse a simple capture" $ do
      let subject = decode "%{1}"
      let result = Right [ ExprCapture (Simple 1) ]
      subject `shouldBe` result

    it "should parse a space limitation capture" $ do
      let subject = decode "%{1S3}"
      let result = Right [ ExprCapture (SpaceLimitation 1 3) ]
      subject `shouldBe` result

    it "should parse a greedy capture" $ do
      let subject = decode "%{1G}"
      let result = Right [ ExprCapture (Greedy 1) ]
      subject `shouldBe` result

    it "should parse a mixture of different captures" $ do
      let subject = decode "%{1}%{2S3}%{4G}"
      let result = Right [ ExprCapture (Simple 1)
                         , ExprCapture (SpaceLimitation 2 3)
                         , ExprCapture (Greedy 4)
                         ]
      subject `shouldBe` result

    it "should parse text" $ do
      let subject = decode "hello world"
      let result = Right [ ExprText "hello world" ]
      subject `shouldBe` result

    it "should parse text mixed with captures" $ do
      let subject = decode "hello %{1} from %{2G}"
      let result = Right [ ExprText "hello "
                         , ExprCapture (Simple 1)
                         , ExprText " from "
                         , ExprCapture (Greedy 2)
                         ]
      subject `shouldBe` result

    it "should parse text with partial captures" $ do
      let subject = decode "hello %world"
      let result = Right [ ExprText "hello %", ExprText "world" ]
      subject `shouldBe` result

compilerSpecs :: Spec
compilerSpecs = describe "Compiler" $
  describe "compile'" $ do
    it "should compile a pattern of text to a regex string" $ do
      let subject = compile' [ ExprText "hello world" ]
      let result = "hello world"
      subject `shouldBe` result

    it "should compile a pattern of simple captures to a regex string" $ do
      let subject = compile' [ ExprCapture (Simple 0) ]
      let result = "(?:.+)"
      subject `shouldBe` result

    it "should compile a pattern of space limitation captures to a regex string" $ do
      let subject = compile' [ ExprCapture (SpaceLimitation 0 0) ]
      let result = "(?:\\S+\\s{0})"
      subject `shouldBe` result

    it "should compile a pattern of space limitation captures to a regex string" $ do
      let subject = compile' [ ExprCapture (SpaceLimitation 0 1) ]
      let result = "(?:\\S+\\s\\S+)"
      subject `shouldBe` result

    it "should compile a pattern of greedy captures to a regex string" $ do
      let subject = compile' [ ExprCapture (Greedy 0) ]
      let result = "(?:.*)"
      subject `shouldBe` result

    it "should compile a mixed pattern to a regex string" $ do
      let subject = compile' [ ExprText "foo "
                             , ExprCapture (Simple 0)
                             , ExprText " is a "
                             , ExprCapture (SpaceLimitation 1 0)
                             ]
      let result = "foo (?:.+) is a (?:\\S+\\s{0})"
      subject `shouldBe` result

matchSpecs :: Spec
matchSpecs = describe "Match" $
  describe "match'" $
    it "should check if our regex matches" $ do
      let subject = match' [ ExprText "foo "
                           , ExprCapture (Simple 0)
                           , ExprText " is a "
                           , ExprCapture (SpaceLimitation 1 0)
                           ] "foo blah is a bar"
      let result = Just [ "foo blah is a bar" ]
      subject `shouldBe` result

sanitisationSpecs :: Spec
sanitisationSpecs = describe "Santisation" $
  describe "sanitise" $ do
    it "should not touch captures" $ do
      let subject = sanitise [ ExprCapture (SpaceLimitation 1 0) ]
      let result = [ ExprCapture (SpaceLimitation 1 0) ]
      subject `shouldBe` result

    it "should escape regex symbols" $ do
      let subject = sanitise [ ExprText "(.*)" ]
      let result = [ ExprText "\\(\\.\\*\\)" ]
      subject `shouldBe` result

integrationSpecs :: Spec
integrationSpecs = describe "Integration" $ do
  it "should pass the simple pattern specification" $
    case decode "foo %{0} is a %{1}" of
      Left _        -> fail "parser error"
      Right pattern -> do
        let regex = compile $ optimise pattern
        match regex "foo blah is a bar" `shouldBe` Just [ "foo blah is a bar" ]
        match regex "foo blah is a very big boat" `shouldBe` Just [ "foo blah is a very big boat" ]
        match regex "foo blah is bar" `shouldBe` Nothing
        match regex "foo blah" `shouldBe` Nothing
        match regex "foo blah is" `shouldBe` Nothing

  it "should pass the space limitation pattern specification" $ do
    case decode "foo %{0} is a %{1S0}" of
      Left _        -> fail "parser error"
      Right pattern -> do
        let regex = compile $ optimise pattern
        match regex "foo blah is a bar" `shouldBe` Just [ "foo blah is a bar" ]
        match regex "foo blah is a very big boat" `shouldBe` Nothing
        match regex "foo blah is bar" `shouldBe` Nothing
        match regex "foo blah" `shouldBe` Nothing
        match regex "foo blah is" `shouldBe` Nothing

    case decode "the %{0S1} %{1} ran away" of
      Left _        -> fail "parser error"
      Right pattern -> do
        let regex = compile $ optimise pattern
        match regex "the big brown fox ran away" `shouldBe` Just [ "the big brown fox ran away" ]

  it "should pass the greedy pattern specification" $
     case decode "bar %{0G} foo %{1}" of
      Left _        -> fail "parser error"
      Right pattern -> do
        let regex = compile $ optimise pattern
        match regex "bar foo bar foo bar foo bar foo" `shouldBe` Just [ "bar foo bar foo bar foo bar foo" ]

feedbackSpecs :: Spec
feedbackSpecs = describe "Feedback" $ do
  it "should address feedback about space limitation modifier" $
    case decode "%{0S0} way home" of
      Left _        -> fail "parser error"
      Right pattern -> do
        let regex = compile $ optimise pattern
        match regex "long way home" `shouldBe` Just [ "long way home" ]
