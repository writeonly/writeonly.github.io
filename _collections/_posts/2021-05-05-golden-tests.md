---
title:    'Złote testy'
author:   TheKamilAdam
category: haskell-eta
langs:    haskell
libs:     hspec hunit
projects: helcam
eso:      eta whitespace
tags:     abstraction pattern-matching sequence type-class
redirect_from:
- pattern-matching
- haskell-eta/pattern-matching
---

Characterization test

https://ro-che.info/articles/2017-12-04-golden-tests


## Taste

## HSpec

### Zwykłe testy

```haskell
spec :: Spec
spec = do 
  describe "Examples" $ do
    it "true"     $ do reduce trueIL           `shouldBe` trueIL
    it "hello"    $ do reduce helloIL          `shouldBe` helloIL
    it "pip"      $ do reduce pipIL            `shouldBe` pipILReduced
    it "pip2"     $ do reduce pip2IL           `shouldBe` pip2ILReduced
    it "reverse"  $ do reduce reverseIL        `shouldBe` reverseILReduced
    it "function" $ do reduce functionIL       `shouldBe` functionIL
    it "add"      $ do reduce addILLinked      `shouldBe` addILReduced
    it "writestr" $ do reduce writeStrIL       `shouldBe` writeStrILReduced
    it "hello2"   $ do reduce hello2ILLinked   `shouldBe` hello2ILReduced
    it "hello4"   $ do reduce hello4ILLinked   `shouldBe` hello2ILReduced
    it "writenum" $ do reduce writeNumILLinked `shouldBe` writeNumILReduced
    it "multiply" $ do reduce multiplyIL       `shouldBe` multiplyILReduced
    it "readnum"  $ do reduce readNumILLinked  `shouldBe` readNumILReduced
    it "fact"     $ do reduce factILLinked     `shouldBe` factILReduced
    it "bottles"  $ do reduce bottlesILLinked  `shouldBe` bottlesILReduced
    it "euclid"   $ do reduce euclidIL         `shouldBe` euclidILReduced
```

### Złote testy

```haskell
  describe "Examples" $ do
    it "true"     $ do generateCode trueIL            `shouldBeDo` readEtaFile "true"
    it "hello"    $ do generateCode helloIL           `shouldBeDo` readEtaFile "hello"
    it "pip"      $ do generateCode pipILReduced      `shouldBeDo` readEtaFile "pip"
    it "pip2"     $ do generateCode pip2ILReduced     `shouldBeDo` readEtaFile "pip2"
    it "reverse"  $ do generateCode reverseILReduced  `shouldBeDo` readEtaFile "reverse"
    it "function" $ do generateCode functionIL        `shouldBeDo` readEtaFile "function"
    it "add"      $ do generateCode addILReduced      `shouldBeDo` readEtaFile "add"
    it "writestr" $ do generateCode writeStrILReduced `shouldBeDo` readEtaFile "writestr"
    it "hello2"   $ do generateCode hello2ILReduced   `shouldBeDo` readEtaFile "hello2"
    it "writenum" $ do generateCode writeNumILReduced `shouldBeDo` readEtaFile "writenum"
    it "multiply" $ do generateCode multiplyILReduced `shouldBeDo` readEtaFile "multiply"
    it "readnum"  $ do generateCode readNumILReduced  `shouldBeDo` readEtaFile "readnum"
    it "fact"     $ do generateCode factILReduced     `shouldBeDo` readEtaFile "fact"
    it "bottles"  $ do generateCode bottlesILReduced  `shouldBeDo` readEtaFile "bottles"
    it "euclid"   $ do generateCode euclidILReduced   `shouldBeDo` readEtaFile "euclid"
```

```haskell
infix 1 `shouldBeDo`
shouldBeDo :: (HasCallStack, Show a, Eq a) => a -> IO a -> Expectation
shouldBeDo action expected = shouldBe action =<< expected
```

### Anty złote testy

```haskell
spec :: Spec
spec = do
  describe "tokenize" $ do

    describe "original ETA" $ do
      it "hello"   $ do tokenize <$> readEtaFile "source/hello"   `shouldReturn` helloTL
      it "hello2"  $ do tokenize <$> readEtaFile "source/hello2"  `shouldReturn` hello2TL
      it "pip"     $ do tokenize <$> readEtaFile "source/pip"     `shouldReturn` pipTL
      it "pip2"    $ do tokenize <$> readEtaFile "source/pip2"    `shouldReturn` pip2TL
      it "fact"    $ do tokenize <$> readEtaFile "source/fact"    `shouldReturn` factTL
      it "bottles" $ do tokenize <$> readEtaFile "source/bottles" `shouldReturn` bottlesTL
      it "crlf"    $ do tokenize <$> readEtaFile "source/crlf"    `shouldReturn` crlfTL
```

```haskell
spec :: Spec
spec = do
  describe "Files" $ do
    it "true"     $ do linkFile "true"     `shouldParseReturn` trueIL
    it "hello"    $ do linkFile "hello"    `shouldParseReturn` helloIL
    it "pip"      $ do linkFile "pip"      `shouldParseReturn` pipIL
    it "pip2"     $ do linkFile "pip2"     `shouldParseReturn` pip2IL
    it "reverse"  $ do linkFile "reverse"  `shouldParseReturn` reverseIL
    it "function" $ do linkFile "function" `shouldParseReturn` functionIL
    it "writestr" $ do linkFile "writestr" `shouldParseReturn` writeStrIL
    it "hello2"   $ do linkFile "hello2"   `shouldParseReturn` hello2ILLinked
    it "hello3"   $ do linkFile "hello3"   `shouldParseReturn` hello2ILLinked
    it "hello4"   $ do linkFile "hello4"   `shouldParseReturn` hello4ILLinked
    it "writenum" $ do linkFile "writenum" `shouldParseReturn` writeNumIL
    it "multiply" $ do linkFile "multiply" `shouldParseReturn` multiplyIL
    it "readnum"  $ do linkFile "readnum"  `shouldParseReturn` readNumIL
    it "fact"     $ do linkFile "fact"     `shouldParseReturn` factILLinked
    it "bottles"  $ do linkFile "bottles"  `shouldParseReturn` bottlesILLinked
    it "euclid"   $ do linkFile "euclid"   `shouldParseReturn` euclidIL

----

linkFile :: String -> ParsedIO InstructionList
linkFile fileName = linkLibIO SourcePath {dirPath = "examples/eas/", filePath = buildEtaFileName fileName}
```

```haskell
infix 1 `shouldParseReturn`
shouldParseReturn :: (Show a, Eq a) => ParsedIO a -> a -> Expectation
shouldParseReturn action = shouldReturn (joinEitherToIO action)
```

### Podwójnie złote testy

```haskell
spec :: Spec
spec = do
  describe "Files" $ do
    forM_ [ "true"
          , "hello"
          , "pip"
          , "pip2"
          , "reverse"
          , "function"
          , "add"
          , "writestr"
          , "hello2"
          , "hello3"
          , "hello4"
--          , "writenum"
          , "multiply"
--          , "readnum"
          , "fact"
          , "bottles"
          , "euclid"
          ] $ \filename -> do
      it filename $ do assembleFile filename `goldenShouldParse` readEtaFile filename

----

assembleFile :: String -> ParsedIO String
assembleFile fileName = assemblyIO SourcePath {dirPath = easDir, filePath = buildAbsoluteEtaFileName fileName}
```

```haskell
spec :: Spec
spec = do
  describe "raw" $ do
    forM_ [ "hello"
          , "hello2"
          , "pip"
          , "pip2"
          , "fact"
          , "bottles"
          , "crlf"
          ] $ \filename -> do
       it filename $ do (show . readTokens <$> readEtaFile ("source/" <> filename)) `goldenShouldBe` readEtaFile ("raw/" <> filename)
```

```haskell

infix 1 `goldenShouldBe`
goldenShouldBe :: (HasCallStack, Show a, Eq a) => IO a -> IO a -> Expectation
goldenShouldBe action expected = join $ liftA2 shouldBe action expected
```

```haskell

infix 1 `goldenShouldParse`
goldenShouldParse :: (Show a, Eq a) => ParsedIO a -> IO a -> Expectation
goldenShouldParse action expected = join $ liftA2 shouldParse action expected
```

```haskell
type ParsedIO a = IO (Parsed a)

type Parsed a = Either String a

type ParsedIOT a = ExceptT String IO a
```

### Złote testy według HSpec

