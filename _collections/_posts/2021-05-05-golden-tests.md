---
title:    'Złote testy'
author:   TheKamilAdam
category: haskell-eta
langs:    haskell
libs:     attoparsec hspec hunit taste 
projects: helma helpa
eso:      eta whitespace
tags:     framework testing
redirect_from:
- golden-tests
- haskell-eta/golden-tests
---

Characterization test


Niestety napisałem brzydkie testy z wykorzystaniem HUnit.



IHMO największą zaletą HSpec jest generacja modułu Spec



Pisz specyfikacje a nie testy




Trochę offtop, ale IHMO ta cała piramida testów (i odwrócona piramida testów) to pic na wodę. Dlaczego o tym mówimy? Bo pokazywali nam to na konferencjach. A czemu nam to pokazywali? Bo piramida ładnie wygląda na slajdach.
Chyba widziałem wszystkie możliwe ułożenia testów (za wyjątkiem piramid):

Pracowałem w firmach gdzie istniały tylko testy manualne.
Widziałem firmę gdzie istniały tylko testy manualne i jednostkowe, bo testerzy nie mieli czasu pisać testów systemowo-akceptacyjnych.
Pracowałem w firmie gdzie nie dało się powiedzieć czy jest więcej jednostkowych czy systemowo-akceptacyjnych bo programiści pisali swoje testy, a testerzy swoje.
Pracowałem w firmie gdzie była niechęć do testów jednostkowych, a programiści i testerzy wspólnie pisali testy integracyjno-akceptacyjne
Co do samych definicji to nie widziałem żadnego porządnego papieru, który określałby co to jest jednostka. Metoda/funkcja? Klasa/moduł? Pakiet? Mikroserwis? Mikroserwis z własną bazą danych? Wszystkie te sprzeczne definicje można spotkać na konferencjach. Niektórzy wprost mówią że definicje się zmieniły odkąd mamy mikroserwisy. Jeśli ktoś ma uznany papier z porządną definicją z chęcią przeczytam.

Które testy osobiście uważam za najlepsze? Te które są szybkie, ale jednocześnie testują maksymalnie dużo kodu. Dla mnie takimi testami są testy na poziomie mikroserwisu z prawdziwą bazą danych postawioną w dockerze. Jeśli czegoś w prosty sposób nie da się postawić w dockerze to mockuję. Albo na poziomie http, albo dostarczam alternatywną implementację klienta.


Jak już mamy te testu to w którym miejscu należy je umieścić?
Pracujac wiele lat z w Javie  z Mavenem przywykłem do bardzo rozbudowanej i sztywnej hierarchi folderów.
Taką samą chierarcię folderów używa Gradle.
W Haskellu nie ma sztywnych wskazuwek które foldery należy używać.
Generat




## Haskell i złote testy



Haskell posiada trzy frameworki 

### Taste - https://hackage.haskell.org/package/tasty-golden

https://ro-che.info/articles/2017-12-04-golden-tests

https://kseo.github.io/posts/2016-12-15-golden-tests-are-tasty.html


Framework jest użyty w 

https://github.com/UnkindPartition/tasty


https://github.com/UnkindPartition/tasty-golden#readme



https://justinethier.github.io/husk-scheme/

### HSpec - hspec-golden

https://github.com/stackbuilders/hspec-golden#readme

> Golden tests store the expected output in a separated file. Each time a golden test is executed the output of the subject under test (SUT) is compared with the expected output. If the output of the SUT changes then the test will fail until the expected output is updated.

Golden testy przechowują oczekiwane wyniki w oddzielnym pliku. Za każdym razem, gdy wykonywany jest złoty test, dane wyjściowe badanego podmiotu (SUT) są porównywane z oczekiwanymi wynikami. Jeśli wynik testu SUT ulegnie zmianie, test zakończy się niepowodzeniem, dopóki oczekiwane dane wyjściowe nie zostaną zaktualizowane.

hspec-golden pozwala na pisanie złotych testów przy użyciu popularnego hspec. 



### HUnit

https://github.com/hspec/HUnit#readme

HUnit jest (jak nazwa wskazuje) dedykoany do testów jednostkowych, a złote testy to z założenia testy integracyjne.
A przynajmniej integracyjne małe (czyli integrujące się z systemem plików).
W zasadzie to jedyne co chciałem napisać o HUnit to przestrzec przed jego używaniem.
Za równo budowanie drzewa testów, jak i używanie operatorów `@?` `@=?` i `@?=` jako odpowiedników dla assertBool/assertEqual.
Użyłem tego frameworku w HelMA, ale ostatecznie wszystkie testy przepisałem na HSpec








## Złote testy w  i HelMA i HelPA

Ja jednak nie zdecydowałem się na złote testy.
uznałem że framework do złotych testów za bardzo narzuca mi formę

Jednak zdecydowałem się przenieść długich sekwencji kodu do plików.
Nazwałem to pirytowe testy (ang. *pyritec tests*)


Rozważymy trzy sytuacje


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

Testujemy funkcje `reduce` 

``shouldBe`` to funkcja assercja

Inaczej porównanie musiało by wyglądać:

```haskell
    it "reverse"  $ do shouldBe (reduce reverseIL) reverseILReduced
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

Po lewej mamy generowanie kodu 
Po prawej mamy wczytanie pliku z kodem źródłowym w **[ETA]**

Tym razem musimy samodzielnie napisać asercję:
```haskell
infix 1 `shouldBeDo`
shouldBeDo :: (HasCallStack, Show a, Eq a) => a -> IO a -> Expectation
shouldBeDo action expected = shouldBe action =<< expected
```

`infix 1 `shouldBeDo``  pozwala ustalić prirotytet operatora

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

`linkFile` Wczytuje plik źródłowy i linkuje do niego szystkie potrzebne biblioteki.


```haskell
infix 1 `shouldParseReturn`
shouldParseReturn :: (Show a, Eq a) => ParsedIO a -> a -> Expectation
shouldParseReturn action = shouldReturn (joinEitherToIO action)
```

`ParsedIO` to pomocniczy typ przydatny przy używaniu attoparsec

```haskell
type ParsedIO a = IO (Parsed a)

type Parsed a = Either String a
```

### Podwójnie złote testy

Tutaj wszystkie testy wygenerujemy na podstaie tablicy:

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
       it filename $ do (show . readTokens <$> readEtaFile ("source/" <> filename)) `pyriticShouldBe` readEtaFile ("raw/" <> filename)
```

jest to możliwe ponieważ plik wejścioy i wyjściowy nazywają się tak samo.
Różnią się tylko folderem i rozszerzeniem

```haskell
infix 1 `pyriticShouldBe`
pyriticShouldBe :: (HasCallStack, Show a, Eq a) => IO a -> IO a -> Expectation
pyriticShouldBe action expected = join $ liftA2 shouldBe action expected
```

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
          , "multiply"
          , "fact"
          , "bottles"
          , "euclid"
          ] $ \filename -> do
      it filename $ do assembleFile filename `pyriticShouldParse` readEtaFile filename

----

assembleFile :: String -> ParsedIO String
assembleFile fileName = assemblyIO SourcePath {dirPath = easDir, filePath = buildAbsoluteEtaFileName fileName}
```

I pomocnicza funkcja do porównywania 
```haskell
infix 1 `pyriticShouldParse`
pyriticShouldParse :: (Show a, Eq a) => ParsedIO a -> IO a -> Expectation
pyriticShouldParse action expected = join $ liftA2 shouldParse action expected
```


## czas - czy to nie jest za wolne

Czytamy te wszystkie wartości z plików. 
Czy to nie jest za wolne?

Z pomocą przychodzi nam tu biblioteka hspec-slow

Cabal domyślnie nie tworzy folderów na kod i testy.
Za to 

Konwencja anrzucona przez stack, podtrzymywana przez 
* folder src
* folder test z plikiem Spec.hs
* folder app z plikiem Main.hs

## Łamanie konwencji

Te trzy foldery do folderu hs
Start testów z pliku hs/test/main

```haskell
module Main where

import qualified Spec
import Test.Hspec.Slow
import Test.Hspec (hspec)

main :: IO ()
main = do
  config <- configure 1
  hspec $ timeThese config Spec.spec
```

## Czy to napewno były złote testy Złote testy według HSpec

