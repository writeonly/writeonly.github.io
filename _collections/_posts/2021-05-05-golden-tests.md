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

* AsmParser
* Reducer
* CodeGenerator

* Assembler



* ReducerSpec
* AsmParserSpec
* CodeGeneratorSpec
* AssemblerSpec

### ReducerSpec czyli zwykłe testy parametryzowane

Moduł `ReducerSpec` testuje funkcę `Reducer.reduce`.
Funkcja `reduce` zmienia wewnętrzną reprezentację na prostszą tłumacząc skomplikowane instrukcje na prossze
Naszą testowaną funkcją jest `Reducer.reduce :: InstructionList -> InstructionList`, która zamienia listę instrukcji w zredukowaną listę instrukcji.

```haskell
module HelVM.HelPA.Assemblers.EAS.ReducerSpec where

import HelVM.HelPA.Assemblers.EAS.Reducer
import HelVM.HelPA.Assemblers.EAS.TestData

import Test.Hspec

spec :: Spec
spec = do
  describe "reduce" $ do
    forM_ [ ("true"     , trueIL           , trueIL)
          , ("hello"    , helloIL          , helloIL)
          , ("pip"      , pipIL            , pipILReduced)
          , ("pip2"     , pip2IL           , pip2ILReduced)
          , ("reverse"  , reverseIL        , reverseILReduced)
          , ("function" , functionIL       , functionIL)
          , ("add"      , addILLinked      , addILReduced)
          , ("writestr" , writeStrIL       , writeStrILReduced)
          , ("hello2"   , hello2ILLinked   , hello2ILReduced)
          , ("hello4"   , hello4ILLinked   , hello2ILReduced)
          , ("writenum" , writeNumILLinked , writeNumILReduced)
          , ("multiply" , multiplyIL       , multiplyILReduced)
          , ("readnum"  , readNumILLinked  , readNumILReduced)
          , ("fact"     , factILLinked     , factILReduced)
          , ("bottles"  , bottlesILLinked  , bottlesILReduced)
          , ("euclid"   , euclidIL         , euclidILReduced)
          ] $ \(fileName , ilLinked, ilReduced) -> do
      it fileName $ do reduce ilLinked `shouldBe` ilReduced
```

`forM_` zamienia nam zwykłe testy na testy parametryczne.

Nastepnie mamy listę krotek.
Pierwszy element krotki zawiera nazwę testu, drugi - listę instrukcji do zredukowania, trzeci - zredukowaną listę instrukcji.

`shouldBe` to funkcja assercja.
Dzięki grawisom funkcja może być użyta jak operator.
Bez grafisów trzeba by zapisać

```haskell
shouldBe :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
```

Inaczej porównanie musiało by wyglądać tak:
```haskell
      it fileName $ do shouldBe (reduce ilLinked) ilReduced
```

### AsmParser - czyli czytanie inputu testów z pliku

Moduł `AsmParserSpec` testuje funkcję `AsmParser.parseAssembler`.
Funkcja `parseAssembler :: Text -> Parsed InstructionList` parsuje plik w języku `EAS` i zwraca listę instrukcji.
Ponieważ parsowanie może się nie udać lista instrukcji opakowana jest w typ `Parsed`, który ma postać:
```haskell
type Parsed a = Either String a
```

Ponieważ jednak nie będziemy pracować na typie `Text`, a na `IO Text` naszym ostatecznym typem do porównania będzie 
`IO (Parsed InstructionList)` czyli dokładniej `IO (Either String InstructionList)`. który dla wygody nazwę ParsedIO:


Biblioteka [HSpec] nie posiada oczywiście asercji dla typu `IO (Either String a)`,
ale posiada asercję `shouldReturn` dla typu `IO a`:
```haskell
shouldReturn :: (HasCallStack, Show a, Eq a) => IO a -> a -> Expectation
```
Jedyne co musimy zrobić to tylko zamienić `IO (Either String a)` na `IO a`

Najpierw zamieniamy `Either String a` na `IO a`
```haskell
eitherToIO :: Parsed a -> IO a
eitherToIO (Right value)  = return value
eitherToIO (Left message) = fail message

```

A potem możemy dodać do tego składanie (flatmapowanie) `IO (IO a)` na `IO a` 
```haskell
joinEitherToIO :: ParsedIO a -> IO a
joinEitherToIO io = eitherToIO =<< io
```

Teraz możemy wszystko opakować w nową asercję:
```haskell
infix 1 `shouldParseReturn`
shouldParseReturn :: (Show a, Eq a) => ParsedIO a -> a -> Expectation
shouldParseReturn action = shouldReturn (joinEitherToIO action)
```

`infix 1` pozwala ustalić prirotytet operatora

Ostatecznie testy wygląda następująco:
```haskell
module HelVM.HelPA.Assemblers.EAS.AsmParserSpec (spec) where

import HelVM.HelPA.Assemblers.EAS.AsmParser
import HelVM.HelPA.Assemblers.EAS.Instruction
import HelVM.HelPA.Assemblers.EAS.FileUtil
import HelVM.HelPA.Assemblers.EAS.TestData

import HelVM.HelPA.Assemblers.Expectations

import HelVM.HelPA.Common.Value

import Test.Hspec
import Test.Hspec.Attoparsec

spec :: Spec
spec = do
  describe "parseFromFile" $ do
    forM_ [ ("true"     , trueIL)
          , ("hello"    , helloIL)
          , ("pip"      , pipIL)
          , ("pip2"     , pip2IL)
          , ("reverse"  , reverseIL)
          , ("function" , functionIL)
          , ("writestr" , writeStrIL)
          , ("hello2"   , hello2IL <> [D "writestr.eas"])
          , ("hello3"   , hello2IL <> [D "writestr.eas"])
          , ("hello4"   , hello4IL <> [D "writestr.eas"])
          , ("writenum" , writeNumIL)
          , ("multiply" , multiplyIL)
          , ("readnum"  , readNumIL)
          , ("fact"     , factIL   <> [D "readnum.eas",D "writenum.eas",D "multiply.eas",D "writestr.eas"])
          , ("bottles"  , bottlesIL)
          , ("euclid"   , euclidIL)
          ] $ \(fileName , il) -> do
      let parseFromFile = parseAssembler <$> readFileText (buildAbsolutePathToEasFile fileName)
      it fileName $ do parseFromFile `shouldParseReturn` il
```


### CodeGeneratorSpec - czyli złote testy

`CodeGeneratorSpec` testuje funkcję `CodeGenerator.generateCode`.
funkcja `generateCode :: InstructionList -> String` generuje kod w języku `ETA` na podstawie wewnętrzej reprezejtacji. 
Wyniku wygenerowanego w `generateCode` nie będziemy porównywać z wartościami zapisanymi w testach tylko ze złotymi plikami.


Tym razem musimy samodzielnie napisać asercję:
```haskell
infix 1 `goldenShouldBe`
goldenShouldBe :: String -> String -> Golden String
goldenShouldBe actualOutput fileName =
  Golden {
    output = actualOutput,
    encodePretty = show,
    writeToFile = writeFile,
    readFromFile = readFile,
    goldenFile = ".output" </> "golden" </> fileName,
    actualFile = Just (".output" </> "actual" </> fileName),
    failFirstTime = False
  }

```


Kod testu wygląda następująco
```haskell
module HelVM.HelPA.Assemblers.EAS.CodeGeneratorSpec (spec) where

import HelVM.HelPA.Assemblers.EAS.CodeGenerator
import HelVM.HelPA.Assemblers.EAS.TestData
import HelVM.HelPA.Assemblers.EAS.FileUtil

import HelVM.HelPA.Assemblers.Expectations

import Test.Hspec

spec :: Spec
spec = do
  describe "generateCode" $ do
    forM_ [ ("true"     , trueIL)
          , ("pip"      , pipILReduced)
          , ("pip2"     , pip2ILReduced)
          , ("reverse"  , reverseILReduced)
          , ("function" , functionIL)
          , ("add"      , addILReduced)
          , ("writestr" , writeStrILReduced)
          , ("hello2"   , hello2ILReduced)
          , ("hello4"   , hello2ILReduced)
          , ("writenum" , writeNumILReduced)
          , ("multiply" , multiplyILReduced)
          , ("readnum"  , readNumILReduced)
          , ("fact"     , factILReduced)
          , ("bottles"  , bottlesILReduced)
          , ("euclid"   , euclidILReduced)
          ] $ \(fileName , ilReduced) -> do
      it fileName $ do generateCode ilReduced `goldenShouldBe` buildAbsolutePathToEtaFile fileName
```

Tablica zawiera krotki (tuple).
Pierwszy element krotki to nazwa pliku,
drugi element krotki to uproszczona reprezentacja wewnętrzna
Po lewej mamy generowanie kodu 
Po prawej mamy wczytanie pliku z kodem źródłowym w **[ETA]**




### AssemblerSpec czyli podwójnie złote testy

Pora na przetestowanie wszstkiego razem

`AssemblerSpec` testuje `Assembler.assembly`

```haskell
module HelVM.HelPA.Assemblers.EAS.AssemblerSpec where

import HelVM.HelPA.Assemblers.EAS.Assembler
import HelVM.HelPA.Assemblers.EAS.FileUtil

import HelVM.HelPA.Assemblers.Expectations

import HelVM.HelPA.Common.API

import Test.Hspec

spec :: Spec
spec = do
  describe "assembleFile" $ do
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
          ] $ \fileName -> do
      let assembleFile = assembly SourcePath {dirPath = easDir, filePath = buildAbsolutePathToEasFile fileName}    
      it fileName $ do assembleFile `goldenShouldParseReturn` buildAbsolutePathToEtaFile fileName
```

```haskell
infix 1 `goldenShouldParseReturn`
goldenShouldParseReturn :: ParsedIO String -> String -> WrappedGoldenIO String
goldenShouldParseReturn = goldenShouldReturn . joinEitherToIO
```

```haskell
goldenShouldReturn' :: IO String -> String -> GoldenIO String
goldenShouldReturn' actualOutputIO fileName = flip goldenShouldBe fileName <$> actualOutputIO

type GoldenIO a = IO (Golden a)
```

Niestety to nie zadziała

```haskell
infix 1 `goldenShouldReturn`
goldenShouldReturn :: IO String -> String -> WrappedGoldenIO String
goldenShouldReturn actualOutputIO = WrappedGoldenIO . goldenShouldReturn' actualOutputIO

newtype WrappedGoldenIO a = WrappedGoldenIO { unWrappedGoldenIO :: GoldenIO a }
```

Potrzebujemy jeszcze tylko implementacji czyli instancji

```haskell  
instance Eq str => Example (WrappedGoldenIO str) where
  type Arg (WrappedGoldenIO str) = ()
  evaluateExample wrapped params action callback = evaluateExample' =<< unWrappedGoldenIO wrapped where
    evaluateExample' golden = evaluateExample golden params action callback
```

BTW to co widzimy powyrzej to chyba rodziny typów (ang. Family Types)

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

## PS udao mi sicakowicie pozbyć `do natation` z kodu produkcyjnego

```haskell
instance WrapperIO m => Evaluator (m ()) where
  doEnd = pass

  doInputChar iu s = doInputChar' =<< wGetChar where
    doInputChar' char = next iu $ push1 (ord char) s

  doOutputChar iu s = wPutChar (chr symbol) *> next iu s' where (symbol, s') = pop1 s
```


```bash
Slow examples:
1.010878967s: hs/test/HelVM/HelMA/Automata/ETA/EvaluatorSpec.hs[28:9]
        interact/SeqStackType/bottles
1.213947573s: hs/test/HelVM/HelMA/Automata/ETA/EvaluatorSpec.hs[30:9]
        monadic/SeqStackType/bottles

Finished in 11.3053 seconds
```

```bash
1.128178606s: hs/test/HelVM/HelMA/Automata/ETA/EvaluatorSpec.hs[70:9]
        interact/ListStackType/bottles
1.083167682s: hs/test/HelVM/HelMA/Automata/ETA/EvaluatorSpec.hs[72:9]
        monadic/ListStackType/bottles
1.04183862s: hs/test/HelVM/HelMA/Automata/ETA/EvaluatorSpec.hs[74:9]
        logging/ListStackType/bottles
1.266628515s: hs/test/HelVM/HelMA/Automata/ETA/EvaluatorSpec.hs[70:9]
        interact/SeqStackType/bottles
1.120756983s: hs/test/HelVM/HelMA/Automata/ETA/EvaluatorSpec.hs[72:9]
        monadic/SeqStackType/bottles
1.118947099s: hs/test/HelVM/HelMA/Automata/ETA/EvaluatorSpec.hs[74:9]
        logging/SeqStackType/bottles

Finished in 24.1430 seconds
1252 examples, 0 failures
```



