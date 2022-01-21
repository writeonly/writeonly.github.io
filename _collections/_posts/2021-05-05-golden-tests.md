---
title:    'Złote testy w Haskellu'
author:   TheKamilAdam
category: haskell-eta
langs:    haskell
libs:     attoparsec hspec hunit taste
projects: helma helpa helvm
eso:      brainfuck eas eta wsa
tags:     framework monad testing
redirect_from:
- golden-tests
- haskell-eta/golden-tests
---

Zainspirowany wpisem o złotych testach na [4programmers](https://4programmers.net/Mikroblogi/View/90241) postanowiłem dodać je do swojego projektu w **[Haskellu]**.

Dlaczego w ogóle złote testy (ang. **golden tests**)?
Złote testy są dobre dla legacy projektów,
gdzie nie wiemy,
co zwrócą testowane funkcje.
Ja,
pisząc od początku nowy kod,
powinienem dobrze wiedzieć co i kiedy może zostać zwrócone.
Jednak tak nie jest.
O ile tak jest dla prostych przypadków,
o tyle dla długich fragmentów kodu w ezoterycznych asemblerach jak [EAS] czy [WSA] nie mam pojęcia co zostanie wygenerowane.
Tutaj idealnie sprawdzają się złote testy.

Niestety [HUnit] nie wspiera złotych testów, 
ale już wcześniej byłem zdecydowany na migrację do frameworka testowego [HSpec].
Jednak nie sądziłem,
że zmiana będzie od razu tak radykalna.
Oprócz [HSpec], także framework [Taste] pozwala na używanie złotych testów.
Jednak zdecydowałem się na HSpec ponieważ:
* HSpec posiada automatyczne generowanie agregatora testów.
* HSpec ma zagnieżdżoną składnię `describe`/`context`/`it`, którą można ładnie wypaczać.

Jeśli jednak ktoś wolałby złote testy we frameworku [Taste] znalazłem dwa teksty poświęcone temu zagadnieniu:
* [Introduction to golden testing](https://ro-che.info/articles/2017-12-04-golden-tests)
* [Golden tests are tasty](https://kseo.github.io/posts/2016-12-15-golden-tests-are-tasty.html)

A jako przykład użycia polecam projekt [Husk Schema](https://justinethier.github.io/husk-scheme/).

## Złote testy w projekcie HelPA

Jako prosty przykład do testów wybrałem asembler [EAS] z projektu [HelPA].

Asembler [EAS] składa się z czterech głównych modułów:
* `AsmParser` - frontend asemblera, który parsuje plik z językiem asemblerowym.
* `Reducer` - frontend backendu asemblera, który redukuje skomplikowane instrukcje do prostych instrukcji.
* `CodeGenerator` - właściwy backend asemblera, który generuje kod w języku ezoterycznym.
* `Assembler` - moduł, który składa to wszystko razem.

A więc po kolej.

### ReducerSpec, czyli parametryzowane testy 

Moduł `ReducerSpec` testuje funkcję `Reducer.reduce`.
Funkcją `Reducer.reduce :: InstructionList -> InstructionList` zamienia listę instrukcji w zredukowaną listę instrukcji.
Redukcja polega na zamianie `wysokopoziomowych` instrukcji na ich `niskopoziomowe` odpowiedniki możliwe do zapisania w języku [ETA].

Test wygląda następująco:
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

Funkcja `forM_` zamienia nam zwykłe testy na testy parametryczne.

Następnie mamy listę krotek.
Pierwszy element krotki zawiera nazwę testu,
drugi — listę instrukcji do zredukowania,
trzeci — zredukowaną listę instrukcji.

Funkcja `shouldBe` to asercja.
Dzięki grawisom funkcja może być użyta jak operator.
Bez grawisów trzeba by zapisać:
```haskell
      it fileName $ do shouldBe (reduce ilLinked) ilReduced
```

### AsmParser, czyli czytanie danych testowych z pliku

Moduł `AsmParserSpec` testuje funkcję `AsmParser.parseAssembler`.
Funkcja `parseAssembler :: Text -> Parsed InstructionList` parsuje plik w języku [EAS] i zwraca listę instrukcji.
Ponieważ parsowanie może się nie udać to lista instrukcji opakowana jest w typ `Parsed`, który ma postać:
```haskell
type Parsed a = Either String a
```

Ponieważ jednak nie będziemy pracować ze zmiennej typu `Text`,
a zmienną typu `IO Text` to naszym ostatecznym typem do porównania będzie `IO (Parsed InstructionList)`,
czyli dokładniej `IO (Either String InstructionList)`. 
Który dla wygody nazwiemy `ParsedIO`:
```haskell
type ParsedIO a = IO (Parsed a)
```

Biblioteka [HSpec] nie posiada oczywiście asercji dla typu `IO (Either String a)`,
ale posiada asercję `shouldReturn` dla typu `IO a`:
```haskell
shouldReturn :: (HasCallStack, Show a, Eq a) => IO a -> a -> Expectation
```
Jedyne co musimy zrobić to tylko zamienić `IO (Either String a)` na `IO a`.

Najpierw zamieniamy `Either String a` na `IO a`
```haskell
eitherToIO :: Parsed a -> IO a
eitherToIO (Right value)  = pure value
eitherToIO (Left message) = fail message
```

A następnie możemy dodać do tego składanie monad (flatMapowanie) z `IO (IO a)` na `IO a` 
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

`infix 1` pozwala ustalić priorytet operatora.

Ostatecznie test wygląda następująco:
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

### CodeGeneratorSpec, czyli złote testy

Moduł `CodeGeneratorSpec` testuje funkcję `CodeGenerator.generateCode`.
Funkcja `generateCode :: InstructionList -> String` generuje kod w języku [ETA] na podstawie listy instrukcji. 
Wyniku wygenerowanego z `generateCode` nie będziemy porównywać z wartościami zapisanymi w testach tylko ze złotym plikiem.

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

Kod testu wygląda następująco:
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
drugi element krotki to lista instrukcji ze zredukowanymi rozkazami.
Po lewej stronie asercji mamy generowanie kodu.
Po prawej stronie asercji mamy wczytanie pliku z kodem źródłowym w **[ETA]**.

### AssemblerSpec, czyli złote testy z czytaniem danych testowych z pliku

Pora na przetestowanie wszystkiego razem.
Moduł `AssemblerSpec` testuje funkcję `Assembler.assembleFile`.
Funkcja `assembleFile :: SourcePath -> ParsedIO String` generuje kod w języku [ETA] na podstawie zmiennej `SourcePath`.
Typ `SourcePath` ma postać:
```haskell
data SourcePath = SourcePath
  { dirPath :: String  -- ścieżka do folderu z bibliotekami z kodem w EAS
  , filePath :: String -- ścieżka do pliku z kodem w EAS
  }
```

Ponieważ funkcja `Assembler.assembleFile` zwraca monadę `IO` potrzebujemy asercji działającej dla typu `IO (Golden String)`.

W celu prostszego zapisu tworzymy alias typu:
```haskell
type GoldenIO a = IO (Golden a)
```

A następnie asercję:
```haskell
goldenShouldReturn' :: IO String -> String -> GoldenIO String
goldenShouldReturn' actualOutputIO fileName = flip goldenShouldBe fileName <$> actualOutputIO
```
Jednak ta asercja nie zadziała z powodu niezgodności typów:
```haskell
hs/test/HelVM/HelPA/Assemblers/EAS/AssemblerSpec.hs:39:7: error:
    • Couldn't match type ‘Arg
                             (HelVM.HelPA.Assemblers.Expectations.GoldenIO String)’
                     with ‘()’
      Expected type: hspec-core-2.8.2:Test.Hspec.Core.Spec.Monad.SpecM
                       () ()
        Actual type: SpecWith
                       (Arg (HelVM.HelPA.Assemblers.Expectations.GoldenIO String))
    • In a stmt of a 'do' block:
        it fileName
          $ do assemble
                 `goldenShouldParseReturn`
                   buildAbsolutePathToEtaFile ("assembleFile" </> fileName)
      In the expression:
        do let assemble = assembleFile ...
           it fileName
             $ do assemble
                    `goldenShouldParseReturn`
                      buildAbsolutePathToEtaFile ("assembleFile" </> fileName)
      In the second argument of ‘($)’, namely
        ‘\ fileName
           -> do let ...
                 it fileName $ do ...’
   |
39 |       it fileName $ do assemble `goldenShouldParseReturn` buildAbsolutePathToEtaFile ("assembleFile" </> fileName)
   |       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

Dlaczego?
Otóż:
* Normalne asercje zwracają typ `Expectation`, który jest aliasem dla typu `IO ()`.
* Złote testy zwracają - `Golden a`.
* Nasze testy zwracają - `Golden (IO String)`.

Problemem jest brak instancji (implementacji) klasy typu `Example` dla `Golden (IO String)`.
Dlatego spróbujmy ją napisać:
```haskell
instance Eq str => Example (GoldenIO str) where
  type Arg (GoldenIO str) = ()
  evaluateExample wrapped params action callback = evaluateExample' =<< unWrappedGoldenIO wrapped where
    evaluateExample' golden = evaluateExample golden params action callback
```
BTW to, co widzimy powyżej to chyba rodziny typów (ang. [Type Family])

Niestety to także nie zadziała i dostaniemy błąd:
```haskell
hs/test/HelVM/HelPA/Assemblers/Expectations.hs:87:1: error: [-Worphans, -Werror=orphans]
    Orphan instance: instance Eq str => Example (GoldenIO str)
    To avoid this
        move the instance declaration to the module of the class or of the type, or
        wrap the type with a newtype and declare the instance on the new type.
   |
87 | instance Eq str => Example (GoldenIO str) where
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^...
```

Wynika to z tego,
że w tej chwili mamy osieroconą instancję klasy typu `Example`.
Instancje klas typów dla typu danych możemy tworzyć tylko w modułach:
* gdzie zdefiniowana jest klasa typu;
* gdzie zdefiniowany jest typ danych.

Ponieważ nie mamy wpływu na klasę typu to musimy utworzyć nowy typ danych opakowujący typ `GoldenIO`:
```haskell
newtype WrappedGoldenIO a = WrappedGoldenIO { unWrappedGoldenIO :: GoldenIO a }
```

Nową asercję:
```haskell
infix 1 `goldenShouldReturn`
goldenShouldReturn :: IO String -> String -> WrappedGoldenIO String
goldenShouldReturn actualOutputIO = WrappedGoldenIO . goldenShouldReturn' actualOutputIO
```

Oraz nową instancji klasy typu `Example`:
```haskell
instance Eq str => Example (WrappedGoldenIO str) where
  type Arg (WrappedGoldenIO str) = ()
  evaluateExample wrapped params action callback = evaluateExample' =<< unWrappedGoldenIO wrapped where
    evaluateExample' golden = evaluateExample golden params action callback
```

Dzięki temu możemy napisać złoty test end-to-end:
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
          , "multiply"
          , "fact"
          , "bottles"
          , "euclid"
          ] $ \fileName -> do
      let assembleFile = assembly SourcePath {dirPath = easDir, filePath = buildAbsolutePathToEasFile fileName}    
      it fileName $ do assembleFile `goldenShouldParseReturn` buildAbsolutePathToEtaFile fileName
```

## Testy jednostkowe kontra testy integracyjne

Po tym wszystkim rodzą się dwa pytania:
* Co z piramidą testów i testami jednostkowymi?
* Na ile to jest szybkie?

### Gdzie testy jednostkowe i piramida testów?

Trochę offtop, ale IMHO ta cała piramida testów (i odwrócona piramida testów) to pic na wodę. 
Dlaczego o tym mówimy? 
Bo pokazywali nam to na konferencjach. 
A czemu nam to pokazywali? Bo piramida ładnie wygląda na slajdach.
Chyba widziałem wszystkie możliwe ułożenia testów (z wyjątkiem piramid):
* Pracowałem w firmach,
  gdzie istniały tylko testy manualne.
* Widziałem firmę,
  gdzie istniały tylko testy manualne i jednostkowe, bo testerzy nie mieli czasu pisać testów systemowo-akceptacyjnych.
* Pracowałem w firmie,
  gdzie nie dało się powiedzieć czy jest więcej jednostkowych czy systemowo-akceptacyjnych,
  bo programiści pisali swoje testy, a testerzy swoje.
* Pracowałem w firmie,
  gdzie była niechęć do testów jednostkowych, a programiści i testerzy wspólnie pisali testy integracyjno-akceptacyjne.

Co do samych definicji to nie widziałem żadnego porządnego papieru,
który określałby co to jest jednostka. 
Metoda/funkcja? 
Klasa/moduł?
Pakiet?
Mikroserwis?
Mikroserwis z własną bazą danych?
Wszystkie te sprzeczne definicje można spotkać na konferencjach.
Niektórzy wprost mówią,
że definicje się zmieniły odkąd mamy mikroserwisy.
Jeśli ktoś ma uznany papier z porządną definicją to z chęcią przeczytam.

Które testy osobiście uważam za najlepsze?
Te które są szybkie, ale jednocześnie testują maksymalnie dużo kodu.
Dla mnie takimi testami dla większości aplikacji webowych są testy na poziomie mikroserwisu z prawdziwą bazą danych postawioną w dockerze.
Jeśli czegoś w prosty sposób nie da się postawić w dockerze to mockuję.
Albo na poziomie http, albo dostarczam alternatywną implementację klienta.

Tutaj jednak, na szczęście, nie mamy aplikacji webowej z http i bazą danych.
Mamy aplikację pracującą na plikach i to na plikach powinniśmy ją testować.

Nie mówię,
że testy jednostkowe są całkiem złe.
Testy jednostkowe były dla mnie przydatne na początku pisania.
Ale teraz małe testy jednostkowe są spowalniaczem przy refaktoryzacji.

### Czas, czyli czy to nie jest za wolne.

Jeśli rezygnujemy z testów jednostkowych na rzecz testów integracyjnych to najważniejsze jest pytanie o czas.
Czy testy integracyjne nie wykonują się za wolno?

Z pomocą przychodzi nam tu biblioteka `hspec-slow`.
Pozwala ona mierzyć czas wykonywania pojedynczych przypadków testowych.

Przy założeniu,
że punkt wejściowy dla testów był w pliku `hs/test/Spec.hs`,
tworzymy plik `hs/test/Main.hs`,
który będzie nowym punktem wejściowym dla testów:

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

Jednocześnie,
jeśli chcemy dalej automatycznie generować agregator dla wszystkich testów,
musimy zmienić plik `hs/test/Spec.hs` na
```haskell
{-# OPTIONS_GHC -F -pgmF hspec-discover -optF --module-name=Spec #-}
```

W projekcie [HelPA] nie ma żadnych testów dłuższych niż jedna sekunda.
Jednak w projekcie [HelMA] kilka takich testów się znalazło: 
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
```
W zasadzie jest to jeden test wywoływany sześciokrotnie z różnymi parametrami.

Całość testów trochę trwa:
```
Finished in 24.1430 seconds
```
Ilość testów jest jednak spora:
```
1252 examples, 0 failures
```

Rozwiązaniem może być pozbycie się części testów.
W tej chwili testuję wszystkie kombinacje parametrów na wszystkich przykładowych programach w językach ezoterycznych.
Drugim rozwiązaniem może być podzielenie testów na dwa zestawy:
* Szybko wykonujące się testy dymne (ang. smoke test)
* Pozostałe testy


## Złote testy - czy warto?

Krótko - warto.
Kod testów się skrócił,
ponieważ wartości oczekiwane do testów zostały przeniesione do złotych plików.
Jednocześnie zlikwidowało to przymus używania znaków ucieczki do zapisywania znaku końca linii.
Oraz rozwiązało to problem,
że niektóre skrypty w [BrainFucku] są zgodne z Windowsem, a nie Linuksem



Kod asemblera **[HelPA]** po zmianach znajduje się na [githubie](https://github.com/helvm/helpa/tree/v0.3.2.0).
Podobnie jak kod interpretera **[HelMA]** po zmianach znajduje się na [githubie](https://github.com/helvm/helma/tree/v0.6.5.0).

[Haskell]:      /langs/haskell
[Haskellu]:     /langs/haskell

[HelMA]:        /projects/helma
[HelPA]:        /projects/helpa

[AttoParsec]:   /libs/attoparsec
[HSpec]:        /libs/hspec
[HSpec.Golden]: /libs/hspec-golden
[HSpec-Slow]:   /libs/hspec-slow
[HUnit]:        /libs/hunit
[Taste]:        /libs/taste

[BrainFucku]:   /eso/brainfuck
[EAS]:          /eso/eas
[ETA]:          /eso/eta
[WhiteSpace]:   /eso/whitespace
[WSA]:          /eso/wsa

[Applicative]:  /tags/applicative
[Do notation]:  /tags/do-notation
[Functor]:      /tags/functor
[Monad]:        /tags/monad
[Type Class]:   /tags/type-class
[Type Family]:  /tags/type-family
