---
title:    'Haskell i klasy typów `Show` oraz `Read`'
author:   TheKamilAdam
category: haskell-eta
tags:     adt compiler interpreter lexer type-class
langs:    haskell java kotlin scala
libs:     hunit
tools:    cabal etlas
projects: helcam helvm
eso:      brainfuck
redirect_from:
  - type-class
  - haskell-eta/type-class
---

Wiele osób zadaje takie pytanie:
> Co napisać w nowym języku programowania, żeby się go nauczyć

Kiedyś usłyszałem odpowiedź,
że jako pierwszy projekt w nowym języku programowania najlepiej napisać [interpreter] [BrainFucka].
[BrainFucka] nie trzeba chyba przedstawiać.
Jest to najpopularniejszy ezoteryczny język programowania.
Przy tym jest bardzo prosty,
ale ma też ciekawe właściwości.
Interpreter [BrainFucka] będzie częścią projektu [HelCam],
a [HelCam] częścią większego projektu [HelVM] (wymawiaj Helium).

Na razie nie będziemy pisać całego interpretera a ograniczymy się do [leksera].
[Lekser] jest to program zamieniający kod źródłowy na tokeny.
Tokeny są przetwarzane później przez kolejne części kompilatora,
czyli np. [parser].
[Lekser] zmienia kod źródłowy zapisany w **[BrainFucku]** na listę tokenów,
a następnie tę listę tokenów znów możemy zamienić na kod źródłowy i porównać,
że dostaliśmy to samo.
A dokładniej zminimalizowany kod bez komentarzy.

## Haskell i programowanie funkcyjne

Można pomyśleć,
że w programowaniu funkcyjnym tworzy się bez ładu i składu funkcje,
które szybko zamieniają się w Latającego Potwora Spaghetti.

My jednak nie będziemy pisać funkcji (z jednym małym wyjątkiem),
tylko będziemy implementować klasy typu (ang. [type class]).

## Typ danych `Token`

Najpierw zdefiniujmy tym danych `Token`:
```haskell
data Token = MoveR
           | MoveL
           | Inc
           | Dec
           | Output
           | Input
           | JmpPast
           | JmpBack
           deriving (Eq, Ord, Enum)
```
Osiem tokenów odpowiadających ośmiu instrukcjom **[BrainFucka]**.

Słowo kluczowe `data` służy do tworzenia typów danych.
Zarówno iloczynów (ang. *product*), jak i sum (ang. *sum*, *coproduct*).
Teraz tylko pytanie, co to jest iloczyn i suma :)

Iloczyn jest to iloczyn kartezjański zmiennych,
czyli krotka/tupla (ang. *tuple*) lub struktura/rekord.
W **[Kotline]** służą do tego klasy danych (ang. *data class*),
a w **[Scali]** - klasy przypadków (ang. *case class*)
Tu trzeba uważać,
ponieważ w **[Haskellu]** struktury nie mają nazwanych pól (wyglądają prawie jak krotki),
za to rekordy, które mają nazwane pola, są niepolecane z powodu dziwnych zachowań.
Najprostszym możliwym iloczynem, z którego można zbudować wszystkie pozostałe, jest para,
czyli krotka dwuelementowa.

Suma to alternatywa wyłączająca (albo-albo).
Czyli coś jest albo typu A, albo typu B.
Najprostsza możliwa suma, z której można zbudować wszystkie pozostałe sumy, to `Either`.

Razem iloczyn i suma tworzą algebraiczne typy danych (ang. *algebraic data type*, **[ADT]**).

Na razie jednak nie potrzebujemy pełnej mocy algebraicznych typów danych,
a prostego enuma,
takiego jak w **[Javie]**.

`Eq, Ord, Enum` to klasy typów, które potrafi wywnioskować kompilator **[Haskella]**:
* `Eq` to możliwość sprawdzania, czy zmienne są sobie równe (operatory `==` i `\=`).
* `Ord` to możliwość ustalania kolejności między zmiennymi (operatory `<`, `<=`, `>` i `>=` )
* `Enum` to możliwość enumeracji, czyli znalezienia następnika i poprzednika

Tak naprawdę z tych trzech klas typów potrzebujemy tylko `Eq`,
ale chciałem pokazać,
że możemy dostać za darmo wiele instancji klas typów.

Następnie tworzymy jeszcze alias `TokenList` dla listy tokenów.
```haskell
type TokenList = [Token]
```

## Klasy typów `Show` i `Read`

Oprócz klasy typów `Eq`,` Ord` i `Enum` potrzebujemy jeszcze dwóch innych implementacji klas typów z biblioteki standardowej,
są to `Show` i `Read`.

`Show` także posiada domyślną implementację,
jednak nie jest tu ona satysfakcjonująca.
Dlatego,
żeby móc używać klasy typu `Show` musimy napisać już trochę kodu.
Musimy zaimplementować jedną metodę `show` i zrobimy to za pomocą dopasowywania wzorców (ang. **[pattern matching]**):
```haskell
instance Show Token where
  show MoveR   = ">"
  show MoveL   = "<"
  show Inc     = "+"
  show Dec     = "-"
  show Output  = "."
  show Input   = ","
  show JmpPast = "["
  show JmpBack = "]"
```
Dla każdej możliwej wartości tworzymy osobny przypadek,
gdzie zwracamy `Stringa` reprezentującego token.

Żeby utworzyć instancję klasy typu `Read` trzeba zaimplementować jedną metodę `readsPrec`.
Teraz także zrobimy to za pomocą dopasowywania wzorców:
```haskell
instance Read Token where
  readsPrec _ ">" = [( MoveR  , "")]
  readsPrec _ "<" = [( MoveL  , "")]
  readsPrec _ "+" = [( Inc    , "")]
  readsPrec _ "-" = [( Dec    , "")]
  readsPrec _ "." = [( Output , "")]
  readsPrec _ "," = [( Input  , "")]
  readsPrec _ "[" = [( JmpPast, "")]
  readsPrec _ "]" = [( JmpBack, "")]
  readsPrec _ _   = []
```

Mamy tu kilka rzeczy:
* dopasowywanie wzorców dla dwóch zmiennych
* `_` to ignorowanie wartości zmiennej (metoda `readsPrec` przyjmuje dwa parametry, a nas interesuje tylko ten drugi).
* `[,]` to składnia listy.
* `(,)` to składnia krotki.

Implementujemy jedną metodę `readsPrec`,
ale w zamian za to dostajemy wiele innych metod do użycia:
* `read` - niebezpieczna metoda,
która zgłasza błąd,
jeśli nie uda się parsowanie.
* `readMaybe` - bezpieczna metoda,
która zwraca `Maybe` będące odpowiednikiem `Option` ze **[Scali]** i `Optional` z **[Javy]**.
* `readEither` - bezpieczna metoda,
która zwraca `Either`.

## Nowy typ `Tokens`

Klasy typów niestety mają swoje ograniczenia.
Nie jesteśmy w stanie utworzyć klas typów `Show` i `Read` dla listy tokenów.
Rozwiązaniem na to jest utworzenie nowego typu danych zawierającego listę tokenów:
```haskell
import Data.Maybe
import Text.Read

newtype Tokens = Tokens TokenList
```

I znów tworzymy nowy typ danych.
Jednak tym razem nie za pomocą `data` a `newtype`.
Jaka jest różnica między tymi słowami?
`newtype` nie tworzy dodatkowej alokacji.
Działa jak `case class Tokens(tokens: TokenList) extends AnyVal` w **[Scali]**.

Instancja dla `Show` jest prosta do zaimplementowania:
```haskell
instance Show Tokens where
  show (Tokens tokens) = tokens >>= show
```
`>>=` to operator składania monad.
Odpowiednik `flatMap` z innych języków programowania.
W naszym przypadku monadami jest lista tokenów i lista znaków wyprodukowanych przez metodę `show` wywoływaną dla każdego tokenu.
W rezultacie dostajemy listę znaków, czyli `String`.

W drugą stronę jest niestety trudniej:
```haskell
charToString :: Char -> String
charToString = (:[])

instance Read Tokens where
  readsPrec _ text = [( Tokens $ text >>= maybeToList . readMaybe . charToString, "")]
```

* `.` to operator składania funkcji.
* `$` to sprytny operator zmieniania kolejności operacji.
Dzięki niemu [wywołania cebulowe] `d(c(b(a)))` można zapisać jako `d $ c $ b $ a`.

Dla każdego znaku w liście kolejno musimy:
* zamienić `char` na `string` za pomocą `charToString`
* sparsować każdy string w liście za pomocą metody `readMaybe`
* każde `Maybe` zamienić na listę zero- lub jedno-elementową za pomocą funkcji `maybeToList`

W rezultacie dostajemy listę tokenów,
którą zamieniamy na typ `Tokens`.

## Budowanie projektu - Cabal

[Cabal] jest najprostszym programem do budowania projektów i zarządzania zależnościami dla **[Haskella]**.
Pozostałe to Stack i Nix.
Początkowy plik konfiguracyjny można wygenerować za pomocą polecenia `cabal init`.
Plik konfiguracyjny nazywa się `<nazwa-projektu>.cabal`,
czyli w naszym przypadku `helcam.cabal`.

Do pliku projektu `helcam.cabal` musimy dodać informacje o modułach do kompilacji:
```cabal
library
  exposed-modules:
      HelVM.HelCam.BrainFuck.Token
      HelVM.HelCam.BrainFuck.Tokens
  other-modules:
  hs-source-dirs:
      src/main/eta
  build-depends:
      base >=4.7 && <5
  default-language: Haskell2010
  ghc-options:      -Wall
```

i teraz możemy skompilować projekt za pomocą polecenia:
```bash
cabal clean && cabal build
```

## Pisanie testów  i biblioteka HUnit

Teraz trzeba by jakoś ten kod przetestować.
Można manualnie,
ale nie po to jesteśmy programistami,
żeby rzeczy robić manualnie.
Na szczęście dla **[Haskella]** istnieje prosta biblioteka,
która umożliwia testy jednostkowe.

do pliku `hs-esovm.cabal` dodajemy konfigurację:
```cabal
test-suite hs-esovm-test
  type: exitcode-stdio-1.0
  main-is: Test.hs
  other-modules:
  hs-source-dirs:
      src/test/eta
  ghc-options: -threaded -rtsopts -with-rtsopts=-N
  build-depends:
      base >=4.7 && <5
    , hs-esovm
    , HUnit
    , test-framework
  default-language: Haskell2010
```

i już możemy pisać testy

```haskell
helloWorldWithComments :: String
helloWorldWithComments = "                                \
++++++++                                                  \
[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]                 \
>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++. \
"

helloWorld :: String
helloWorld = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

helloWorldAsList :: String
helloWorldAsList = "[+,+,+,+,+,+,+,+,[,>,+,+,+,+,[,>,+,+,>,+,+,+,>,+,+,+,>,+,<,<,<,<,-,],>,+,>,+,>,-,>,>,+,[,<,],<,-,],>,>,.,>,-,-,-,.,+,+,+,+,+,+,+,.,.,+,+,+,.,>,>,.,<,-,.,<,.,+,+,+,.,-,-,-,-,-,-,.,-,-,-,-,-,-,-,-,.,>,>,+,.,>,+,+,.]"

--------------------------------------------------------------------------------

testHelloWorld :: Test
testHelloWorld = TestCase (assertEqual "testHelloWorld" helloWorld (show $ readTokens helloWorld))

testHelloWorldWithComments :: Test
testHelloWorldWithComments = TestCase (assertEqual "testHelloWorldWithComments" helloWorld (show $ readTokens helloWorldWithComments))

testTokenAsList :: Test
testTokenAsList = TestCase (assertEqual "testTokenAsList" helloWorldAsList (show $ tokenList $ readTokens helloWorldWithComments))
```

Następnie wszystkie testy trzeba zebrać razem:
```haskell
testsOfTokens :: Test
testsOfTokens = TestList [ TestLabel "testHelloWorld" testHelloWorld
                          , TestLabel "testHelloWorldWithComments" testHelloWorldWithComments
                          , TestLabel "testTokenAsList" testTokenAsList
                          ]
```

Oraz stworzyć moduł startowy dla testów:
```haskell
module Main(main) where

import HelVM.HelCam.BrainFuck.TokensTest

import Test.HUnit

testExample :: Test
testExample = TestCase (assertEqual "test" "test" "test")

testList :: Test
testList = TestList [ TestLabel "testExample" testExample
                    , TestLabel "testsOfTokens" testsOfTokens
                    ]

main :: IO ()
main = do
  _ <- runTestTT testList
  return ()
```

Teraz możemy wywołać testy za pomocą polecenia:
```bash
cabal test
```

## Wyprowadzenie i podsumowanie

No ale po co programiście **[Javy]**, **[Kotlina]** czy **[Scali]** język **[Haskell]**?
Oczywiście oprócz tego,
że **[Haskell]** uczy programowania funkcyjnego,
które jest coraz bardziej potrzebne.
Przecież tego i tak nie można użyć w projekcie działającym na **[JVM]**.

Otóż od niedawna można.
Można skompilować **[Haskella]** i uruchomić go na [JVMie].
Kompilator, który to robi nazywa się **[Eta]** a odpowiednikiem [Cabala] jest [Etlas].

Mając zainstalowany kompilator [Eta] i narzędzie do budowania [Etlas] można skompilować projekt za pomocą polecenia:
```bash
etlas clean && etlas build && etlas test
etlas run helcam
```

Kod jest dostępny na [Githubie](https://github.com/helvm/helcam/releases/tag/v0.2.0.0).

[Eta]:                /langs/eta
[Haskell]:            /langs/haskell
[Haskella]:           /langs/haskell
[Haskellu]:           /langs/haskell
[Java]:               /langs/java
[Javie]:              /langs/java
[Javy]:               /langs/java
[Kotlin]:             /langs/kotlin
[Kotlina]:            /langs/kotlin
[Scala]:              /langs/scala
[Scali]:              /langs/scala

[Cabal]:              /tools/cabal
[Cabala]:             /tools/cabal
[Etlas]:              /tools/etlas
[JVM]:                /tools/jvm
[JVMie]:              /tools/jvm

[helcam]:             /projects/helcam
[helvm]:              /projects/helvm

[BrainFucka]:         /eso/brainfuck

[interpreter]:        /tags/interpreter
[lekser]:             /tags/lexer
[leksera]:            /tags/lexer
[pattern matching]:   /tags/pattern-matching
[parser]:             /tags/parser
[type class]:         /tags/type-class

[wywołania cebulowe]: /scalapipe
