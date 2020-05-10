---
title:    'Testy jednostkowe i mockowanie wyścia-wejścia w Haskellu'
author:   TheKamilAdam
category: haskell-eta
tags:     io monad state testing type-class unit-testing
langs:    eta haskell
libs:     hunit
tools:    cabal etlas
projects: helma helvm
redirect_from:
  - state-monad
  - haskell-eta/state-monad
---

Testy pisać trzeba, to oczywiste.
Pozostaje pytanie `jednostkowe czy integracyjne?`
Ja na razie zdecydowałem się na jednostkowe,
co postawiło mnie przed kolejnym pytaniem `jak przetestować kod używający wyjścia-wejścia w Haskellu?`
To znowu sprowadza się do pytania `jak zamockować monadę IO w Haskellu?`

Poglądowo spójrzmy na kod zawierający odczyt i zapis ze standardowych strumieni napisany w języku **[Haskell]**:

```haskell
pipe :: IO ()
pipe = do
  char <- getChar
  putChar char
  pipe
```
Ten program to proste echo.
Odczytuje jeden znak ze standardowego wejścia i wypisuje go na standardowe wyjście,
po czym ponawia w nieskończoność za pomocą rekurencji.
Za odczyt odpowiedzialna jest funkcja `getChar` a za wypisanie - `putChar`.
Co ciekawe na [EsoLangs] nazywają taki program [Cat].

Ponieważ nie ma tu za wiele do testowania,
spójrzmy na bardziej skomplikowaną wersję.

```haskell
filterIf0 :: IO ()
filterIf0 = do
  char <- getChar
  if char == '0'
    then putChar '\n'
    else do
      putChar char
      filterIf0
```
Tutaj odczyt jest przerywany,
jeśli zostanie wczytany znak '0'.
Dodatkowo jest na końcu dodawany znak nowej linii dla lepszego działania w konsoli.

Niestety powyższy kod jest nietestowalny w sposób jednostkowy.
Dla testów jednostkowych najlepiej by było,
gdyby kod wyglądał następująco:
```haskell
listFilterIf0 :: [Char] -> [Char]
listFilterIf0 []          = []
listFilterIf0 (char:rest) =
  if char == '0'
    then ['\n']
    else char : listFilterIf0 rest
```

Niestety takiego kodu nie można używać w sposób interaktywny.

## Funkcje wyższego rzędu (ang. *Higher-order functions*)

Rozwiązaniem jest przekazywanie funkcji odczytujących i zapisujących jako parametry.
Dzięki czemu dla testów będziemy mogli przekazać zamockowaną implementację.

Najpierw sprawdźmy jaki typ mają funkcje które nas interesują:
```haskell
getChar :: IO Char
putChar :: Char -> IO ()
```

Następnie utwórzmi aliasy dla tych typów:
```haskell
type IOGetChar = IO Char
type IOPutChar = Char -> IO ()
```

Nasz produkcyjny kod będzie wyglądać następująco:
```haskell
ioFilterIf0 :: IO ()
ioFilterIf0 = ioFilterIf0' getChar putChar
```

A funkcja,
którą będziemy testować jednostkowy,
będzie miała postać:
```haskell
ioFilterIf0' :: IOGetChar -> IOPutChar -> IO ()
ioFilterIf0' ioGetChar ioPutChar = do
  char <- ioGetChar
  if char == '0'
    then ioPutChar '\n'
    else do
      ioPutChar char
      ioFilterIf0' ioGetChar ioPutChar
```

Niestety powyższy kod dalej jest nietestowalny jednostkowo.
Ponieważ nie jesteśmy w stanie napisać zamockowanych implementacji `getChar` i `putChar`.

## Kod zależny od interfejsu, a nie od implementacji

Na początek musimy rozluźnić trochę typy,
żeby zależały od *interfejsu* a nie od *implementacji*.
```haskell
type MGetChar m = m Char
type MPutChar m = Char -> m ()
```
W naszym przypadku *interfejsem* jest klasa typów `Monad` czyli [monada],
a *implementacją* - monada [IO].

Teraz nasza funkcja,
którą będziemy,
testować wygląda następująco:
```haskell
mFilterIf0 :: Monad m => MGetChar m -> MPutChar m -> m ()
mFilterIf0 mGetChar mPutChar = do
  char <- mGetChar
  if char == '0'
    then mPutChar '\n'
    else do
      mPutChar char
      mFilterIf0 mGetChar mPutChar
```
Kod produkcyjny dużo się nie zmienił:
```haskell
ioMFilterIf0 :: IO ()
ioMFilterIf0 = mFilterIf0 getChar putChar
```

## Testy jednostkowe i monada State
Na potrzeby testów potrzebujemy strukturę,
która będzie zastępować wejście-wyjście:
```haskell
data MockIO = MockIO { input :: String, output :: String }
  deriving (Eq, Show)

createMockIO :: String -> MockIO
createMockIO input = MockIO (input) []

getOutput :: MockIO -> String
getOutput (MockIO input output) = reverse output
```

Następnie możemy stworzyć nasze zamockowane funkcje:
```haskell
mockGetChar :: MockIO Char
mockGetChar = do
  state <- get
  let char = head $ input state
  put $ state { input = tail $ input state }
  return char

mockPutChar :: Char -> MockIO ()
mockPutChar char = do
  state <- get
  put $ state { output = char : output state }
```

Przyda się też funkcja konwertująca naszą monadę `State` na funkcję `String -> String`:
```haskell
execMockIO :: MockIO () -> String -> String
execMockIO mockIO input = getOutput $ execState mockIO $ createMockIO input
```

Ostatecznie nasz test wygląda następująco:
```haskell
testsOfFilterIf0 :: Test
testsOfFilterIf0 = test
  [ "testFilter0"  ~: "test FilterIf0"  ~: "qwerty\n" ~=? execMockIO (mFilterIf0 mockGetChar mockPutChar) "qwerty0uiop"
  ]
```

## Klasy typów

W tym przykładzie mieliśmy do zamokowania tylko dwie funkcje.
Jeśli jednak byłoby ich więcej, np. sześć jak poniżej, mogłoby to zacząć być problematyczne.
Na szczęście w **[Haskellu]** można grupować funkcje za pomocą interfejsów a dokładniej za pomocą [klas typów].

```haskell
class Monad m => WrapperIO m where
  wGetChar  :: m Char
  wPutChar  :: Char -> m ()
  wGetLine  :: m String
  wPutStr   :: String -> m ()
  wPutStrLn :: String -> m ()
  wFlush    :: m ()
  wPutStrLn s = wPutStr $ s ++ "\n"
  wFlush = return ()
```

Implementacja produkcyjna:
```haskell
instance WrapperIO IO where
  wGetChar  = getChar
  wPutChar  = putChar
  wGetLine  = getLine
  wPutStr   = putStr
  wPutStrLn = putStrLn
  wFlush    = hFlush stdout
```

Implementacja zamockowana (na potrzeby testów):
```haskell
instance WrapperIO MockIO where
  wGetChar = mockGetChar
  wPutChar = mockPutChar
  wGetLine = mockGetLine
  wPutStr  = mockPutStr
```

Kod produkcyjny:
```haskell
main :: IO ()
main = do
  putStrLn "Hello, Eta!"
  ioWFilterIf0
```

Nowe testy nie różnią się wiele od poprzednich.
Jednak tym razem nie musimy przekazywać funkcji jako parametrów:
```haskell
testsOfFilterIf0 :: Test
testsOfFilterIf0 = test
  [ "testWFilter0" ~: "test WFilterIf0" ~: "qwerty\n" ~=? execMockIO wFilterIf0 "qwerty0uiop"
  ]
```

## Podsumowanie

Nie chciałem poznawać monady [State] tak wcześnie podczas mojej nauki **[Haskella]**,
ale zostałem do tego zmuszony przez chęć napisania testów jednostkowych do mojego interpretera.
Monada [State] umożliwia modyfikowanie zmiennych w języku który raczej słynie z niemodyfikowalnych zmiennych.
Myślę,
że tej monady nie należy nadużywać.

Kod jest dostępny na [GitHubie](https://github.com/helvm/helma/tree/v0.4.0.0).

[Haskell]:            /langs/haskell
[Haskella]:           /langs/haskell
[Haskellu]:           /langs/haskell

[IO]:                 /tags/io
[State]:              /tags/state

[EsoLangs]:           https://esolangs.org/
[Cat]:                https://esolangs.org/wiki/Cat_program
