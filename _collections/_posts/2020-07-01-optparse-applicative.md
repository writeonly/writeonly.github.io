---
title:    'Parsowanie parametrów wiersza poleceń w Haskellu'
author:   TheKamilAdam
category: haskell-eta
tags:     assembler cli interpreter
langs:    eta haskell
libs:     optparse-applicative
tools:    cabal etlas
projects: helma helpa helvm
eso:      brainfuck eta subleq whitespace
redirect_from:
  - optparse-applicative
  - haskell-eta/optparse-applicative
---

Prawie każdy program,
który można wywoływać z wiersza poleceń (ang. *Command Line Interface*, **[CLI]**),
a [interpreter] jak [HelMA] w szczególności,
musi posiadać obsługę parametrów (ang. *Options*) przekazywanych z wiersza poleceń.
Obsługę taką można napisać samodzielnie albo z pomocą dedykowanej bibliotek.
Ja zdecydowałem się na bibliotekę [optparse-applicative]. 
Biblioteka ta posiada szereg zalet.
Między innymi można jej używać w **[eta-lang]**.

## Biblioteka `optparse-applicative` w praktyce

Biblioteka [optparse-applicative] jest jedną z tych bibliotek,
do których najlepiej nie czytać dokumentacji tylko od razu spojrzeć na przykład kodu.
Zwłaszcza jeśli mało umie się jeszcze **[Haskella]**.

Najpierw tworzymy moduł,
który będzie zawierać parser parametrów przekazywanych przez wiersz poleceń:
```haskell
{-# Language DataKinds          #-}
{-# Language ExplicitNamespaces #-}

module AppOptions where

import Options.Applicative

import Text.Read
```

### Parser parametrów
Następnie tworzymy sam parser parametrów:
```haskell
optionParser :: Parser AppOptions
optionParser = AppOptions
  <$> strOption    (  long    "lang"
                   <> short   'l'
                   <> metavar "[LANG]"
                   <> help   ("Language to interpret " ++ show langs)
                   <> value (show Cat)
                   <> showDefault
                   )
  <*> switch       (  long    "emit-tl"
                   <> short   'T'
                   <> help    "Emit the lexed tokens"
                   <> showDefault
                   )
  <*> switch       (  long    "emit-il"
                   <> short   'I'
                   <> help    "Emit the parsed instructions"
                   <> showDefault
                   )
  <*> switch       (  long    "ascii-labels"
                   <> short   'A'
                   <> help    "Use ascii labels"
                   <> showDefault
                   )
  <*> switch       (  long    "eta"
                   <> short   'E'
                   <> help    "Eta compliance mode"
                   <> showDefault
                   )
  <*> strOption    (  long    "impl"
                   <> short   'i'
                   <> metavar "[IMPL]"
                   <> help   ("Implementation of interpreter " ++ show impls)
                   <> value (show Monadic)
                   <> showDefault
                   )
  <*> argument str (  metavar "FILE")
```

Biblioteka [optparse-applicative] obsługuje kilka typów parametrów.
Są to między innymi:
* `strOption` - opcjonalny parametr typu `String`
* `switch` - parametr typu `boolean` nieprzyjmujący wartości
* `argument` - wymagany parametr typu `String`

### Record
Kolejnym krokiem jest zdefiniowanie rekordu,
do którego będą zapisywane parametry:
```haskell
data AppOptions = AppOptions
  { lang        :: String      -- Lang
  , emitTL      :: EmitTL
  , emitIL      :: EmitIL
  , asciiLabels :: AsciiLabels
  , etaMode     :: EtaMode
  , impl        :: String      -- Impl
  , file        :: String
  }
```

### Typy niestandardowe
Niestandardowe typy danych to tylko aliasy na typy standardowe:
```haskell
type EmitIL      = Bool
type EmitTL      = Bool
type AsciiLabels = Bool
type EtaMode     = Bool
```

Żeby używać niestandardowych typów danych, musimy je ręcznie sparsować.
W przypadku nieudanego parsowania możemy zgłosić błąd:
```haskell
data Lang = Cat | BF | WS
  deriving (Eq, Read, Show)

langs :: [Lang]
langs = [Cat, BF, WS]

computeLang :: String -> Lang
computeLang raw = valid $ readMaybe raw where
  valid (Just a)  = a
  valid Nothing = error ("Lang '" ++ raw ++ "' is not valid lang. Valid langs are : " ++ show langs)

----

data Impl = Monadic | Interact deriving (Eq, Read, Show)

impls :: [Impl]
impls = [Monadic, Interact]

computeImpl :: String -> Impl
computeImpl raw = valid $ readMaybe raw where
  valid (Just a)  = a
  valid Nothing = error ("Impl '" ++ raw ++ "' is not valid impl. Valid impls are : " ++ show impls)
```

## Main i złożenie wszystkiego razem

W funkcji `main` znajduje się wywołanie parsera parametrów i przekazanie sterowania do funkcji `run`:
```haskell
main :: IO ()
main = execParser opts >>= run where
  opts = info (optionParser <**> helper)
      ( fullDesc
     <> header "HelCam: The Interpreter of BrainFuck and WhiteSpace"
     <> progDesc "Runs esoteric programs - complete with pretty bad error messages" )
```

### Run
W funkcji `run` znajdują się globalne ustawienia oraz przekazanie sterowania do funkcji `eval`: 
```haskell
run :: AppOptions -> IO ()
run AppOptions{file, lang, emitTL, emitIL, asciiLabels, etaMode, impl} = do
  hSetBuffering stdout NoBuffering
  source <- readFile file
  eval (computeLang lang) emitTL emitIL asciiLabels etaMode (computeImpl impl) source
```

### Eval
Główna logika wyboru interpretera znajduje się w funkcji `eval`.
Zamienia ona kombinację sparsowanych parametrów linii poleceń na konkretne akcje,
głównie wywołania interpreterów konkretnych języków ezoterycznych:
```haskell
eval :: Lang -> EmitTL -> EmitIL -> AsciiLabels -> EtaMode -> Impl -> Source -> IO ()
eval BF  _    _    _ True Interact = BFIE.interactEvalBF
eval BF  _    _    _ True _        = BFME.monadicEvalBF
eval BF  _    _    _ _    Interact = interactEvalBF
eval BF  _    _    _ _    _        = monadicEvalBF
eval WS  True _    _ _    _        = print . tokenizeWS
eval WS  _    True a _    _        = pPrintNoColor . parseWS a
eval WS  _    _    a True Interact = WSIE.interactEvalWS a
eval WS  _    _    a True _        = WSME.monadicEvalWS a
eval WS  _    _    a _    Interact = interactEvalWS a
eval WS  _    _    a _    _        = monadicEvalWS a
eval _   _    _    _ _    _        = putStrLn
```

## Podsumowanie

Można uznać,
że zakończyłem pisanie interpretera języka [BrainFuck],
dodatkowo pisząc także interpreter języka [WhiteSpace].
Nie czuję jednak,
żebym umiał język programowania **[Haskell]**.

Projekt można kontynuować na różne sposoby:
* Praca w głąb, czyli optymalizacja interpreterów.
W tej chwili jednak nie wiem co optymalizować,
ponieważ mam za mało danych.
* Praca wzwyż, czyli nadbudowanie abstrakcji,
czyli np. dodanie wspólnego pseudo asemblerem dla języków [BrainFuck] i [WhiteSpace].
To jednak nadaje się bardziej na kolejny projekt jak [Helpa].
* Praca wszerz, czyli dodawanie kolejnych ezoterycznych języków programowania jak [SubLeq] i [ETA].

Kod jest dostępny na [GitHubie](https://github.com/helvm/helma/tree/v0.5.0.0).

[eta-lang]:             /langs/eta
[Haskell]:              /langs/haskell
[Haskella]:             /langs/haskell
[Haskellu]:             /langs/haskell

[optparse-applicative]: /libs/optparse-applicative

[HelMA]:               /projects/helma
[HelPA]:                /projects/helpa
[HelVM]:                /projects/helvm

[BrainFuck]:            /eso/brainfuck
[ETA]:                  /eso/eta
[SubLeq]:               /eso/subleq
[WhiteSpace]:           /eso/whitespace

[assembler]:            /tags/assembler
[cli]:                  /tags/cli
[interpreter]:          /tags/interpreter
