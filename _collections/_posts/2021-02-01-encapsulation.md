---
title:    'Hermetyzacja w Haskellu'
author:   TheKamilAdam
category: haskell-eta
tags:     containers
langs:    haskell
libs:     containers
eso:      subleq whitespace
redirect_from:
- encapsulation
- haskell-eta/encapsulation
---

Abstrakcja kolekcji w Haskellu 

Stack zamiast listy
hermetyzacja i abstrakcja

ArithmeticStack zamiast Stack.

Polimorficzny Stack i ArithmeticStack.

Dwie implementacje 
ListStack i SeqStack

RAM

## Implementacje

Udało mi się napisać trzy implementacje. 
Opartą na liscie, sekwencji i mapie.

### Implementacja oparta na liście
Najpierw implementacja oparta na liście.
Była to dla mnie naturalna implementacja ponieważ list jest domyślną kolekcją danych w Haskellu.


```haskell
{-# Language GeneralizedNewtypeDeriving #-}
module HelVM.HelCam.Common.RAM.ListRAM (
  RAM,
  empty,
  fromList,
  load,
  store
) where

import HelVM.HelCam.Common.Util

import Data.Default
```

Eksportujamy typ `RAM` oraz nazwy czterech funkcji pracujących na tym typie.
Dwa konstruktory oraz *mutatory*.

Następnie definiujemy oba konstruktory:
```haskell
import Prelude hiding (empty, fromList)

newtype RAM s = MakeRAM [s] deriving (Foldable)
type DRAM s = D (RAM s)

-- Constructors
empty :: Default s => RAM s
empty = MakeRAM []

fromList :: Default s => [s] -> RAM s
fromList = MakeRAM
```

Następnie definiujemy dwa *mutatory*,
które niestety będziemy musieli przekopiowywać między implementacjami
(Do poprawy w przyszłości):
```haskell
-- Mutators
load :: (Integral a, Default s) => RAM s -> a -> s
load (MakeRAM m) address = index' m (fromIntegral address) ?: def

store :: (Integral a, Default s) => a -> s -> DRAM s
store address symbol (MakeRAM m) = MakeRAM $ insert' (fromIntegral address) symbol m
```

I teraz niskopoziomowe funkcje, 
wymagane przez *mutatory*, 
zależne od implementacji
```haskell
-- Private
index' :: [s] -> Int -> Maybe s
index' = (!!?)

insert' :: Default s => Int -> s -> [s] -> [s]
insert' 0       symbol []     = [symbol]
insert' 0       symbol (_:xs) = symbol : xs
insert' address symbol []     = def    : insert' (address-1) symbol []
insert' address symbol (x:xs) = x      : insert' (address-1) symbol xs
```

O ile odczyt elementów z listy jest proste,
o tyle zapis czegoś w liście wymaga trochę kodu.
Wymaga także przekopiowania fragmentu listy.
W przypadku dodania nowego elementu na koniec listy wymaga przekopiowania całej zawartości listy :(

### Implementacja oparta na Sekwencji

```haskell
import Data.Sequence as Seq

newtype RAM s = MakeRAM (Seq s) deriving (Foldable)
type DRAM s = D (RAM s)

-- Constructors
empty :: Default s => RAM s
empty = MakeRAM Seq.empty

fromList :: Default s => [s] -> RAM s
fromList = MakeRAM . Seq.fromList
```

```haskell
-- Private
index' :: Seq s -> Int -> Maybe s
index' = (!?)

insert' :: Default s => Int -> s -> Seq s -> Seq s
insert' address symbol m = insert'' (Seq.length m) where
  insert'' l
    | address < l  = Seq.update address symbol m
    | otherwise    = m <> Seq.replicate (address - l) def |> symbol
```

### Implementacja oparta na Mapie

Nie będzie to jednak zwykła mapa a IntMapa dedykowana do adresowania jej typem `Int`.
Ograniczenie to wynika z tego 

```haskell
import Data.IntMap as IntMap

newtype RAM s = MakeRAM (IntMap s) deriving (Foldable)
type DRAM s = D (RAM s)

-- Constructors
empty :: Default s => RAM s
empty = MakeRAM IntMap.empty

fromList :: Default s => [s] -> RAM s
fromList list = MakeRAM $ IntMap.fromList $ zip [0..] list
```

Implementacje `index'` i `insert'` są banalnie proste
```haskell
-- Private
index' :: IntMap s -> Int -> Maybe s
index' = (!?)

insert' :: Int -> s -> IntMap s -> IntMap s
insert' = insert
```

Czy implementacja oparta na `IntMap` jest lepsza niż na `Seq`?
Tego nie wiem.
`IntMap` nie posiada tegoretycznych wartości w notacji dużego O.
Z drugiej strony nawet jakby posiadał to może się to różnie przekładać na rzeczywiste rezultaty.
Wszystko też zależy od ilości danych.
Potrzebny byłby duży program w SubLequ.
I porządne testy wydajnościowe

Ważne jest to, że udało się z hermetyzować użycie Listy, Sekqencji i IntMapy


## Użycie

```haskell

import HelVM.HelCam.Machines.SubLeq.Lexer
import HelVM.HelCam.Machines.SubLeq.Symbol

import HelVM.HelCam.Common.RAM.IntMapRAM as RAM
import HelVM.HelCam.Common.MockIO
import HelVM.HelCam.Common.Util
import HelVM.HelCam.Common.WrapperIO

type Memory = RAM Symbol

class Evaluator r where
  eval :: Source -> r
  eval = evalIL . tokenize

  evalIL :: SymbolList -> r
  evalIL il = doInstruction 0 $ RAM.fromList il

  doInstruction :: Symbol -> Memory -> r
  doInstruction ic m
    | ic < 0    = doEnd
    | src < 0   = doInputChar  dst ic m
    | dst < 0   = doOutputChar src ic m
    | otherwise = doInstruction ic' $ store dst diff m
      where
        src  = load m ic
        dst  = load m $ ic + 1
        diff = load m dst - load m src
        ic'
          | diff <= 0 = load m $ ic + 2
          | otherwise = ic + 3

  doEnd    :: r
  doInputChar  :: Symbol -> Symbol -> Memory -> r
  doOutputChar :: Symbol -> Symbol -> Memory -> r

----

interactEval :: Source -> IO ()
interactEval _ = pass

batchEval :: Source -> Output
batchEval source = eval source ([]::String)

batchEvalIL :: SymbolList -> Output
batchEvalIL memory = evalIL memory ([]::String)

instance Evaluator (Input -> Output) where
  doEnd _ = []

  doInputChar _       _  _ []    = error "Empty input"
  doInputChar address ic m (value:input) = doInstruction (ic+3) (store address (ord value) m) input

  doOutputChar address ic memory input = chr (load memory address) : doInstruction (ic+3) memory input

----

monadicEval :: Source -> IO ()
monadicEval = eval

instance Evaluator (IO ()) where
  doEnd = pass

  doInputChar address ic m = do
    value <- wGetInt
    doInstruction (ic+3) $ store address value m

  doOutputChar address ic memory = do
    wPutInt $ load memory address
    doInstruction (ic+3) memory

----

instance Evaluator (MockIO ()) where
  doEnd = pass

  doInputChar address ic m = do
    value <- mockGetInt
    doInstruction (ic+3) $ store address value m

  doOutputChar address ic memory = do
    mockPutInt $ load memory address
    doInstruction (ic+3) memory
```

## Podsumowanie

