---
title:    'Abstrakcja i Hermetyzacja w Haskellu'
author:   TheKamilAdam
category: programming
tags:     atd coproduct product
langs:    java kotlin scala rust
libs:     autovalue immutables lombok vavr
tools:    jvm
redirect_from:
- 5-programming-languages
- programming/5-programming-languages
---

Abstrakcja kolekcji w Haskellu 

Stack zamiast listy
hermetyzacja i abstrakcja

ArithmeticStack zamiast Stack.

Polimorficzny Stack i ArithmeticStack.

Dwie implementacje 
ListStack i SeqStack

## Implementacja oparta na liście
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

import Prelude hiding (empty, fromList)

newtype RAM s = MakeRAM [s] deriving (Foldable)
type DRAM s = D (RAM s)
```

Eksportujamy typ `RAM` oraz nazwy czterech funkcji pracujących na tym typie.
Dwa konstruktory oraz *getter* i *setter*.

Następnie definiujemy dwie funkcje które niestety będziemy musieli przekopiowywać między implementacjami
(Do poprawy w przyszłości):
```haskell
load :: (Integral a, Default s) => RAM s -> a -> s
load heap address = load' heap (fromIntegral address) ?: def

store :: (Integral a, Default s) => a -> s -> DRAM s
store address = store' (fromIntegral address)
```

I teraz rdzeń implementacji:
```haskell
-- Core
empty :: Default s => RAM s
empty = MakeRAM []

fromList :: Default s => [s] -> RAM s
fromList = MakeRAM

load' :: Default s => RAM s -> Int -> Maybe s
load' (MakeRAM m) address = m !!? address

store' :: Default s => Int -> s -> DRAM s
store' address symbol (MakeRAM m) = MakeRAM $ insert' address symbol m

insert' :: Default s => Int -> s -> [s] -> [s]
insert' 0       symbol []     = [symbol]
insert' 0       symbol (_:xs) = symbol : xs
insert' address symbol []     = def    : insert' (address-1) symbol []
insert' address symbol (x:xs) = x      : insert' (address-1) symbol xs
```

O ile utworzenie listy i odczyt elementów są proste,
o tyle zapis czegoś w liście wymaga trochę kody.
Wymaga także przekopiowania fragmentu listy.
W przypadku dodania nowego elementu na koniec wymaga przekopiowania całej listy :(

## Implementacja oparta na Sekwencji

```haskell
import HelVM.HelCam.Common.Util

import Data.Default
import Data.Sequence as Seq

newtype RAM s = MakeRAM (Seq s) deriving (Foldable)
type DRAM s = D (RAM s)
```

```haskell
-- Core
fromList :: Default s => [s] -> RAM s
fromList = MakeRAM . Seq.fromList

empty :: Default s => RAM s
empty = MakeRAM Seq.empty

load' :: Default s => RAM s -> Int -> Maybe s
load' (MakeRAM h) address = h !? address

store' :: Default s => Int -> s -> DRAM s
store' address symbol (MakeRAM m) = MakeRAM $ insert (Seq.length m) where
  insert l
    | address < l  = Seq.update address symbol m
    | otherwise    = m <> Seq.replicate (address - l) def |> symbol
```

## Implementacja oparta na Mapie

```haskell
import HelVM.HelCam.Common.Util

import Data.Default
import Data.IntMap as IntMap

newtype RAM s = MakeRAM (IntMap s) deriving (Foldable)
type DRAM s = D (RAM s)
```

```haskell
-- Core
fromList :: Default s => [s] -> RAM s
fromList list = MakeRAM $ IntMap.fromList $ zip [0..] list

empty :: Default s => RAM s
empty = MakeRAM IntMap.empty

load' :: Default s => RAM s -> Int -> Maybe s
load' (MakeRAM m) address = m !? address

store' :: Default s => Int -> s -> DRAM s
store' address symbol (MakeRAM m) = MakeRAM $ insert address symbol m
```

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