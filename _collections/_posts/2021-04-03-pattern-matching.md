---
title:    'Abstrakcja i dopasowanie do wzorców'
author:   TheKamilAdam
category: haskell-eta
langs:    haskell
libs:     containers
projects: helcam
eso:      eta subleq
tags:     abstraction typeclass sequence
redirect_from:
- pattern-matching
- haskell-eta/pattern-matching
---

Po [hermetyzacji] i [abstrakcji] RAMu dla interpretera **[HELCAM]** pora na stos.
Stos, zwłaszcza stos arytmetyczny, jest strukturą używaną w wielu interpreterach jezyków ezoterycznych.
Więc warto wydzielić tą abstrakcję do osobnego modułu.


Żeby zaimplementować stos będziemy potrzebować dopasowania do wzorców (ang. *[pattern matching]*)

Abstrakcje i dopasowanie do wzorców są to pojęcia kłócące się.

Ponieważ dopasowanie do wzorców działa na implementacji, 
Trzeba opakować dopasowanie do wzorców w abstrakcje.


Najpier
```haskell
{-# Language AllowAmbiguousTypes   #-}
{-# Language FlexibleInstances     #-}
{-# Language MultiParamTypeClasses #-}
module HelVM.HelCam.Common.Memories.Stack (
  Index,
  Stack,
  select,
  HelVM.HelCam.Common.Memories.Stack.empty,
  HelVM.HelCam.Common.Memories.Stack.lookup,
  HelVM.HelCam.Common.Memories.Stack.splitAt',
  HelVM.HelCam.Common.Memories.Stack.drop',
  push1,
  pop1,
  push2,
  pop2
) where

import Data.Sequence as Seq

type Index = Int
```

Najpier exporty.
Jak się później okaże mamy tu jeden typ,
jedną klasę typów (ang. *[TypeClass]*)
i 9 funkcji.
Jedna generyczna i osiem metod (funkcje  klasie typów).


Następnie kod, który normalnie znalazłby się w klasie bazowej:
```haskell
select :: Stack s m => Index -> m -> s
select i stack = check $ HelVM.HelCam.Common.Memories.Stack.lookup i stack where
  check (Just symbol) = symbol
  check  Nothing      = error $ "Empty stack " <> show stack <> " index " <> show i
```
Udostępniamy jedną funkcję generyczną.
Czemu tylko jedną?
O ty później.

## Abstrakcja oparta na klasie typu

Nasz stos będzie potrzebować 8 podstawowych metod.

Podobnie jak dla klasy typów **[RAM]** potrzebujemy klasy typów dla dwóch parametrów, symbolu i pamieci:

```haskell
class (Semigroup m, Show m) => Stack s m where
  empty    :: m
  lookup   :: Index -> m -> Maybe s
  splitAt' :: s -> Index -> m -> (m, m)
  drop'    :: s -> Index -> m -> m
  push1    :: s -> m -> m
  pop1     :: m -> (s, m)
  push2    :: s -> s -> m -> m
  pop2     :: m -> (s, s, m)
```
Są tu dwie brzydkie metody `splitAt'` i `drop'`.
Wynika to z tego,
że w każdej sygnaturze muszą być użyte oba parametry generyczne.
Niestety nie umiałem tego zrobić lepiej.

## Implementacja oparta na liście

Najpierw prostsza implementacja dla listy:
```haskell
instance Show s => Stack s [s] where
  empty                              = []
  lookup            i         stack  = stack !!? i
  splitAt' _        i         stack  = Prelude.splitAt i stack
  drop'    _        i         stack  = Prelude.drop i stack
  push1             symbol    stack  = symbol: stack
  pop1             (symbol  : stack) = (symbol, stack)
  pop1                        stack  = error $ "Empty stack " <> show stack
  push2    symbol   symbol'   stack  = symbol: symbol': stack
  pop2    (symbol : symbol' : stack) = (symbol, symbol', stack)
  pop2                        stack  = error $ "Empty stack " <> show stack
```
Mamy tu klasyczne dopasowanie do wzorców dla list.
Nic nadzwyczajnego

## Implementacja oparta na sekwencji

Implementacja dla sekwencji:
```haskell
instance Show s => Stack s (Seq s) where
  empty                                  = Seq.fromList []
  lookup              i           stack  = Seq.lookup i stack
  splitAt' _          i           stack  = Seq.splitAt i stack
  drop'    _          i           stack  = Seq.drop i stack
  push1               symbol      stack  = symbol <| stack
  pop1               (symbol :<|  stack) = (symbol, stack)
  pop1                            stack  = error $ "Empty stack " <> show stack
  push2    symbol     symbol'     stack  = symbol <| symbol' <| stack
  pop2    (symbol :<| symbol' :<| stack) = (symbol, symbol', stack)
  pop2                            stack  = error $ "Empty stack " <> show stack
```

Mamy tu dwa nowe operatory `:<|` dla dopasowania do wzorców oraz `<|` dla dołączania do sekwencji.
Jeśli operawalibyśmy na drugim końcu sekwencji należałoby użyć `:|>` i `|>`.

## Przykład użycia

```haskell
{-# Language FlexibleContexts      #-}
module HelVM.HelCam.Machines.ETA.StackOfSymbols where

import HelVM.HelCam.Machines.ETA.EvaluatorUtil  

import HelVM.HelCam.Common.Memories.Stack

-- Arithmetic

divMod :: Stack Symbol m => m -> m
divMod stack = push2 (symbol' `mod` symbol ::Symbol) (symbol' `div` symbol ::Symbol) stack'
  where (symbol, symbol', stack') = pop2 stack

sub :: Stack Symbol m => m -> m
sub stack = push1 (symbol' - symbol ::Symbol) stack'
    where (symbol, symbol', stack') = pop2 stack

-- Stack instructions

halibut :: Stack Symbol m => m -> m
halibut stack
  | i <= 0     = copy (negate i) stack'
  | otherwise  = move (0 ::Symbol) i stack'
    where (i, stack') = pop1 stack

move :: Stack Symbol m => Symbol -> Index -> m -> m
move symbol i stack = tops <> middles <> bottoms where
  (middles, stack')  = splitAt' symbol i stack
  (tops, bottoms)    = splitAt' symbol 1 stack'

copy :: Stack Symbol m => Index -> m -> m
copy i stack = push1 (select i stack ::Symbol) stack
```

Czemu te funkcje są osobno?
Dwuparametrowe

## Podsumowanie

Nie wyszło to tak dobrze jak chciałem.

[HELCAM]: /projects/helcam

