---
title:    'Abstrakcja i dopasowanie do wzorców'
author:   TheKamilAdam
category: haskell-eta
langs:    haskell
libs:     containers
eso:      eta subleq
tags:     abstraction typeclass sequence
redirect_from:
- pattern-matching
- haskell-eta/pattern-matching
---

Abstrakcje i dopasowanie do wzorców są to pojęcia kłócące się.

Ponieważ dopasowanie do wzorców działa na implementacji, 
Trzeba opakować dopasowanie do wzorców w abstrakcje.


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

select :: Stack s m => Index -> m -> s
select i stack = check $ HelVM.HelCam.Common.Memories.Stack.lookup i stack where
  check (Just symbol) = symbol
  check  Nothing      = error $ "Empty stack " <> show stack <> " index " <> show i
```

## Abstrakcja oparta na klasie typu

Nasz stos będzie potrzebować 8 podstawowych metod

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
Wynika to z tego że w każdej sygnaturze muszą być użyte oba typy generyczne.
Niestety nie umiałem tego zrobić lepiej.

## Implementacja oparta na liscie

Implementacja dla listy:
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
Mamy tu klasyczny patern matching dla list.
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

## ETA

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

## Podsumowanie

Nie wyszło to tak dobrze jak chciałem.

