---
title:    'Abstrakcja i dopasowanie do wzorców'
author:   TheKamilAdam
category: haskell-eta
langs:    haskell
libs:     containers
projects: helma
eso:      eta whitespace
tags:     abstraction operator pattern-matching sequence type-class
redirect_from:
- pattern-matching
- haskell-eta/pattern-matching
---

Po [hermetyzacji] i [abstrakcji] RAMu dla interpretera [HelMA] pora na stos.
Stos, zwłaszcza stos arytmetyczny, jest strukturą używaną w wielu interpreterach jezyków ezoterycznych.
Więc warto wydzielić tą abstrakcję do osobnego modułu.


Żeby zaimplementować stos będziemy potrzebować dopasowania do wzorców (ang. *[pattern matching]*),
niestety abstrakcje i dopasowanie do wzorców są to pojęcia kłócące się.

Ponieważ dopasowanie do wzorców działa na implementacji, 
Trzeba opakować dopasowanie do wzorców w abstrakcje.


## Abstrakcja i klasy typów

Spójrzmy na moduł `HelVM.HelCam.Common.Memories.Stack`,
dla czytelności podzielony na pięć listingów.

Najpierw deklaracje i importy:
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

Jak się później okaże, eksporty zawierają jeden typ,
jedną klasę typów (ang. *[Type Class]*)
i 9 funkcji, 
w tym jedna funkcja generyczna i osiem metod (uparcie nazywam tak funkcje w klasach typów).


Następnie kod,
który normalnie znalazłby się w klasie bazowej:
```haskell
select :: Stack s m => Index -> m -> s
select i stack = check $ HelVM.HelCam.Common.Memories.Stack.lookup i stack where
  check (Just symbol) = symbol
  check  Nothing      = error $ "Empty stack " <> show stack <> " index " <> show i
```
Definiujemy jedną funkcję generyczną.
Czemu tylko jedną?
O tym później.

### Abstrakcja oparta na klasie typu

Nasz stos będzie potrzebować 8 podstawowych metod.

Podobnie jak dla klasy typów `RAM` potrzebujemy klasy typów dla dwóch parametrów,
symbolu `s` i pamieci `m`:
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
Niestety nie umiałem tego napisać lepiej.

### Implementacja oparta na liście

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
Nic nadzwyczajnego.

### Implementacja oparta na sekwencji

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
Jeśli operowalibyśmy na drugim końcu sekwencji należałoby użyć `:|>` i `|>`.

## Ograniczenia

Tutaj pojawia się mały zgrzyt.
Niestety nie potrafię zdefinioć niektórych funkcji generycznych dla parametru generycznego `Stack s m`.
Na razie musimy zadowolić się mniej genrycznymi wersjami dla parametru `Stack Symbol m`.
Funkcje te są zdefiniowane w modułach `HelVM.HelCam.Machines.ETA.StackOfSymbols` oraz `HelVM.HelCam.Machines.WhiteSpace.StackOfSymbols`.

Funkcje pomocnicze dla interpretera eso języka **[ETA]**:
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

Funkcje pomocnicze dla interpretera eso języka **[WhiteSpace]**:
```haskell
{-# Language AllowAmbiguousTypes   #-}
{-# Language FlexibleContexts      #-}
{-# Language FlexibleInstances     #-}
{-# Language MultiParamTypeClasses #-}
module HelVM.HelCam.Machines.WhiteSpace.StackOfSymbols where

import HelVM.HelCam.Machines.WhiteSpace.EvaluatorUtil
import HelVM.HelCam.Machines.WhiteSpace.Instruction

import HelVM.HelCam.Common.Memories.Stack

-- Arithmetic

binaryOp :: Stack Symbol m => BinaryOperator -> m -> m
binaryOp op stack = push1 (doBinary op symbol symbol' ::Symbol) stack' where (symbol, symbol', stack') = pop2 stack

-- Stack instructions

swap :: Stack Symbol m => m -> m
swap stack = push2 (symbol'::Symbol) symbol stack' where (symbol, symbol', stack') = pop2 stack

discard :: Stack Symbol m => m -> m
discard = drop' (0::Symbol) 1

slide :: Stack Symbol m => Index -> m -> m
slide i stack = push1 (symbol::Symbol) (drop' (0::Symbol) i stack') where (symbol, stack') = pop1 stack

dup :: Stack Symbol m => m -> m
dup = copy 0

copy :: Stack Symbol m => Index -> m -> m
copy i stack = push1 (select i stack ::Symbol) stack
```

Jak widać funkcja `copy :: Stack Symbol m => Index -> m -> m` jest w obu modułach,
jednak nie potrafię jej w prosty sposób uogólnić.

## Przykład użycia
Jako przykład użycia fragment modułu `HelVM.HelCam.Machines.ETA.Evaluator`:
```haskell
class Evaluator r where
  next :: Stack Symbol m => InstructionUnit -> m -> r
  next iu s = doInstruction t iu' s where (t, iu') = nextIU iu

  doInstruction :: Stack Symbol m => Maybe Token -> InstructionUnit -> m -> r
  -- IO instructions
  doInstruction (Just O) iu s = doOutputChar iu s
  doInstruction (Just I) iu s = doInputChar  iu s

  -- Stack instructions
  doInstruction (Just N) iu s = next iu' (push1 (symbol::Symbol) s) where (symbol, iu') = parseNumber iu
  doInstruction (Just H) iu s = next iu $ halibut s

  -- Arithmetic
  doInstruction (Just S) iu s = next iu $ sub s
  doInstruction (Just E) iu s = next iu $ Stack.divMod s

  -- Control
  doInstruction (Just R) iu s = next iu s
  doInstruction (Just A) iu@(IU il ic) s = next iu (push1 (nextLabel il ic) s)
  doInstruction (Just T) iu@(IU il _ ) s = transfer $ pop2 s where
    transfer (_, 0, s') = next iu s'
    transfer (0, _, _ ) = doEnd
    transfer (l, _, s') = next (IU il $ findAddress il l) s'
  doInstruction Nothing _ _  = doEnd

  ----
  doEnd :: r
  doOutputChar :: Stack Symbol m => InstructionUnit -> m -> r
  doInputChar  :: Stack Symbol m => InstructionUnit -> m -> r
```
Cel został osiągnięty.
Struktura stosu została schowana za abstrakcją klasy typu `Stack`.
Teraz można swobodnie wymieniać implementację między listą a sekwencją.


## Podsumowanie
Mimo że cel został osiągnięty,
kod jednak nie wyszedł tak dobry jak chciałem.
Może to pora na użycie *[Rodzin typów]*?

Kod interpretera **[HelMA]** po zmianach znajduje się na [githubie](https://github.com/helvm/helma/tree/v0.6.4.0).


[Scali]:                       /langs/scala
[Javie]:                       /langs/java
[Javy]:                        /langs/java
[Haskell]:                     /langs/haskell

[ETA]:                         /eso/eta
[WhiteSpace]:                  /eso/whitespace

[interfejsy]:                  /tags/interface
[oop]:                         /tags/oop
[traity]:                      /tags/trait
[klasy typów]:                 /tags/type-class
[kostruktura]:                 /tags/coproduct
[Type Class]:                  /tags/type-class
[pattern matching]:            /tags/pattern-matching

[HELMA]:                       /projects/helma

[Abstrakcji]:                  /haskell-eta/abstraction
[Hermetyzacji]:                /haskell-eta/encapsulation

[Wieloparametrowa klasa typu]: https://wiki.haskell.org/Multi-parameter_type_class

[Rodzin typów]:                https://wiki.haskell.org/GHC/Type_families              
