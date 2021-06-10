---
title:    'Zależności funkcyjne w Haskellu'
author:   TheKamilAdam
category: haskell-eta
langs:    haskell
libs:     rio
projects: helma
eso:      eta whitespace
tags:     collection stack type-class type-family 
redirect_from:
- type-family
- haskell-eta/type-family
---

Funkcje zależne

Spytano mnie raz,
co jest trudnego w **[Haskellu]**,
co jednocześnie jest łatwa w OOP.
Stworzenie interfejsu kolekcji i danie możliwości implementowania go klientom

Klasa typu od dwóch parametrów



Próbowałem, wyszło źle o czym jest w https://writeonly.pl/haskell-eta/pattern-matching

Okazało się że potrzebuje Functional Dependency.

https://web.cecs.pdx.edu/~mpj/pubs/fundeps-esop2000.pdf

https://cth.altocumulus.org/~hallgren/Papers/hallgren.pdf

Trzeba uwarzać bo to początek typów zależnych
Funkcje zależne są początkiem do typów zależnych.

https://wiki.haskell.org/Dependent_type - typy zależne
https://en.wikipedia.org/wiki/Dependent_type


Główna różnica miedzy nimi jest taka że algebraiczne typy danych są zamknięte (wszystkie implementacje muszą być w jednym miejscu),
a rodziny typów są otwarte (można dodawać nowe implementacje w nowych plikach)

## Składnia

https://www.fpcomplete.com/haskell/tutorial/fundeps/

```haskell
class Collects e ce | ce -> e where
  empty  :: ce
  insert :: e -> ce -> ce
  member :: e -> ce -> Bool
  toList :: ce -> [e]
```

implementacja wygląda jak implementacja klasy typu dla dwóch parametrów:
```haskell
instance Eq e => Collects e [e] where
  empty           = []
  insert e l      = (e:l)
  member e []     = False
  member e (x:xs) 
    | e == x      = True
    | otherwise   = member e xs
  toList l        = l
```


## Klasy typów w HelMA

Głowna zmiana polegała na dodaniu `| m -> s` do klasy typu `Stack`.

```haskell
class (Semigroup m, Show m) => Stack s m | m -> s where
  empty    :: m
  lookup   :: Index -> m -> Maybe s
  splitAt  :: Index -> m -> (m, m)
  drop     :: Index -> m -> m
  pushList :: [s] -> m -> m
  push1    :: s -> m -> m
  push2    :: s -> s -> m -> m
  pop1     :: m -> (s, m)
  pop2     :: m -> (s, s, m)
  push1  symbol         = pushList [symbol]
  push2  symbol symbol' = pushList [symbol , symbol']
  splitAt' :: Index -> m -> (m, m)
  drop'    :: Index -> m -> m
  splitAt'  = HelVM.HelMA.Common.Memories.Stack.splitAt
  drop'     = HelVM.HelMA.Common.Memories.Stack.drop
```
Przepraszam za brzydkie `splitAt'` i `drop'`.
Mam nadzieję że gdy przemigrujemy z [relude] na [rio] przestaną być one potrzebne.

Musimy też poprawić nasze implementacje (instancje)
```haskell
instance Show s => Stack s [s] where
  empty                              = []
  lookup            i         stack  = stack !!? i
  splitAt           i         stack  = Prelude.splitAt i stack
  drop              i         stack  = Prelude.drop i stack
  pushList          symbols   stack  = symbols <> stack
--  push1             symbol    stack  = symbol: stack
--  push2    symbol   symbol'   stack  = symbol: symbol': stack
  pop1             (symbol  : stack) = (symbol, stack)
  pop1                        stack  = error $ "Empty stack " <> show stack
  pop2    (symbol : symbol' : stack) = (symbol, symbol', stack)
  pop2                        stack  = error $ "Empty stack " <> show stack

instance Show s => Stack s (Seq s) where
  empty                                  = Seq.fromList []
  lookup              i           stack  = Seq.lookup i stack
  splitAt             i           stack  = Seq.splitAt i stack
  drop                i           stack  = Seq.drop i stack
  pushList            symbols     stack  = Seq.fromList symbols >< stack
--  push1               symbol      stack  = symbol <| stack
--  push2    symbol     symbol'     stack  = symbol <| symbol' <| stack
  pop1               (symbol :<|  stack) = (symbol, stack)
  pop1                            stack  = error $ "Empty stack " <> show stack
  pop2    (symbol :<| symbol' :<| stack) = (symbol, symbol', stack)
  pop2                            stack  = error $ "Empty stack " <> show stack
```


Od tej pory nie potrzebujemy już rzutować elementu stosu na `Symbol`,
a więc wiele metod pomocniczych do manipulacji stosem możemy przenieść bezpośrednio do modułu `Stack`:
```haskell
halibut :: (Integral s, Stack s m) => m -> m
halibut stack
  | i <= 0     = copy (negate i) stack'
  | otherwise  = move i stack'
    where 
      (symbol, stack') = pop1 stack
      i = fromIntegral symbol

move :: Stack s m => Index -> m -> m
move i stack = tops <> middles <> bottoms where
  (middles, stack')  = splitAt' i stack
  (tops, bottoms)    = splitAt' 1 stack'

swap :: Stack s m => m -> m
swap stack = push2 symbol' symbol stack' where (symbol, symbol', stack') = pop2 stack

discard :: Stack s m => m -> m
discard = drop' 1

slide :: Stack s m => Index -> m -> m
slide i stack = push1 symbol (drop' i stack') where (symbol, stack') = pop1 stack

dup :: Stack s m => m -> m
dup = copy 0

copy :: Stack s m => Index -> m -> m
copy i stack = push1 (select i stack) stack
```

Możemy też zdefiniować operacja arytmetyczne:
```haskell
divMod :: (Integral s, Stack s m) => m -> m
divMod = binaryOps [Mod , Div]

sub :: (Integral s, Stack s m) => m -> m
sub = binaryOp Sub

binaryOp :: (Integral s, Stack s m) => BinaryOperator -> m -> m
binaryOp op = binaryOps [op]

binaryOps :: (Integral s, Stack s m) => [BinaryOperator] -> m -> m
binaryOps ops stack = pushList (calculateOps symbol symbol' ops) stack' where (symbol, symbol', stack') = pop2 stack
```

W tym celu wydzielimy moduł `BinaryOperator`:
```haskell
module HelVM.HelMA.Common.BinaryOperator where

calculateOps :: Integral s => s -> s -> [BinaryOperator] -> [s]
calculateOps symbol symbol' = map (calculateOp symbol symbol')

calculateOp :: Integral s => s -> s -> BinaryOperator -> s
calculateOp symbol symbol' op = doBinary op symbol' symbol

doBinary :: Integral s => BinaryOperator -> s -> s -> s
doBinary Add = (+)
doBinary Sub = (-)
doBinary Mul = (*)
doBinary Div = div
doBinary Mod = mod

data BinaryOperator = Add | Sub | Mul | Div | Mod
  deriving (Eq, Show, Read)
```

## Jeden wielki Stack

## Dużo małych Klas Typów
Jak to działa dla funktora?

## Jedna implementacja


```haskell


class (Semigroup c , Show c) => Stack e c | c -> e where
  fromList :: [e] -> c
  empty    :: c
  index    :: c -> Index -> e
  lookup   :: Index -> c -> Maybe e
  splitAt  :: Index -> c -> (c , c)
  drop     :: Index -> c -> c
  pop1     :: c -> (e , c)
  pop2     :: c -> (e , e , c)

instance (Show c , Semigroup c , I.Drop e c , I.FromList e c , I.Lookup e c , I.SplitAt e c , I.Pop1 e c , I.Pop2 e c) => Stack e c where
  fromList = I.fromList
  empty    = I.empty
  index    = I.index
  lookup   = I.lookup
  splitAt  = I.splitAt
  drop     = I.drop
  pop1     = I.pop1
  pop2     = I.pop2
```

## Sumowanie ograniczeń


http://dev.stephendiehl.com/hask/#constraint-kinds


## Podsumowanie

Stworzenie interfejsu kolekcji  Haskellu nie jest jednak trudne.
Wystarczy wiedzieć czego się szuka i znaleźć to :)



Klasy typów są niesamowity narzędziem pozwanalącym pisać bardzo elastyczny i polimorficzny kod

[Haskell]:              /langs/haskell
[Haskella]:             /langs/haskell
[Haskellu]:             /langs/haskell

[pattern-matching]:                /pattern-matching

[Wieloparametrowa klasa typu]: https://wiki.haskell.org/Multi-parameter_type_class

Dependencies types

https://wiki.haskell.org/Functional_dependencies_vs._type_families
https://wiki.haskell.org/Functional_dependencies