---
title:    'Rodziny typów w Haskellu'
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

Spytano mnie raz co jest trudniego w Haskellu co jednocześnie jest łatwa w OOP.
Stworzenie interfejsu kolekcji i danie możliwości implementowania go klientom

Klasa typu od dwóch parametrów

Próbowałem, wyszło źle o czym jest w https://writeonly.pl/haskell-eta/pattern-matching

Opazało się że potrzebuje rodziny typów

Rodziny typów to rozmudowanie zagadnienie zachaczające od typy zależne (ang. Dependencies types)

O czymmożna poczytać na wiki haskella

https://wiki.haskell.org/GHC/Type_families

Dodatkowo wplatują się tu jeszcze Algebtaiczne typy danych (ang. Generalised algebraic datatype, **GADT**) 


Główna różnica miedzy nimi jest taka że algebraiczne typy danych są zamknięte (wszystkie implementacje muszą być w jednym miejscu),
a rodziny typów są otwarte (można dodawać nowe implementacje w nowych plikach)

## Składnia

Poszukując 

```haskell
-- Declare a list-like data family
data family XList a

-- Declare a list-like instance for Char
data instance XList Char = XCons !Char !(XList Char) | XNil

-- Declare a number-like instance for ()
data instance XList () = XListUnit !Int
```

Szykając interfejsu kolekcji trzeba wiedzieć że szuka się interfejsu kolekcji.
Wtedy w prosty sposób można znaleźć to:
```haskell
class Collects ce where
  type Elem ce
  empty  :: ce
  insert :: Elem ce -> ce -> ce
  member :: Elem ce -> ce -> Bool
  toList :: ce -> [Elem ce]
```
A więc rodzina typów nie różni się wiele od klasy typu.
Jedyną różnicą jest linia `type Elem ce`

Jest też krótrzy zapis
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

Ale można zdefiować element też wprost
```haskell
instance Eq e => Collects [e] where
  type Elem [e]   = e
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