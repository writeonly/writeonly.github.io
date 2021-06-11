---
title:    'Zależności funkcyjne w Haskellu'
author:   TheKamilAdam
category: haskell-eta
langs:    haskell
projects: helma
tags:     collection dependent-type functional-dependency multi-parameter-type-class type-class
redirect_from:
- type-family
- haskell-eta/type-family
---

Spytano mnie raz,
co jest trudnego w **[Haskellu]**,
co jednocześnie jest łatwa w OOP.
Stworzenie interfejsu kolekcji i danie możliwości implementowania go klientom

Klasa typu od dwóch parametrów



Próbowałem, wyszło źle o czym jest w [pattern-matching]

Okazało się że potrzebuje Functional Dependency.

Trzeba uważać, bo to początek typów zależnych
Funkcje zależne są początkiem do typów zależnych.

https://wiki.haskell.org/Dependent_type - typy zależne
https://en.wikipedia.org/wiki/Dependent_type


Główna różnica miedzy nimi jest taka że algebraiczne typy danych są zamknięte (wszystkie implementacje muszą być w jednym miejscu),
a rodziny typów są otwarte (można dodawać nowe implementacje w nowych plikach)

## Składnia



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

Głowna zmiana polegała na dodaniu `| m -> s` do klasy typu [Stack].

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
Jak to działa dla funktora, aplikatyw i monada?
Każde dziedziczy z poprzedniego

Funktor posiada metodę (funkcję polimorficzną) `fmap`.
Dzięki temu nie musimy pisać w kodzie `List.fmap`, `Seq.fmap` czy `IntMap.fmap`,
tylko odpowiednia implementacja jest ustalana na podstawie parametrów.


Miło by było mieć też polimorficzne funkcje  `drop`, `empty`, `fromList` , `splitAt`,


Tworzymy folder

```haskell
module HelVM.HelMA.Common.Collections.FromList where

import Prelude hiding (fromList)

import qualified Data.List.Index as List
import qualified Data.IntMap     as IntMap
import qualified Data.Sequence   as Seq

intMapFromList :: [e] -> IntMap e
intMapFromList = IntMap.fromList . List.indexed

class FromList e c | c -> e where
  fromList :: [e] -> c
  empty :: c
  empty = fromList []

instance FromList e [e] where
  fromList = id
  empty = []

instance FromList e (Seq e) where
  fromList = Seq.fromList
  empty = Seq.empty

instance FromList e (IntMap e) where
  fromList = intMapFromList
  empty = IntMap.empty
```

```haskell
module HelVM.HelMA.Common.Collections.Lookup where

import qualified Data.IntMap   as IntMap
import qualified Data.Sequence as Seq

index :: (Show c , Lookup e c) => c -> Int -> e
index c i = check (c `indexMaybe` i) where
  check (Just e) = e
  check  Nothing = error $ "Empty stack " <> show c <> " index " <> show i

indexMaybe :: Lookup e c => c -> Int -> Maybe e
indexMaybe = flip lookup

class Lookup e c | c -> e where
  lookup:: Int -> c -> Maybe e

instance Lookup e [e] where
  lookup = flip (!!?)

instance Lookup e (Seq e) where
  lookup = Seq.lookup

instance Lookup e (IntMap e) where
  lookup = IntMap.lookup
```

```haskell
module HelVM.HelMA.Common.Collections.Insert where

import Data.Default
import Data.Sequence ((|>))

import qualified Data.IntMap     as IntMap
import qualified Data.Sequence   as Seq

class Insert e c | c -> e where
  insert :: Int -> e -> c -> c

instance Default e => Insert e [e] where
  insert 0 e []     = [e]
  insert 0 e (_:xs) = e   : xs
  insert i e []     = def : insert (i-1) e []
  insert i e (x:xs) = x   : insert (i-1) e xs

instance Default e => Insert e (Seq e) where
  insert i e c = insert' $ Seq.length c where
    insert' l
      | i < l       = Seq.update i e c
      | otherwise   = c <> Seq.replicate (i - l) def |> e

instance Insert e (IntMap e) where
  insert = IntMap.insert
```

```haskell
module HelVM.HelMA.Common.Collections.Pop where

import Data.Sequence (Seq(..))

class Pop1 e c | c -> e where
  pop1 :: c -> (e , c)

instance Show e => Pop1 e [e] where
  pop1 (e  : c) = (e , c)
  pop1       c  = error $ "Empty " <> show c

instance Show e => Pop1 e (Seq e) where
  pop1 (e :<|  c) = (e , c)
  pop1         c  = error $ "Empty " <> show c

class Pop2 e c | c -> e where
  pop2 :: c -> (e , e , c)

instance Show e => Pop2 e [e] where
  pop2 (e : e' : c) = (e , e', c)
  pop2           c  = error $ "Empty " <> show c

instance Show e => Pop2 e (Seq e) where
  pop2 (e :<| e' :<| c) = (e , e', c)
  pop2               c  = error $ "Empty  " <> show c
```

Importujemy wszystkie potrzebne moetody
```haskell
import Prelude hiding (divMod , drop , empty , fromList , splitAt , swap)

import HelVM.HelMA.Common.Collections.Drop
import HelVM.HelMA.Common.Collections.FromList
import HelVM.HelMA.Common.Collections.Lookup
import HelVM.HelMA.Common.Collections.Pop
import HelVM.HelMA.Common.Collections.SplitAt
```

Jednak pisanie kolejnych metod do jakaś tragedia:
```haskell
-- Arithmetic

divMod :: (Integral e , Semigroup c , FromList e c , Pop2 e c) => c -> c
divMod = binaryOps [Mod , Div]

sub :: (Integral e , Semigroup c , FromList e c , Pop2 e c) => c -> c
sub = binaryOp Sub

binaryOp :: (Integral e , Semigroup c , FromList e c , Pop2 e c) => BinaryOperator -> c -> c
binaryOp op = binaryOps [op]

binaryOps :: (Integral e , Semigroup c , FromList e c , Pop2 e c) => [BinaryOperator] -> c -> c
binaryOps ops c = pushList (calculateOps e e' ops) c' where (e , e', c') = pop2 c

-- Stack instructions

halibut :: (Show c , Semigroup c , Integral e , FromList e c , Lookup e c , SplitAt e c , Pop1 e c) => c -> c
halibut c
  | i <= 0    = copy (negate i) c'
  | otherwise = move i c'
    where
      i = fromIntegral e
      (e , c') = pop1 c

move :: (Semigroup c , SplitAt e c) => Index -> c -> c
move i c = c1 <> c2 <> c3 where
  (c1 , c3) = splitAt 1 c'
  (c2 , c') = splitAt i c

swap :: (Semigroup c , FromList e c , Pop2 e c) => c -> c
swap c = push2 e' e c' where (e , e' , c') = pop2 c

discard :: Drop e c => c -> c
discard = drop 1

slide :: (Semigroup c , Drop e c , FromList e c , Pop1 e c) => Index -> c -> c
slide i c = push1 e (drop i c') where (e , c') = pop1 c

dup :: (Show c , Semigroup c , FromList e c , Lookup e c) => c -> c
dup = copy 0

copy :: (Show c , Semigroup c , FromList e c , Lookup e c) => Index -> c -> c
copy i c = push1 (c `index` i) c

-- Push instructions

pushChar1 :: (Num e , Semigroup c , FromList e c) => Char -> c -> c
pushChar1 = genericPush1 . ord

genericPush1 :: (Integral v , Num e , Semigroup c , FromList e c) => v -> c -> c
genericPush1 = push1 . fromIntegral

push1 :: (Semigroup c , FromList e c) => e -> c -> c
push1 e = pushList [e]

push2 :: (Semigroup c , FromList e c) => e -> e -> c -> c
push2 e e' = pushList [e , e']

pushList :: (Semigroup c , FromList e c) => [e] -> c -> c
pushList es c = fromList es <> c
```


## Jedna implementacja

Najpierw importujemy wszystkie potrzebne funkcje do `I`:
```haskell
import Prelude hiding (divMod , drop , empty , fromList , splitAt , swap)

import qualified HelVM.HelMA.Common.Collections.Drop     as I
import qualified HelVM.HelMA.Common.Collections.FromList as I
import qualified HelVM.HelMA.Common.Collections.Lookup   as I
import qualified HelVM.HelMA.Common.Collections.Pop      as I
import qualified HelVM.HelMA.Common.Collections.SplitAt  as I
```

Nastepnie tworzymy naszą [Klasę Typów] z jedną implementacją (instancją):
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

Cała implementacja jest w pliku [StackImpl].
Czemu `*Impl`?
Bo przypomina to Javową patologię z Serwisami z jedną implementacją `*ServiceImpl`

## Sumowanie ograniczeń

Gdy już byłem zdołowany że zostanę z beznensowną Klasą Typu z jedną implementacją
przypadkiem przeczytałem że w Haskellu ograniczenia rodzai mogą być elementami pierwszego rodzaju.
Wystarczy łączyć rozszeżenie

http://dev.stephendiehl.com/hask/#constraint-kinds

Po włączeniu rozszeżenia importujemy wszystkie potrzebne nam funkcje:
```haskell

import Prelude hiding (divMod , drop , empty , fromList , splitAt , swap)

import HelVM.HelMA.Common.Collections.Drop
import HelVM.HelMA.Common.Collections.FromList
import HelVM.HelMA.Common.Collections.Lookup
import HelVM.HelMA.Common.Collections.Pop
import HelVM.HelMA.Common.Collections.SplitAt
```

A następnie piszemy jedną magiczną linię:
```haskell
type Stack e c = (Show c , Semigroup c , Drop e c , FromList e c , Lookup e c , SplitAt e c , Pop1 e c , Pop2 e c)
```
Właśnie zsumowaliśmy wszystkie ograniczenia do jednego typu `Stack`.

I teraz można żyć.
I teraz da się pracować.

Cała implementacja jest w pliku [StackConst] (jak ograniczenia).

## Podsumowanie

Stworzenie interfejsu kolekcji  Haskellu nie jest jednak trudne.
Wystarczy wiedzieć czego się szuka i znaleźć to :)



Klasy typów są niesamowity narzędziem pozwanalącym pisać bardzo elastyczny i polimorficzny kod.



[Pattern Matching]:            /pattern-matching

[Haskell]:                     /langs/haskell
[Haskella]:                    /langs/haskell
[Haskellu]:                    /langs/haskell

[ClassyPrelude]:               /libs/classy-prelude

[Klasę Typów]:                 /tags/type-class
[Typy zależne]:                /tags/dependent-types
[Wieloparametrowa klasa typu]: /tags/multi-parameter-type-class
[Zalezności funkcyjne]:        /tags/functional-dependencies

[RAM]:        https://github.com/helvm/helma/blob/master/hs/src/HelVM/HelMA/Common/Memories/RAM.hs
[Stack]:      https://github.com/helvm/helma/blob/master/hs/src/HelVM/HelMA/Common/Memories/Stack.hs
[StackConst]: https://github.com/helvm/helma/blob/master/hs/src/HelVM/HelMA/Common/Memories/StackConst.hs
[StackImpl]:  https://github.com/helvm/helma/blob/master/hs/src/HelVM/HelMA/Common/Memories/StackImpl.hs
[StackUtil]:  https://github.com/helvm/helma/blob/master/hs/src/HelVM/HelMA/Common/Memories/StackUtil.hs

[Functional dependencies vs. type families]: https://wiki.haskell.org/Functional_dependencies_vs._type_families
