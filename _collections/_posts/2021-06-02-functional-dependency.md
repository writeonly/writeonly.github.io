---
title:    'Zależności Funkcyjne w Haskellu'
author:   TheKamilAdam
category: haskell-eta
langs:    haskell java
lib:      classy-prelude relude rio
projects: helma helvm
tags:     collection dependent-type functional-dependency multi-parameter-type-class oop type-class
redirect_from:
- functional-dependency
- haskell-eta/functional-dependency
---

Spytano mnie raz,
co jest trudnego w **[Haskellu]**,
co jednocześnie jest łatwe w OOP.
Np. stworzenie interfejsu kolekcji i danie możliwości implementowania go klientom-użytkownikom.
W tym celu potrzebujemy [Klasę Typu] od dwóch parametrów.
Ale żeby mieć dobry interfejs, to nie wystarczy.
O czym się przekonaliśmy w artykule [Abstrakcja i dopasowanie do wzorców].

Okazuje się,
że tego,
czego nam brakowało to [Zależności Funkcyjne] (ang. *Functional Dependency*).

## Składnia Zależności Funkcyjnych

Poszukując przykładu interfejsu dla kolekcji można trafić na taki przykład [Zależności Funkcyjnych]:
```haskell
class Collects e ce | ce -> e where
  empty  :: ce
  insert :: e -> ce -> ce
  member :: e -> ce -> Bool
  toList :: ce -> [e]
```
Czyli jedyna nowość to `| ce -> e `.
Zapis ten pozwala na uzależnienie jednego typu od drugiego.

Implementacja wygląda jak implementacja [Klasy Typu] dla dwóch parametrów:
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

## Zależności Funkcyjne w HelMA

Główna zmiana polegała na dodaniu `| c -> e` do [Klasy Typu] [Stack].
```haskell
class (Semigroup c , Show c) => Stack e c | c -> e where
  fromList   :: [e] -> c
  empty      :: c
  indexMaybe :: c -> Index -> Maybe e
  lookup     :: Index -> c -> Maybe e
  splitAt    :: Index -> c -> (c , c)
  drop       :: Index -> c -> c
  pop1       :: c -> (e , c)
  pop2       :: c -> (e , e , c)
  indexMaybe = flip lookup
  lookup     = flip indexMaybe
```
Przy okazji poprawiłem trochę sygnatury metod.

Musimy też poprawić nasze implementacje (instancje):
```haskell
instance Show e => Stack e [e] where
  fromList             = id
  empty                = []
  lookup        i   c  = c !!? i
  splitAt       i   c  = List.splitAt i c
  drop          i   c  = List.drop i c
  pop1         (e : c) = (e , c)
  pop1              c  = error $ "Empty c " <> show c
  pop2    (e : e' : c) = (e , e', c)
  pop2              c  = error $ "Empty c " <> show c

instance Show e => Stack e (Seq e) where
  fromList                 = Seq.fromList
  empty                    = Seq.empty
  lookup         i      c  = Seq.lookup i c
  splitAt        i      c  = Seq.splitAt i c
  drop           i      c  = Seq.drop i c
  pop1          (e :<|  c) = (e , c)
  pop1                   c = error $ "Empty c " <> show c
  pop2    (e :<| e' :<| c) = (e , e', c)
  pop2                  c  = error $ "Empty c " <> show c
```

Od tej pory nie potrzebujemy już rzutować elementu stosu na `Symbol`,
a więc wiele metod pomocniczych do manipulacji stosem możemy przenieść bezpośrednio do modułu [Stack]:
```haskell
-- Stack instructions

halibut :: (Integral e , Stack e c) => c -> c
halibut c
  | i <= 0    = copy (negate i) c'
  | otherwise = move i c'
    where
      i = fromIntegral e
      (e , c') = pop1 c

move :: Stack e c => Index -> c -> c
move i c = c1 <> c2 <> c3 where
  (c1 , c3) = splitAt 1 c'
  (c2 , c') = splitAt i c

swap :: Stack e c => c -> c
swap c = push2 e' e c' where (e , e', c') = pop2 c

discard :: Stack e c => c -> c
discard = drop 1

slide :: Stack e c => Index -> c -> c
slide i c = push1 e (drop i c') where (e , c') = pop1 c

dup :: Stack e c => c -> c
dup = copy 0

copy :: Stack e c => Index -> c -> c
copy i c = push1 (c `index` i) c

-- Push instructions

pushChar1 :: (Num e , Stack e c) => Char -> c -> c
pushChar1 = genericPush1 . ord

genericPush1 :: (Integral v , Num e , Stack e c) => v -> c -> c
genericPush1 = push1 . fromIntegral

push1 ::  Stack e c => e -> c -> c
push1 e = pushList [e]

push2 :: Stack e c => e -> e -> c -> c
push2 e e' = pushList [e , e']

pushList :: Stack e c => [e] -> c -> c
pushList es c = fromList es <> c

----

index :: (Stack e c) => c -> Int -> e
index c i = check (c `indexMaybe` i) where
  check (Just e) = e
  check  Nothing = error $ "Empty stack " <> show c <> " index " <> show i
```

Możemy też zdefiniować operacje arytmetyczne:
```haskell
-- Arithmetic

divMod :: (Integral e , Stack e c) => c -> c
divMod = binaryOps [Mod , Div]

sub :: (Integral e , Stack e c) => c -> c
sub = binaryOp Sub

binaryOp :: (Integral e , Stack e c) => BinaryOperator -> c -> c
binaryOp op = binaryOps [op]

binaryOps :: (Integral e , Stack e c) => [BinaryOperator] -> c -> c
binaryOps ops c = pushList (calculateOps e e' ops) c' where (e , e', c') = pop2 c
```

W tym celu wydzielimy moduł [BinaryOperator]:
```haskell
module HelVM.HelMA.Common.BinaryOperator where

calculateOps :: Integral a => a -> a -> [BinaryOperator] -> [a]
calculateOps operand operand' = map (calculateOp operand operand')

calculateOp :: Integral a => a -> a -> BinaryOperator -> a
calculateOp operand operand' operation = doBinary operation operand' operand

doBinary :: Integral a => BinaryOperator -> a -> a -> a
doBinary Add = (+)
doBinary Sub = (-)
doBinary Mul = (*)
doBinary Div = div
doBinary Mod = mod

data BinaryOperator = Add | Sub | Mul | Div | Mod
  deriving (Eq , Show , Read)
```

## Dużo małych Klas Typów

Mam wrażenie,
że ten kod dalej nie jest odpowiednio polimorficzny.
Stworzyliśmy jeden wielki interfejs.
Co,
jednak gdybyśmy chcieli stworzyć klasyczny stos tylko z funkcjami `push` i `pop`?
Musielibyśmy wszystko pisać od początku.
Programowanie byłoby o wiele łatwiejsze,
gdybyśmy mieli polimorficzne funkcje dla kolekcji podobnie,
tak jak mamy polimorficzną metodę `fmap` zdefiniowaną w [Funktorze].

Dzięki polimorfizmowi nie musimy pisać w kodzie `List.fmap`, `Seq.fmap` czy `IntMap.fmap`,
tylko wybór odpowiedniej implementacji jest ustalana na podstawie parametru.
Miło by było mieć tak samo polimorficzne metody  `drop`, `empty`, `fromList`, `index`, `insert`, `lookup` i `splitAt`.

W tym celu tworzymy folder `HelMA.Common.Collections` i umieszczamy w nim następujące [Klasy Typów]:

[FromList] z metodami `fromList` i `empty`,
żeby tworzyć kolekcję na podstawie listy:
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

[Lookup] z metodą `lookup`,
żeby wyszukiwać elementy w kolekcji po indeksie:
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

[Insert] z metodą `insert`,
żeby wstawiać elementy do kolekcji:
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

[Pop] z metodą `pop1` i `pop2`,
żeby pobierać ze szczytu stosu:
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

Plus jeszcze [Drop] i [Pop] z nudną implementacją.
```haskell
module HelVM.HelMA.Common.Collections.Drop where

import Prelude  hiding (drop)

import qualified Data.Sequence as Seq
import qualified Prelude       as List (drop)

class Drop e c | c -> e where
  drop :: Int -> c -> c

instance Drop e [e] where
  drop i c = List.drop i c

instance Drop e (Seq e) where
  drop i c = Seq.drop i c
```
```haskell
module HelVM.HelMA.Common.Collections.SplitAt where

import Prelude  hiding (splitAt)

import qualified Data.Sequence as Seq
import qualified Prelude       as List (splitAt)

class SplitAt e c | c -> e where
  splitAt  :: Int -> c -> (c , c)

instance SplitAt e [e] where
  splitAt i c = List.splitAt i c

instance SplitAt e (Seq e) where
  splitAt i c = Seq.splitAt i c
```

Teraz importujemy wszystkie potrzebne metody do modułu `Stack`:
```haskell
import HelVM.HelMA.Common.Collections.Drop
import HelVM.HelMA.Common.Collections.FromList
import HelVM.HelMA.Common.Collections.Lookup
import HelVM.HelMA.Common.Collections.Pop
import HelVM.HelMA.Common.Collections.SplitAt
```

Musimy jeszcze ukryć przeszkadzające nam funkcje:
```haskell
import Prelude hiding (divMod , drop , empty , fromList , splitAt , swap)
```

Niby osiągnęliśmy cel,
jednak pisanie wysokopoziomowych metod to jakaś tragedia:
```haskell
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

Naprawdę sygnaturą metody `halibut :: (Show c , Semigroup c , Integral e , FromList e c , Lookup e c , SplitAt e c , Pop1 e c) => c -> c` można straszyć dzieci.

Cała implementacja jest w pliku [StackUtil].
Czemu `Util`?
Bo nie mamy żadnego wspólnego interfejsu,
tylko zbiór przypadkowych metod :(

## Wszystkie metody w jednej Klasie Typu z jedną implementacją
Pomysł jest prosty.
Za pomocą małych [Klas Typów] zdefiniujemy nową implementację [Klasy Typu] `Stack`.

Najpierw importujemy wszystkie potrzebne funkcje do `I` (jak `Implementation`):
```haskell
import qualified HelVM.HelMA.Common.Collections.Drop     as I
import qualified HelVM.HelMA.Common.Collections.FromList as I
import qualified HelVM.HelMA.Common.Collections.Lookup   as I
import qualified HelVM.HelMA.Common.Collections.Pop      as I
import qualified HelVM.HelMA.Common.Collections.SplitAt  as I
```

I oczywiście ukryjmy domyślne importowane funkcje:
```haskell
import Prelude hiding (divMod , drop , empty , fromList , splitAt , swap)
```

Następnie tworzymy naszą [Klasę Typów]:
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
```

I jedną implementację (instancję) dla niej:
```haskell
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

Niestety musimy dodać rozszerzenie kompilatora `{-#LANGUAGE UndecidableInstances#-}`,
co nie jest fajne.

Cała implementacja jest w pliku [StackImpl].
Czemu `*Impl`?
Bo przypomina to [Javową] patologię z Serwisami z jedną implementacją `*ServiceImpl`.

## Sumowanie ograniczeń

Gdy już byłem zdołowany,
że zostawię projekt z bezsensowną [Klasą Typu],
z jedną implementacją to przypadkiem przeczytałem,
że w [Haskellu] ograniczenia rodzai mogą być *obywatelami pierwszej kategori*.
Wystarczy łączyć rozszerzenie `{-#LANGUAGE ConstraintKinds#-}`.

Po włączeniu rozszerzenia importujemy wszystkie potrzebne nam metody:
```haskell
import HelVM.HelMA.Common.Collections.Drop
import HelVM.HelMA.Common.Collections.FromList
import HelVM.HelMA.Common.Collections.Lookup
import HelVM.HelMA.Common.Collections.Pop
import HelVM.HelMA.Common.Collections.SplitAt
```

I ponownie ukrywamy przeszkadzające nam funkcje:
```haskell
import Prelude hiding (divMod , drop , empty , fromList , splitAt , swap)
```

A następnie piszemy jedną magiczną linię:
```haskell
type Stack e c = (Show c , Semigroup c , Drop e c , FromList e c , Lookup e c , SplitAt e c , Pop1 e c , Pop2 e c)
```
Właśnie zsumowaliśmy wszystkie ograniczenia do jednego typu `Stack`.

I teraz można żyć.
I teraz da się pracować.

Cały kod jest w pliku [StackConst] (`Const` jak `Constraint`).

## Podsumowanie

Stworzenie interfejsu kolekcji w **[Haskellu]** nie jest jednak trudne.
Wystarczy wiedzieć,
czego się szuka i znaleźć to :)
[zależności Funkcyjne] są niesamowitym narzędziem pozwalającym pisać bardzo elastyczny i polimorficzny kod.

Kod jednak dalej nie jest idealny.
Co można jeszcze poprawić?
* Przepisać [Relude] na [RIO] - uniknie się w ten sposób importów ukrywających.
* Przepisać [Zależności Funkcyjne] na [Rodziny Typów] - rodziny typów są bardziej restrykcyjne i popularniejsze.
* Użyć ograniczenia istniejące już w [Haskellu] jak `IsList`.
* Użyć biblioteki polimorficznych importów jak [ClassyPrelude].

Cały kod interpretera [HelMA] jest dostępny na [githabie](https://github.com/helvm/helma/tree/v0.6.6.0).

[Abstrakcja i dopasowanie do wzorców]:            /pattern-matching

[Haskell]:                     /langs/haskell
[Haskella]:                    /langs/haskell
[Haskellu]:                    /langs/haskell
[Javową]:                      /langs/java

[ClassyPrelude]:               /libs/classy-prelude
[Relude]:                      /libs/relude
[RIO]:                         /libs/rio

[HelMA]:                       /projects/helma

[Funktorze]:                   /tags/functor
[Klas Typów]:                  /tags/type-class
[Klasą Typu]:                  /tags/type-class
[Klasę Typów]:                 /tags/type-class
[Klasę Typu]:                  /tags/type-class
[Klasy Typów]:                 /tags/type-class
[Klasy Typu]:                  /tags/type-class
[OOP]:                         /tags/oop
[Rodziny Typów]:               /tags/type-family
[Typy zależne]:                /tags/dependent-types
[Wieloparametrowa klasa typu]: /tags/multi-parameter-type-class
[Zależności funkcyjne]:        /tags/functional-dependency
[Zależności Funkcyjnych]:      /tags/functional-dependency

[BinaryOperator]: https://github.com/helvm/helma/blob/v0.6.6.0/hs/src/HelVM/HelMA/Common/BinaryOperator.hs

[Drop]:           https://github.com/helvm/helma/blob/v0.6.6.0/hs/src/HelVM/HelMA/Common/Collections/Drop.hs
[FromList]:       https://github.com/helvm/helma/blob/v0.6.6.0/hs/src/HelVM/HelMA/Common/Collections/FromList.hs
[Insert]:         https://github.com/helvm/helma/blob/v0.6.6.0/hs/src/HelVM/HelMA/Common/Collections/Insert.hs
[Lookup]:         https://github.com/helvm/helma/blob/v0.6.6.0/hs/src/HelVM/HelMA/Common/Collections/Lookup.hs
[SplitAt]:        https://github.com/helvm/helma/blob/v0.6.6.0/hs/src/HelVM/HelMA/Common/Collections/SplitAt.hs
[Pop]:            https://github.com/helvm/helma/blob/v0.6.6.0/hs/src/HelVM/HelMA/Common/Collections/Pop.hs
[Push]:           https://github.com/helvm/helma/blob/v0.6.6.0/hs/src/HelVM/HelMA/Common/Collections/Push.hs

[RAM]:            https://github.com/helvm/helma/blob/v0.6.6.0/hs/src/HelVM/HelMA/Common/Memories/RAM.hs
[Stack]:          https://github.com/helvm/helma/blob/v0.6.6.0/hs/src/HelVM/HelMA/Common/Memories/Stack.hs
[StackConst]:     https://github.com/helvm/helma/blob/v0.6.6.0/hs/src/HelVM/HelMA/Common/Memories/StackConst.hs
[StackImpl]:      https://github.com/helvm/helma/blob/v0.6.6.0/hs/src/HelVM/HelMA/Common/Memories/StackImpl.hs
[StackUtil]:      https://github.com/helvm/helma/blob/v0.6.6.0/hs/src/HelVM/HelMA/Common/Memories/StackUtil.hs

[constraint-kinds]:                          http://dev.stephendiehl.com/hask/#constraint-kinds
[Functional dependencies vs. type families]: https://wiki.haskell.org/Functional_dependencies_vs._type_families
