---
title:    'Hermetyzacja w Haskellu'
author:   TheKamilAdam
category: haskell-eta
langs:    haskell
libs:     containers
eso:      subleq whitespace
projects: helma
redirect_from:
- encapsulation
- haskell-eta/encapsulation
---

Hermetyzacja to cecha charakterystyczna obiektowych języków programowania.
Co jednak nie znaczy,
że hermetyzacja nie istnieje w **[Haskellu]**.
Hermetyzację podobną do klas z języków OOP osiąga się w **[Haskellu]** za pomocą modułów i jawnego eksportowania funkcji i typów.

Jako przykład przedstawię trzy implementacje modułu emulującego pamięć o dostępie swobodnym (RAM) dla interpreterów ezoterycznych języków programowania.
Moduły są użyte w interpreterach języków **[SubLeq]** i **[WhiteSpace]**.

## Implementacje

Implementacje są przedstawione w takiej kolejnosci jak powstawały wraz z poznawaniem **[Haskella]**.
Oparte są na Liście, Sekwencji i IntMapie.

### Implementacja oparta na liście
Najpierw implementacja oparta na liście.
Była to dla mnie naturalna implementacja,
ponieważ list jest domyślną (jedyną dostępną w standardowej bibliotece) kolekcją danych w **[Haskellu]**.

Najpierw eksportujemy typ `RAM` oraz nazwy czterech funkcji pracujących na tym typie.
Dwa konstruktory oraz *mutatory*.
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
Ponieważ dla wszystkich implementacji ten kod jest prawie identyczny,
nie będę go już powtarzać.

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
Funkcja `empty` tworzy pusty `RAM` a `fromList` - ram wstępnie załadowany danymi.


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

O ile odczyt elementów z listy jest prosty,
o tyle zapis elementu w liście wymaga trochę kodu.
Wymaga także przekopiowania fragmentu listy.
W przypadku dodania nowego elementu na koniec listy wymaga przekopiowania całej zawartości listy.
Czyli czas wstawiania to `O(n)`,
gdzie `n` to wartość adresu.
Nie jest to dobra implementacja,
ale jej zaletą jest to,
że nie zależy od zewnętrznych bibliotek.

### Implementacja oparta na Sekwencji

Importujemy sekwencje jako `Seq` oraz definiujemy typ i dwa konstruktory do niego:
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

Następnie przekopiowujemy kod,
który powinien być wspólny,
dwa *mutatory* `load` i `store`.
(Do poprawy w przyszłości)

Na koniec definiujemy funkcje zależne od implementacji:
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

Także tutaj kod zapisu to pare linii.
Jednak tym razem nie musimy kopiować żadnych struktur.
Sekwencja zajmie się tym za nas. 
Jednocześnie gwarantując wydajność stawiania nowego elementu na poziomie `O(log n)`.

### Implementacja oparta na Mapie

Nie będzie to jednak zwykła mapa a IntMapa dedykowana do adresowania jej typem `Int`.
Ograniczenie to wynika z tego,
że zarówno Lista,
jak i Sekwencja,
także są indeksowane typem `Int`. 

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

Dla funkcji `fromList` trzeba napisać trochę kodu.
Wynika to z tego,
że `IntMap.fromList` przyjmuje pary indeks, wartość.

Implementacje `index'` i `insert'` są banalnie proste:
```haskell
-- Private
index' :: IntMap s -> Int -> Maybe s
index' = (!?)

insert' :: Int -> s -> IntMap s -> IntMap s
insert' = insert
```

Czy implementacja oparta na `IntMap` jest lepsza niż na `Seq`?
Tego nie wiem.
Typ `IntMap` nie ma podanych teoretycznych wartości w notacji dużego O.
Z drugiej strony nawet jakby posiadał,
to może się to różnie przekładać na rzeczywiste rezultaty.
Wszystko też zależy od ilości danych.
Potrzebny byłby duży program w języku **[SubLeq]**.
I porządne testy wydajnościowe.

Ważne jest to, że udało się z hermetyzować użycie Listy, Sekwencji i IntMapy


## Użycie

Jako przykład interpreter języka **[SubLeq]**:

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

Udało się ukryć szczegóły implementacyjne wewnątrz modułu oraz możemy dokonać wyboru między implementacjami zmieniając jedną linie:
```haskell
import HelVM.HelCam.Common.RAM.ListRAM as RAM
import HelVM.HelCam.Common.RAM.SeqRAM as RAM
import HelVM.HelCam.Common.RAM.IntMapRAM as RAM
```
Nie jest to jednak rozwiązanie idealne.
Nie możemy zmieniać implementacji podczas działania programu.
Także wstrzykiwanie różnych implementacji na potrzeby testów jest utrudnione.

Kod interpretera [Helcam] po zmianach znajduje się [tutaj].

[Haskell]:              /langs/haskell
[Haskella]:             /langs/haskell
[Haskellu]:             /langs/haskell

[SubLeq]:               /eso/subleq
[WhiteSpace]:           /eso/whitespace

[Helcam]:               /projects/helma

[tutaj]:                https://github.com/helvm/helma/tree/v0.6.2.0
