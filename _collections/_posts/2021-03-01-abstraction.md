---
title:    'Abstrakcja w Haskellu'
author:   TheKamilAdam
category: haskell-eta
langs:    haskell java scala
libs:     containers
eso:      subleq whitespace
tags:     abstraction interface oop trait typeclass
redirect_from:
- encapsulation
- haskell-eta/encapsulation
---

Po [Hermetyzacji] pora na Abstrakcje.
Abstrakcja ma w programowaniu wiele znaczeń.
Jednak w tym artykule będzie mi chodzić o abstrakcję spotykaną w OOP,
czyli interfejsy (w Javie), traity (w Scali) czy klasy czysto abstrakcyjne (w C++).
Czy Haskell ma odpowiednik interfejsów/traitów?
Tak są to type classy. 

{-# Language FlexibleInstances #-}
module HelVM.HelCam.Common.RAM where

import HelVM.HelCam.Machines.WhiteSpace.Instruction

import HelVM.HelCam.Common.OrError
import HelVM.HelCam.Common.Util

import Data.Default
import Data.IntMap as IntMap
import Data.Sequence as Seq
import Data.Sequence (Seq (..))

load :: Memory m => m -> Symbol -> Symbol
load heap address = load' heap (fromIntegral address) ?: def

store :: Memory m => Symbol -> Symbol -> m -> m
store address = store' (fromIntegral address)

storeNum :: Memory m => Symbol -> Input -> m -> m
storeNum address line = store address (readOrError line :: Symbol)

class (Semigroup m, Show m) => Memory m where
fromList' :: [Symbol] -> m
empty     :: m
load'     :: m -> Index -> Maybe Symbol
store'    :: Index -> Symbol -> m -> m

instance Memory [Symbol] where
fromList' = id
empty     = []
load'     = (!!?)
store'    = insert'

insert' :: Default v => Int -> v -> [v] -> [v]
insert' 0       value []     = [value]
insert' 0       value (_:xs) = value : xs
insert' address value []     = def   : insert' (address-1) value []
insert' address value (x:xs) = x     : insert' (address-1) value xs

instance Memory (Seq Symbol) where
fromList' = Seq.fromList
empty     = Seq.fromList []
load'     = (Seq.!?)
store' address value heap = insert' (Seq.length heap) where
insert' l
| address < l  = Seq.update address value heap
| address == l = heap |> value
| otherwise    = heap <> Seq.replicate (address - l) def |> value

instance Memory (IntMap Symbol) where
fromList' list = IntMap.fromList $ Prelude.zip [0..] list
empty  = IntMap.empty
load'  = (IntMap.!?)
store' = IntMap.insert



