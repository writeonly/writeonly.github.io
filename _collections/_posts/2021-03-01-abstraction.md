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


```haskell
{-# Language FlexibleInstances     #-}
{-# Language MultiParamTypeClasses #-}
{-# Language AllowAmbiguousTypes   #-}
```

```haskell
module HelVM.HelCam.Common.RAM (
  RAM,
  HelVM.HelCam.Common.RAM.empty,
  HelVM.HelCam.Common.RAM.fromList,
  load,
  store
) where

import Data.Default
import Data.IntMap as IntMap
import Data.Sequence as Seq

type Address = Int
```

Następnie mamy abstrakcję.
```haskell
load :: (Integral a, Default s, RAM s m) => m -> a -> s
load memory address = index' memory (fromIntegral address) ?: def

store :: (Integral a, Default s, RAM s m) => a -> s -> m -> m
store address = insert' (fromIntegral address)

class (Default s, Semigroup m) => RAM s m where
  fromList :: [s] -> m
  empty    :: m
  index'   :: m -> Address -> Maybe s
  insert'  :: Address -> s -> m -> m
```

Ostateczna część to implementacja:
```haskell
instance (Default s) => RAM s [s] where
  fromList = id
  empty    = []
  index'   = (!!?)
  insert' 0       symbol []     = [symbol]
  insert' 0       symbol (_:xs) = symbol : xs
  insert' address symbol []     = def    : insert' (address-1) symbol []
  insert' address symbol (x:xs) = x      : insert' (address-1) symbol xs

instance (Default s) => RAM s (Seq s) where
  fromList = Seq.fromList
  empty    = Seq.fromList []
  index'   = (Seq.!?)
  insert' address symbol memory = insert'' (Seq.length memory) where
    insert'' l
      | address < l = Seq.update address symbol memory
      | otherwise   = memory <> Seq.replicate (address - l) def |> symbol

instance (Default s) => RAM s (IntMap s) where
  fromList list = IntMap.fromList $ Prelude.zip [0..] list
  empty         = IntMap.empty
  index'        = (IntMap.!?)
  insert'       = IntMap.insert
```

https://wiki.haskell.org/Multi-parameter_type_class

## Użycie

Nowy kod Type classy `Evaluator` wygląda następująco:
```haskell
class Evaluator r where
  simpleEval :: Source -> r
  simpleEval source = eval source defaultRAMType

  simpleEvalIL :: SymbolList -> r
  simpleEvalIL il = evalIL il defaultRAMType

  eval :: Source -> RAMType -> r
  eval source = evalIL $ tokenize source

  evalIL :: SymbolList -> RAMType -> r
  evalIL il ListRAMType   = doInstruction 0 (RAM.fromList il::SymbolList)
  evalIL il SeqRAMType    = doInstruction 0 (RAM.fromList il::Seq Symbol)
  evalIL il IntMapRAMType = doInstruction 0 (RAM.fromList il::IntMap Symbol)

  doInstruction :: RAM Symbol m => Symbol -> m -> r
  doInstruction ic memory
    | ic  < 0   = doEnd
    | src < 0   = doInputChar  dst ic memory
    | dst < 0   = doOutputChar src ic memory
    | otherwise = doInstruction ic' $ store dst diff memory
      where
        src  = load memory ic
        dst  = load memory $ ic + 1
        diff = load memory dst - load memory src :: Symbol
        ic'
          | diff <= 0 = (load memory $ ic + 2) :: Symbol
          | otherwise = ic + 3

  doEnd        :: r
  doInputChar  :: RAM Symbol m => Symbol -> Symbol -> m -> r
  doOutputChar :: RAM Symbol m => Symbol -> Symbol -> m -> r
```


Ponieważ odkryłem zapis `instance (TypeClassa1 t) => TypeClassa2 t where` możemy przy okazji przepisać implementacje Evaluator
```haskell
instance (WrapperIO m) => Evaluator (m ()) where
  doEnd = pass

  doInputChar address ic memory = do
    value <- wGetInt
    doInstruction (ic+3) $ store address value memory

  doOutputChar address ic memory = do
    wPutInt (load memory address :: Symbol)
    doInstruction (ic+3) memory
```


Potrzebujemy jeszcze kostruktury (enuma) dającą możliwość wyboru implementacji: 
```haskell
module HelVM.HelCam.Common.Types.RAMType where

data RAMType = ListRAMType | SeqRAMType | IntMapRAMType deriving (Eq, Read, Show)

ramTypes :: [RAMType]
ramTypes = [ListRAMType, SeqRAMType, IntMapRAMType]

defaultRAMType :: RAMType
defaultRAMType = IntMapRAMType

computeRAMType :: String -> RAMType
computeRAMType raw = valid $ readMaybe raw where
  valid (Just value)  = value
  valid Nothing = error $ "RAMType '" <> toText raw <> "' is not valid RAMType. Valid ramTypes are : " <> show ramTypes
```


## Alternatywy

https://wiki.haskell.org/GHC/Type_families
