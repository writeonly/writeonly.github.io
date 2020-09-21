---
title:    'Haskell Pipe operator i inne operatory'
author:   TheKamilAdam
category: haskell-eta
tags:     
langs:    haskell ocaml rust scala
libs:     flow
tools:
projects: 
eso:      
redirect_from:
  - pipe-operators
  - haskell-eta/pipe-operator
---



## Pipe operator

```scala
data
  .first_function()
  .second_function()
  .third_function()
```



```scala
val data1 = first_function(data)
val data2 = second_function(data1)
val data3 = third_function(data2)
```
```scala
third_function(second_function(first_function(data)))
```

### Pipe operator

```scala
data |> first_function |> second_function |> third_function
```

## Haskell i kombinatory



Haskell podobnie jak Scala czy OCaml nie używa żadnej magii tylko pozwala definiować operatory.



https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Function.html



```haskell
f d = f3 (f2 (f1 d))
```

### Operator dolara (ang. *Dollar operator*)
Operator dolara `($)` robi "apply forward" zwane też "pipe into".
Definicja:
```haskell
($) :: (a -> b) -> (a -> b)
```

Użycie:
```haskell
f d = f3 $ f2 $ f1 d
```

Powyższy zapis jest czytelniejszy niż zapis nawiasowy,
jednak dalej wymaga czytania od prawej do lewej,
co jest nienaturalne.

### Operator et (ang. *Ampersand operator*)
Operator et `(&)` robi "apply backward" zwane też "pipe from".

Definicja:
```haskell
a -> (a -> b) -> b 
```

Użycie:
```haskell
f d = d & f1 & f2 & f3
```

Jest to czytelniejsza postać dla użytkowników języków obiektowych oraz języka **[Rust]**,
jednak w Haskellu rzadko używana.

### Operator kropki (ang. *Dot operator*)
Operator kropki `(.)` służy do składania funkcji (ang. *[Function composition](https://wiki.haskell.org/Function_composition)*).

Definicja:
```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
```
Użycie:
```haskell
f d = (f3 . f2 . f1) d
```

Jednak nie jest to,
czego można by spodziewać się po programistach Haskella.
Haskell pozwala na *[PointFree](https://wiki.haskell.org/Pointfree) Style*,
czyli możliwość niezapisywania argumentów:
```haskell
f = f3 . f2 . f1
```

Styl PointFree bywa też nazywany stylem Pointless,
ponieważ jest oskarżany o zaciemnianie kodu.

### Pakiet Flow 

Pakiet [Flow](https://hackage.haskell.org/package/flow-1.0.21/docs/Flow.html) pozwala używać operatorów znanych z innych języków programowania.
Definiuje on dwa operatory aplikacji oraz dwa operatory kompozycji, które w uproszczeniu można wyjaśnić jako:
```haskell
(<|) = ($)      -- "apply forward"    or "pipe into"
(|>) = (&)      -- "apply backward"   or "pipe from"
(<.) = (.)      -- "compose backward" or "but first"
(.>) = flip (.) -- "compose forward"  or "and then"
```

## Inne operatory

### Funktor (ang. Funktor)
https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Functor.html#t:Functor

Podstawą funktora jest funkcja (metoda?) `fmap`:
```haskell
fmap :: f => (a -> b) -> (f a -> f b)
```
`fmap` może wydawać się bezsensowny ponieważ pobiera funkcję i zwraca funkcję.
Jeśli jednak zapiszemy tę sygnaturę inaczej to nabierze to większego sensu:
```haskell
fmap :: f => (a -> b) -> f a -> f b
(<$>) :: f => (a -> b) -> f a -> f b 
(<&>) :: f => f a -> (a -> b) -> f b 
```

Teraz funkcja `fmap` pobiera dwa argumenty, funkcję mapującą i funktor do przemapowania.
Operator `<&>` pozwala pisać kod w stylu bardziej obiektowym. 


```haskell
fmap function1 data1
function1 <$> data1
data1 <&> function1
```

```haskell
unescapedStringParser :: Parser Instruction
unescapedStringParser = U <$> stringParser

labelDefinitionParser :: Parser Instruction
labelDefinitionParser = L <$> (char '>' *> identifierParser <* char ':')

includeFileParser :: Parser Instruction
includeFileParser = D <$> (char '*' *> fileNameParser <* char '\n')
```


```haskell
($>) :: f => f a -> b -> f b 
(<$) :: f => a -> f b -> f a
```

```haskell
>>> Nothing $> "Haskell"
Nothing
>>> Just "Scala" $> "Haskell"
Just "haskell"
>>> "Haskell" <$ Nothing
Nothing
>>> "Haskell" <$ Just "Scala"
Just "Haskell"
```


```haskell
zeroOperandInstructionParser :: Parser Instruction
zeroOperandInstructionParser =
      zeroOperandInstruction E ["E", "dividE"]
  <|> zeroOperandInstruction T ["T", "Transfer"]
  <|> zeroOperandInstruction A ["A", "Address"]
  <|> zeroOperandInstruction O ["O", "Output"]
  <|> zeroOperandInstruction I ["I", "Input"]
  <|> zeroOperandInstruction S ["S", "Subtract"]
  <|> zeroOperandInstruction H ["H", "Halibut"]
    where zeroOperandInstruction i ts = i <$ (asciiCIChoices ts *> endWordParser)
```


### Monada (ang. Monad)

https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Monad.html#t:Monad
```haskell
(>>=) :: m => m a -> (a -> m b) -> m b
(=<<) :: m => (a -> m b) -> m a -> m b
```

Tu można się zdziwić.

```haskell
replaceStrings :: InstructionList -> InstructionList
replaceStrings il = replaceString =<< il

replaceString :: Instruction -> InstructionList
```

### Aplikatyw (ang. Applicative)



https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Applicative.html#t:Applicative
app
```haskell
(<*>) :: f (a -> b) -> f a -> f b 
(<**>) :: f => f a -> f (a -> b) -> f b
```


```haskell
(*>) :: f a -> f b -> f b
(<*) :: f a -> f b -> f a
```

Tu kolejność jest ważna. 
Pierwszy ignoruje pierwszą wartość.
Drugi operator ignoruje drugą wartość.

```haskell
labelDefinitionParser :: Parser Instruction
labelDefinitionParser = L <$> (char '>' *> identifierParser <* char ':')

includeFileParser :: Parser Instruction
includeFileParser = D <$> (char '*' *> fileNameParser <* char '\n')
```

### Problemy z Aplikatiwem

```
pure = return
(>>) = (*>)
```

### Inne problemy

```
Foldable ----> Traversable <--- Functor ------> Alt ---------> Plus           Semigroupoid
     |               |            |                              |                  |
     v               v            v                              v                  v
Foldable1 ---> Traversable1     Apply --------> Applicative -> Alternative      Category
                                  |               |              |                  |
                                  v               v              v                  v
                                Bind ---------> Monad -------> MonadPlus          Arrow

https://hackage.haskell.org/package/semigroupoids
```


[OCcaml]:  /langs/ocaml
[Scala]:   /langs/scala

