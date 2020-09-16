---
title:    'Haskell Pipe operator i inne operatory'
author:   TheKamilAdam
category: haskell-eta
tags:     assembler cisc dsl lexer misc parser regexp risc rpn
langs:    dc forth haskell joy mouse perl postscript rpl webassembly
libs:     attoparsec happy-alex megaparsec parsec readp
tools:
projects: helcam helpa helvm
eso:      beatnik brainfuck eas eta false funge piet whitespace
redirect_from:
  - attoparsec
  - haskell-eta/attoparsec
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

https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Function.html

```haskell
f3 (f2 (f1 d))
```


### Operator dolara (ang. dollar operator)

```haskell
($) :: (a -> b) -> (a -> b)
```

```haskell
f3 $ f2 $ f1 d
```


Application operator. This operator is redundant, since ordinary application (f x) means the same as (f $ x). However, $ has low, right-associative binding precedence, so it sometimes allows parentheses to be omitted; for example:
et ampersand

```haskell
a -> (a -> b) -> b 
```

```haskell
d & f1 & f2 & f3
```

#### Składanie funkcji (ang. Function composition)
https://wiki.haskell.org/Function_composition

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
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

