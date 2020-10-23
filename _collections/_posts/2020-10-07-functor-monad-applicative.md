---
title:    'Funktor, Monada i Aplikatywa'
author:   TheKamilAdam
category: haskell-eta
tags:     applicative functor library monad operator parser type-class
langs:    haskell
libs:     semigroupoids
projects: helpa
eso:      eta
redirect_from:
  - functor-monad-applicative
  - haskell-eta/functor-monad-applicative
---

Trzy **[klasy typów]** **[Funktor]**, **[Monada]** i **[Aplikatywa]** są to prawdopodobnie trzy najpopularniejsze klasy typów do przetwarzania danych.

### Funktor (ang. Functor)

[Funktor w Haskellu](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Functor.html#t:Functor)
jest prawdopodobnie najprostszą **[klasą typu]** do przetwarzania danych.

Podstawą **[Funktora]** jest metoda `fmap`:
```haskell
fmap :: (a -> b) -> (f a -> f b)
```

`fmap` może wydawać się bezsensowny,
ponieważ pobiera funkcję i zwraca funkcję.
Jeśli jednak zapiszemy tę sygnaturę inaczej,
to nabierze to większego sensu:
```haskell
fmap :: (a -> b) -> f a -> f b
(<$>) :: (a -> b) -> f a -> f b
(<&>) :: f a -> (a -> b) -> f b
```

Teraz:
* Funkcja `fmap` pobiera dwa argumenty, funkcję mapującą i funktor do przemapowania.
* Operator `(<$>)` podobnie jak operator `($)` pozwala pomijać nawiasy.
* Operator `(<&>)` podobnie jak operator `(&)` pozwala pisać kod w stylu bardziej obiektowym.

Przykłady użycia:
```haskell
fmap (function1 data1)
fmap $ function1 data1
function1 <$> data1
data1 <&> function1
```

W moim parserze języka **[ETA]** preferuje operator `(<$>)`:
```haskell
unescapedStringParser :: Parser Instruction
unescapedStringParser = U <$> stringParser

labelDefinitionParser :: Parser Instruction
labelDefinitionParser = L <$> (char '>' *> identifierParser <* char ':')

includeFileParser :: Parser Instruction
includeFileParser = D <$> (char '*' *> fileNameParser <* char '\n')
```

Są jeszcze dwa pomocnicze operatory różniące się kolejnością argumentów:
```haskell
($>) :: f a -> b -> f b
(<$) :: a -> f b -> f a
```

Przykład użycia:
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

W moim **[parserze]** języka **[ETA]** preferuje operator `(<$)`:
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

[Monada w Haskellu](https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Monad.html#t:Monad)
jest bazą dla całej rodziny klas typów.
To właśnie nią straszy się niegrzeczne dzieci.
W praktyce **[Monada]** jest bardzo prosta i sprowadza się do zdefiniowania jednego operatora,
który występuje w dwóch wersjach:
```haskell
(>>=) :: m a -> (a -> m b) -> m b
(=<<) :: (a -> m b) -> m a -> m b
```
Pozwalają one na łączenie dwóch monad w jedną monadę.

Tu można się zdziwić,
bo dokumentacja **[Haskella]** preferuje operator `(>>=)`.
Mnie jednak bardziej pasuje operator `(=<<)`,
ponieważ pozwala czytać kod od prawej do lewej,
podobnie jak operatory `($)` i `(<$>)`.

Przykład z asemblera języka **[ETA]** wygląda następująco:
```haskell
replaceStrings :: InstructionList -> InstructionList
replaceStrings il = replaceString =<< il

replaceString :: Instruction -> InstructionList
```

### Aplikatywa (ang. Applicative)

[Aplikatywa w Haskellu](https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Applicative.html#t:Applicative) jest klasą pośrednią między Funktorem a Monadą.

**[Aplikatywa]** dziedziczy z **[Funktora]**,
a **[Monada]** dziedziczy z **[Aplikatywy]**.
W zasadzie jest to **[Monada]** bez operatorów `(>>=)` i `(=<<)`,
czyli bez możliwości składania. 

**[Aplikatywa]** posiada następujące operacje:
```haskell
pure :: a -> f a
(<*>) :: f (a -> b) -> f a -> f b
(<**>) :: f a -> f (a -> b) -> f b
```

Gdzie:
* Funkcja `pure` pozwala utworzyć **[Aplikatywę]**.
* Operator `(<*>)` jest bardzo podobny do `(<$>)`, ale tutaj także funkcja jest opakowana w kontekst.
* Operator `(<**>)` podobnie jak operator `(<&)` pozwala na zapis w stylu bardziej obiektowym.


Są jeszcze dwa operatory pomocnicze:
```haskell
(*>) :: f a -> f b -> f b
(<*) :: f a -> f b -> f a
```

Gdzie:
* Operator `(*>)` pozwala odrzucić pierwszy argument.
* Operator `(<*)` pozwala odrzucić drugi argument.

Tu kolejność jest ważna.
Pierwszy ignoruje pierwszą wartość.
Drugi operator ignoruje drugą wartość.

W przeciwieństwie do poprzednich par operatorów,
oba te operatory są potrzebne.

Przykład z asemblera języka **[ETA]** wygląda następująco:
```haskell
labelDefinitionParser :: Parser Instruction
labelDefinitionParser = L <$> (char '>' *> identifierParser <* char ':')

includeFileParser :: Parser Instruction
includeFileParser = D <$> (char '*' *> fileNameParser <* char '\n')
```
### Problemy z Aplikatywą

Nie zawsze było tak,
że **[Aplikatywa]** był klasą bazową dla **[Monady]**.
Aplikatywa została dodany później w związku z pracami nad **[parserami]** w **[Haskell]**.

Z tego powodu niektóre operacje z **[Aplikatywy]** są zdupkiowane w **[Monadzie]**.
Są to:
```haskell
pure = return
(<*>) = ap
(>>) = (*>)
```

Zawsze należy dbać o to,
żeby operacje te posiadały te same implementacje.

### Szczegóły implementacyjne w standardowej bibliotece Haskella

Nie wszystkie podane przeze mnie powyżej metody są zdefiniowane w **[klasach typów]**.
* **[Funktor]** posiada tylko metody `fmap` i `(<$)`,
przy czym do minimalnej definicji wystarczy tylko implementacja `fmap`.
`(<$>)` i `($>)` są funkcjami przyjmującymi **[Funktor]**.
* **[Aplikatywa]** posiada tylko metody `pure`, `(<*>)`, `lift`, `(*>)` oraz `(*>)`.
przy czym do minimalnej definicji wystarczy tylko implementacja `pure` oraz `(<*>)` lub `liftA2`.
`(<**>)` jest dostarczana jako funkcja przyjmująca **[Aplikatywę]**.
* **[Monad]** posiada tylko metody `(>>=)`, `(>>)`, `return` i `fail` ,
przy czym do minimalnej definicji wystarczy tylko implementacja `(>>=)`.
`(=<<)`, `ap` i wiele innych są dostarczane jako funkcje przyjmujące **[Monadę]**

### Inne problemy i inne implementacje 

Nie tylko hierarchia dziedziczenia między **[Aplikatywą]** a **[Monadą]** jest zepsuta w **[Haskellu]**.
Ogólnie cała hierarchia dziedziczenia jest popsuta (nie zawiera odpowiednio dużo kroków pośrednich).
Na szczęście jest biblioteka [semigroupoids](https://hackage.haskell.org/package/semigroupoids),
która rozwiązuje ten problem.
Implementuje ona wszystkie teoretycznie istniejące kroki pośrednie:
```
Foldable ----> Traversable <--- Functor ------> Alt ---------> Plus           Semigroupoid
     |               |            |                              |                  |
     v               v            v                              v                  v
Foldable1 ---> Traversable1     Apply --------> Applicative -> Alternative      Category
                                  |               |              |                  |
                                  v               v              v                  v
                                Bind ---------> Monad -------> MonadPlus          Arrow
```

Jest tylko jeden problem.
Posiada ona własną definicję [Funktora](https://hackage.haskell.org/package/semigroupoids/docs/Data-Functor-Apply.html#t:Functor).

## Podsumowanie

**[Haskell]** jest wspaniałym językiem programowania,
ale niestety nie wszystko w nim jest idealne.
Zmiany są jednak wprowadzane stopniowo,
nawet jeśli wiąże się to ze złamaniem kompatybilności.

[Haskell]:       /langs/haskell
[Haskella]:      /langs/haskell
[Haskellu]:      /langs/haskell

[semigroupoids]: /libs/semigroupoids

[ETA]:           /eso/eta

[Aplikatywa]:    /tags/applicative
[Aplikatywą]:    /tags/applicative
[Aplikatywę]:    /tags/applicative
[Aplikatywy]:    /tags/applicative
[Funktor]:       /tags/functor
[Funktora]:      /tags/functor
[klasą typu]:    /tags/type-class
[klasy typów]:   /tags/type-class
[klasach typów]: /tags/type-class
[Monad]:         /tags/monad
[Monada]:        /tags/monad
[Monadą]:        /tags/monad
[Monadę]:        /tags/monad
[Monady]:        /tags/monad
[Monadzie]:      /tags/monad
[parserze]:      /tags/parser
[parserami]:     /tags/parser
