---
title:    'Problem wywołań cebulowych w Haskellu'
author:   TheKamilAdam
category: haskell-eta
tags:     operator onion
langs:    haskell ocaml rust scala
libs:     scalaz zio
tools:
projects:
eso:
redirect_from:
  - pipe-operators
  - haskell-eta/pipe-operators
---

Przez `Problem wywołań cebulowych` rozumiem sytuację zagnieżdżonego wywoływania różnego rodzaju funkcji jak:
```haskell
someFunction someData = thirdFunction (secondFunction (firstFunction someData))
```

Duża ilość wywołań powoduje dużą ilość nawiasów na końcu:
```haskell
f x = f9 (f8 (f7 (f6 (f5 (f4 (f3 (f2 (f1 (f0 x)))))))))
```
co jest mało czytelne.

W **[Haskellu]** można zapisać to trochę czytelniej za pomocą klauzuli `where`:
```haskell
someFunction someData = thirdFunction secondDate where 
  secondData = secondFunction firstData
  firstData = firstFunction someData
```

Niestety klauzula `where` może powodować dużą rozwlekłość kodu:
```haskell
f x = f9 x8 where 
  x8 = f8 x7
  x7 = f7 x6
  x6 = f6 x5
  x5 = f5 x4
  x4 = f4 x3
  x3 = f3 x2
  x2 = f2 x1
  x1 = f1 x
```
Dodatkowo tworzymy dużą ilość pośrednich zmiennych,
które tylko zaciemniają kod.

W językach obiektowych (lub wspierających notację obiektową jak **[Rust]**) mamy szansę na lepszy zapis,
jeśli `firstFunction` jest metodą obiektu `firstData`,
`secondFunction` jest metodą obiektu `secondData`,
a `thirdFunction` jest metodą obiektu `thirdData`:
```scala
def someFunction(someDate: SomeDate): SomeResult = someData
  .firstFunction()
  .secondFunction()
  .thirdFunction()
```

Niestety nie zawsze mamy takie szczęście i wtedy kod wygląda mniej więcej tak:
```scala
def someFunction(someData: SomeDate): SomeResult = {
  val firstData = firstFunction(someData)
  val secondDate = secondFunction(firstData)
  third_function(secondDate)
}
```

Lub w krótszej formie bez zmiennych pośrednich tak:
```scala
def someFunction(someData: SomeDate): SomeResult = third_function(secondFunction(firstData(someData)))
```

W **[Scali]** można jeszcze próbować dodać metodę do obiektu za pomocą konwersji `implicit`,
jednak może wymagać to dużej ilości kodu,
a zysk jest raczej mały.
Chociaż czasem jest to używane w bibliotekach jak [Scalaz] czy [Zio].

Na szczęście można rozwiązać to za pomocą operatora potoku,
zwanego też w **[Scali]** operatorem drozda:
```scala
def someFunction(someData: SomeDate): someData |> firstFunction |> secondFunction |> thirdFunction
```
Pisałem o tym w artykule [Problem wywołań cebulowych w Scali](/scalapipe).

## Operatory aplikacji (i kombinacji)

**[Haskell]** także posiada swoje operatory potoku.
Jednak dla *utrudnienia* nazywają się one operatorami aplikacji.
Znajdują się one w module [Data.Function](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Function.html).

Podobnie jak w przypadku języków **[Scala]** czy **[OCaml]**,
w **[Haskell]** operatory te nie są częścią składni języka,
tylko zdefiniowane są tak jak funkcje.
Dzięki temu,
że te języki programowania mają elastyczną składnię można definiować własne operatory.

W dalszej części artykułu będę poszukiwać czytelniejszej formy dla funkcji:
```haskell
f x = f3 (f2 (f1 x))
```

### Operator dolara (ang. *Dollar operator*)
Pierwszym operatorem aplikacji jest operator dolara `($)`.
Operator ten robi "apply forward" zwane też "pipe into".
Definicja:
```haskell
($) :: (a -> b) -> (a -> b)
```

Użycie operatora dolara wygląda następująco:
```haskell
f d = f3 $ f2 $ f1 d
```

Jest to odpowiednik z innych języków programowania:
```haskell
f d = f3 <| f2 <| f1 <| d
```

Powyższy zapis jest czytelniejszy niż zapis nawiasowy,
jednak dalej wymaga czytania od prawej do lewej,
co jest nienaturalne.

### Operator et (ang. *Ampersand operator*)
Drugim operatorem aplikacji jest operator et `(&)`.
Operator ten robi "apply backward" zwane też "pipe from".
Definicja:
```haskell
a -> (a -> b) -> b
```

Użycie operatora et wygląda następująco:
```haskell
f d = d & f1 & f2 & f3
```

Jest to odpowiednik z innych języków programowania:
```haskell
f d = d |> f1 |> f2 |> f3
```

Jest to czytelniejsza postać dla użytkowników języków obiektowych oraz języka **[Rust]**,
jednak w Haskellu rzadko używana.

### Operator kropki (ang. *Dot operator*)
Operator kropki `(.)` służy do składania funkcji (ang. *[Function composition](https://wiki.haskell.org/Function_composition)*).
Jest to operator kompozycji.
Definicja:
```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
```
I użycie w **[Haskellu]** jakiego moglibyśmy się spodziewać:
```haskell
f d = (f3 . f2 . f1) d
```

Jednak nie jest to to,
czego można by spodziewać się po programistach **[Haskella]**.
Haskell pozwala na *[PointFree](https://wiki.haskell.org/Pointfree) Style*,
czyli możliwość niezapisywania argumentów:
```haskell
f = f3 . f2 . f1
```

Styl PointFree bywa też nazywany stylem Pointless,
ponieważ jest oskarżany o zaciemnianie kodu.

## Pakiet Flow
Pakiet [Flow](https://hackage.haskell.org/package/flow-1.0.21/docs/Flow.html) pozwala używać operatorów znanych z innych języków programowania,
takich jak **[Scala]** czy **[OCaml]**.
Definiuje on dwa operatory aplikacji oraz dwa operatory kompozycji,
które w uproszczeniu można wyjaśnić jako:
```haskell
(<|) = ($)      -- "apply forward"    or "pipe into"
(|>) = (&)      -- "apply backward"   or "pipe from"
(<.) = (.)      -- "compose backward" or "but first"
(.>) = flip (.) -- "compose forward"  or "and then"
```

Dzięki tym operatorom możemy zdefiniować złożenie funkcji na cztery różne czytelne sposoby:
```haskell
f x = f3 (f2 (f1 x))      -- normalny zapis
f x = f3 <| f2 <| f1 <| x -- apply forward
f x = x |> f1 |> f2 |> f3 -- apply backward
f = f3 <. f2 <. f1        -- compose backward
f = f1 .> f2 .> f3        -- compose forward
```

## Podsumowanie

**[Haskell]** posiada wiele operatorów pozwalających na redukcję nawiasów podczas wywoływania funkcji.
Samodzielnie (lub w zespole) należy ustalić,
który ze styli jest najbardziej czytelny dla nas.

[Haskell]:              /langs/haskell
[Haskella]:             /langs/haskell
[Haskellu]:             /langs/haskell
[OCaml]:                /langs/ocaml
[Scala]:                /langs/scala
[Scali]:                /langs/scala
[Rust]:                 /langs/rust

[scalaz]:               /libs/scalaz
[zio]:                  /libs/zio

[operator]:             /tags/operator
