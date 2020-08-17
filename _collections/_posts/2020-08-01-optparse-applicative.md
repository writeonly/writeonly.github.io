---
title:    'Parsowanie parametrów wiersza poleceń w Haskellu'
author:   TheKamilAdam
category: haskell-eta
tags:     assembler cli interpreter
langs:    eta haskell
libs:     optparse-applicative
tools:    cabal etlas
projects: helcam helpa helvm
eso:      brainfuck eta subleq whitespace
redirect_from:
  - optparse-applicative
  - haskell-eta/optparse-applicative
---

Wiele styli - jak perl
Jeden styl - jak python
Dwa style

Istnieją dwa główne style pisania programów funkcjonalnych, które są obsługiwane przez Haskella głównie dlatego, że kilku projektantów języków preferowało te różne style.

W stylu deklaracji formułujesz algorytm za pomocą kilku równań, które powinny być spełnione.
W stylu wyrażeń tworzysz duże wyrażenia z małych wyrażeń.

## Styl wyrażeń



### Abstrakcja lambda i wiązanie globalne

```racket
(lambda (a b) (+ a b))
```

```racket
((lambda (a b) (+ a b)) 2 3)
```

```racket
(define add (lambda (a b) (+ a b)))
```


```racket
(define (add a b) (+ a b))
```

(define (id arg ...) body ...+)
which is a shorthand for

(define id (lambda (arg ...) body ...+))

```haskell
let f :: Num -> Num -> Num
let f = \a b -> a+b
```

[Lambda abstraction](https://wiki.haskell.org/Lambda_abstraction) 
[Anonymous function](https://wiki.haskell.org/Anonymous_function)

### wyrażenie `let in` czyli wiązanie lokalny



let, let*, letrec, ...


### wyrażenie if

[https://wiki.haskell.org/If-then-else](If-then-else)

### wyrażenie case

[Case](https://wiki.haskell.org/Case)

## Styl deklaracja

### Deklaracja funkcji i wiązanie globalny

```haskell
f :: Num -> Num -> Num
f x = x*x
```

Co warto zauważyć?
Otóż typ funkcji oraz zmiennej zawiwrającą

[Eta conversion](https://wiki.haskell.org/Eta_conversion)


### Klauzura `where` czyli wiązanie lokalny

[Let vs. Where](https://wiki.haskell.org/Let_vs._Where)

### Strażnicy (ang. `Guards`)

### Dopasowanie do wzorców (ang `Pattern matching`)


## Inne

[Declaration vs. expression style](https://wiki.haskell.org/Declaration_vs._expression_style)

