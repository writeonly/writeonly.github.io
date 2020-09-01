---
title:    'Styl wyrażenia a styl deklaracji'
author:   TheKamilAdam
category: haskell-eta
langs:    haskell ocaml perl python racket scheme
redirect_from:
  - declaration-vs-expression-style
  - haskell-eta/declaration-vs-expression-style
---

Wiele styli - jak perl
Jeden styl - jak python
Dwa style

Istnieją dwa główne style pisania programów funkcjonalnych, które są obsługiwane przez Haskella głównie dlatego, że kilku projektantów języków preferowało te różne style.

W stylu deklaracji formułujesz algorytm za pomocą kilku równań, które powinny być spełnione.
W stylu wyrażeń tworzysz duże wyrażenia z małych wyrażeń.

## Styl wyrażeń

Zacznę od stylu wyrażeń,
bo jest to coś co częściej jest spotykane w językach funkcyjnych.

Scheme Racket

Styl wyrażeń 

### Abstrakcja lambda i wiązanie

Trudno powiedzieć czym jest programowanie funkcyjne.
Wielu pisze że jest programowanie oparte na zwracaniu wartości.
Trudno jednak wyokrazić sobie programowanie funkcyjne bez możliwości deklarowania lambd.

(Functions (Procedures): lambda)[https://docs.racket-lang.org/guide/lambda.html]

```
(lambda (arg-id ...)
  body ...+)
```

Przykładowe utworzenie lambdy to
```racket
(lambda (a b) (+ a b))
```

Z taką utworznoną lambdą niewiele można zrobić.
W zasadzie można ją tylko wywołać (zaaplikować)
```racket
((lambda (a b) (+ a b)) 2 3)
```

(Definitions: define)[https://docs.racket-lang.org/guide/define.html]
```
(define id expr)
```

Czyli w maszym przypadku
```racket
(define add (lambda (a b) (+ a b)))
```


Konstrukcja 
```racket
(define id (lambda (arg ...) body ...+))
```
jest na tyle popularna, że istnieje dla niej skrót
```
(define (id arg ...) body ...+)
```

Czyli maszą lambdę można przypisać do zmiennej za pomocą kodu:
```racket
(define (add a b) (+ a b))
```

https://www2.lib.uchicago.edu/keith/ocaml-class/data.html#function
```ocaml
let inc = fun n -> n + 1;;
```

W Haskellu w trybie interaktywnym 
```haskell
let f :: Num -> Num -> Num
let f = \a b -> a+b
```

Poza intrybem interaktywnym
```haskell
f :: Num -> Num -> Num
f = \a b -> a+b
```

[Lambda abstraction](https://wiki.haskell.org/Lambda_abstraction) 
[Anonymous function](https://wiki.haskell.org/Anonymous_function)

### wyrażenie `let in` czyli wiązanie lokalny

[Local Binding](https://docs.racket-lang.org/guide/let.html)

[Local Binding: let, let*, letrec, ...](https://docs.racket-lang.org/reference/let.html#%28form._%28%28lib._racket%2Fprivate%2Fletstx-scheme..rkt%29._let%29%29)

```racket
(let ([id val-expr] ...) body ...+)
```

```racket
((lambda (id ...) body ...+) val-expr ...)
```

```racket
((lambda (a b) (+ a b)) 2 3)
```

(let )

https://www2.lib.uchicago.edu/keith/ocaml-class/definitions.html

#### Ocaml

```ocaml
let n = 2 in n * n;;
```

#### Haskell

W Haskellu jest wyrażenie `let-in`

```
aaa x y = let r = 3 
              s = 6
              in  r*x + s*y
```

### wyrażenie if

[Conditionals](https://docs.racket-lang.org/guide/conditionals.html)

[Conditionals: if, cond, and, and or](https://docs.racket-lang.org/reference/if.html)

[https://wiki.haskell.org/If-then-else](If-then-else)

### wyrażenie case

[Simple Dispatch: case](https://docs.racket-lang.org/guide/case.html)

[Pattern Matching](https://docs.racket-lang.org/guide/match.html)

[Case](https://wiki.haskell.org/Case)

[Type declarations and pattern matching](https://caml.inria.fr/pub/docs/oreilly-book/html/book-ora016.html)

```ocaml
let imply v = match v with 
     (true,true)   -> true
   | (true,false)  -> false
   | (false,true)  -> true
   | (false,false) -> true;;
val imply : bool * bool -> bool = <fun>
```

[Data Types and Matching](https://ocaml.org/learn/tutorials/data_types_and_matching.html)

```ocaml
# let rec to_string e =
    match e with
    | Plus (left, right) ->
       "(" ^ to_string left ^ " + " ^ to_string right ^ ")"
    | Minus (left, right) ->
       "(" ^ to_string left ^ " - " ^ to_string right ^ ")"
    | Times (left, right) ->
       "(" ^ to_string left ^ " * " ^ to_string right ^ ")"
    | Divide (left, right) ->
       "(" ^ to_string left ^ " / " ^ to_string right ^ ")"
    | Value v -> v;;
val to_string : expr -> string = <fun>
# let print_expr e =
    print_endline (to_string e);;
val print_expr : expr -> unit = <fun>
```

[Pattern Matching](https://www2.lib.uchicago.edu/keith/ocaml-class/pattern-matching.html)
```ocaml
    # let f x = 
	if x = "foo"
	then "it is foo"
	else if x = "bar"
	then "bar is the one"
	else if x = "zap"
	then "totally different"
	else "our default: " ^ x
      ;;
    val f : string -> string = <fun>
    # f "hello";;
    - : string = "our default: hello"
    # f "bar";;
    - : string = "bar is the one"
```

https://ocaml.org/learn/tutorials/data_types_and_matching.html#Pattern-matching-on-datatypes

## Styl deklaracja

### Deklaracja funkcji i wiązanie

W Scheme ani Racket nie ma innej możliwości deklaracji funkcji niż za pomocą lambdy.
Dlatego przejrziemy odrazu do języka Haskell

W Haskellu deklaracja funkcji wygląda następująco
```haskell
f :: Num -> Num -> Num
f x = x*x
```

Co warto zauważyć?
Otóż typ funkcji oraz typ lambdy przypisanej do zmiennej są identyczne.
Dlatego w każdym miejscu,
gdzie jest wymagana lambda,
możemy też użyć funkcji.

Jeśli jednak postanowimy napisać lambdę jak w poniższym przykładzie:
```haskell
map (\x -> abs x) [1, -1, 2 -2]
```

Kompilator haskella skorzysta z [Eta conversion](https://wiki.haskell.org/Eta_conversion)
i zamieni to na postać:
```haskell
map abs [1, -1, 2 -2]
```


### Klauzura `where` czyli wiązanie lokalny

[Let vs. Where](https://wiki.haskell.org/Let_vs._Where)

### Strażnicy (ang. `Guards`)

### Dopasowanie do wzorców (ang `Pattern matching`)


## Podsumowanie
Który ze styli jest lepszy?
Moim skromnym zdaniem 

[Declaration vs. expression style](https://wiki.haskell.org/Declaration_vs._expression_style)

