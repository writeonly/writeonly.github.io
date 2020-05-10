---
title:    'Sześć rodzajów polimorfizmu - polimorfizm jakiego nie znamy z OOP'
author:   TheKamilAdam
category: programming
tags:     ad-hoc compiler fp interface interpreter monad oop protocol trait type-class
langs:    clojure common-lisp haskell java kotlin ocaml perl racket rust scala scheme smalltalk
libs:     arrow cats scalaz
tools:    bash
redirect_from:
  - polymorphism-that-we-do-not-know
  - programming/polymorphism-that-we-do-not-know
---

Wydawałoby się,
że trzy rodzaje polimorfizmu,
jakie są spotykane w obiektowych językach programowania
i które opiswałem w artykule [Sześć rodzajów polimorfizmu - polimorfizm jaki znamy z OOP](/polymorphism-as-we-know-it),
są wszystkim czego potrzebujemy do szczęścia,
żeby pisać zwięzły kod.
Czasem pojawi się tylko jakiś zgrzyt,
gdzie trzeba zaimplementować wzorzec wizytator lub fakrykę.

## [Wielometody] (ang. *multimethods*)

Żeby wyjaśnić co to jest wielomatoda (Wielometoda),
muszę najpierw zdefiniować,
co to jest metoda.
Poprzednie dwa rodzaje polimorfizmu można uznać za tradycyjny obiektowy polimorfizm z jednokrotną i dynamiczną wysyłką (ang. *[single and dynamic dispatch]*).
Używają one metod do zapewnienia polimorfizmu w czasie uruchomienia (ang. *[runtime]*).
W większości języków programowania metody zapisywane są jako:
```
obiekt.metoda(arg1, arg2, ..., argN)
```

Metodę można sobie jednak wyobrazić jako funkcję polimorficzną ze względu na swój pierwszy parametr:
```
metoda(obiekt, arg1, arg2, ..., argN)
```
Powyższy zapis jest możliwy w niektórych starszych językach obiektowych np. w **[Perlu]** podczas wywoływania metod oraz w **[Pythonie]** podczas deklarowania metod.

Wielomatoda jest to funkcja,
która może być polimorficzna ze względu na swoje wszystkie argumenty.
Polimorfizm rozwiązywany ze względy na kilka obiektów jest nazywany wielokrotną wysyłką (ang. *[multiple dispatch]*).
```
wielomatoda(arg0, arg1, arg2, ..., argN)
```

Wielometody są zwykle używane w językach programowania z rodziny **[Lisp]** jak **[Common Lisp]**, **[Scheme]**, **[Racket]** i **[Clojure]**.
Dlatego tam zapis wygląda następująco:
```
(wielomatoda arg0, arg1, arg2, ..., argN)
```

Wielometod są najpotężniejszym ze wszystkich typów polimorfizmu.
I w oparciu o wielometody można zbudować tradycyjny obiektowy polimorfizm.

Wadą wielometod ich jest to,
że są najpowolniejsze ze wszystkich rodzajów polimorfizmu.
Ponieważ trzeba sprawdzić większą ilość warunków,
żeby wywołać konkretną implementację.

Co ciekawe *[Common Lisp Object System]*,
czyli wielometody z **[Common Lisp]** są uważane za przykład funkcji uogólnionych (ang. *[generic function]*).
Za to kursy **[Clojure]** są zgodne,
że wielometody są odmianą polimorfizmu *ad hoc*.

## [Polimorfizm ad hoc] (ang. *[ad hoc polymorphism]*)

> Ad hoc polymorphism: defines a common interface for an arbitrary set of individually specified types.

Za [wikipedią].

Czyli w wolnym tłumaczeniu:
> Polimorfizm ad hoc: definiuje wspólny interfejs dla dowolnego zestawu indywidualnie określonych typów.

Wymyślony w roku 1968 dla języka programowania **Algol 68**.
Jest to niejako odwrotność polimorfizmu parametrycznego.
Polimorfizm parametryczny służy do opisania ogólnego algorytmu,
za to polimorfizm *[ad hoc]* służy do opisania szczególnych przypadków.

Polimorfizm *[ad hoc]* można podzielić na *dwa* rodzaje ze względu na czas wiązania:
* Wczesne wiązanie (ang. *[Early binding]*) - rozwiązywane w czasie kompilacji.
* Późne wiązanie (ang. *[Late binding]*) - rozwiązywane w czasie wywoływania.
* Nie wiadomo jakie wiązanie.
* Wiadomo jakie wiązanie.

### Wczesne wiązanie i statyczny polimorfizm *ad hoc*
W językach statycznie typowanych jest rozwiązywany podczas kompilacji i dzieli się na:
* [przeciążanie funkcji] (ang. *[function overloading]*)
* [przeciążanie operatorów] (ang. *[operator overloading]*)

Jest to w zasadzie lukier składniowy dzięki,
któremu można deklarować takie same nazwy funkcji i metod dla różnych typów danych.

Dzięki temu metoda statyczna `max` w Javie jest identyczna dla wszystkich typów prymitywnych:
```java
static dataType max(dataType a, dataType b)
```

Chociaż pod spodem są to cztery różne metody statyczne:
```java
static double max(double a, double b);
static float  max(float  a, float  b);
static int    max(int    a, int    b);
static long   max(long   a, long   b);
```

W języku,
który nie posiada polimorfizmu *[ad hoc]*,
trzeba utworzyć każdą funkcję z inną nazwą.
```java
double      fmax (double      a, double      b);
float       fmaxf(float       a, float       b);
long double fmaxl(long double a, long double b);
```

Za statyczny polimorfizm *[ad hoc]* są uważane także:
* `extensions` w **[Kotlinie]**
* specyfikacja szablonów w C++

### Późne wiązanie i dynamiczny polimorfizm *[ad hoc]*

Niektóre języki programowania jak np. **[SmallTalk]** posiadają podwójną wysyłkę (ang. *[double dispatch]*).
Metody są tam polimorficzne nie tylko ze względu na obiekt,
dla którego wywoływana jest funkcja,
ale także ze względu na argument.
W tradycyjnych językach obiektowych można to zasymulować za pomocą wzorca [odwiedzający], (ang. *[visitor pattern]*).

Za dynamiczny polimorfizm *[ad hoc]* są uważane także:
* `implicit conversion` w **[Scali]**
* Wielometody w języki **[Clojure]** (ale wielometody w **[Common Lisp]** już nie)

A teraz zagadka.
Jeśli podwójna wysyłka i wielokrotna wysyłka są podrodzajami polimorfizmu *[ad hoc]*
to czemu pojedyncza wysyłka (subtyping i polimorfizm strukturalny) nie jest podrodzajem polimorfizmu *[ad hoc]*?
Może istnieją tylko dwa rodzaje polimorfizmu?
Polimorfizm parametryczny i polimorfizm *[ad hoc]*?

### Nie wiadomo jakie wiązanie

Występuje dla przypadku [klasy typów] w **[Haskellu]**.
W większości przypadków [klasy typów] są rozwiązywane podczas kompilacji.
Jednak czasem jest to niemożliwe dla kompilatora.
W takim przypadku kompilator przesuwa czas wiązania na czas wykonania.
Co pozwala osiągnąć polimorfizm podobny do subtypingu i sprawia,
że programowanie obiektowe jest możliwe w **[Haskellu]**.

[Klasy typów] są możliwe do uzyskania także w innych językach programowania jak np.:
* w **[Scali]** przy używaniu bibliotek **[Scalaz]**/**[Cats]**
* w **[Kotlina]** przy używaniu biblioteki **[ARROW]**

### Wiadomo jakie wiązanie

Cechy (ang. *[trait]*) w języku **[Rust]** są uproszczonymi wersjami [klas typów].
Wiązania w nich są rozwiązywane w czasie kompilacji.
Jednak dzięki wskaźnikom można przesunąć czas rozwiązywania wiązań na czas wykonania.
Co pozwala osiągnąć polimorfizm podobny do subtypingu i sprawia,
że programowanie obiektowe jest możliwe w **[Ruscie]**.

## Inne rodzaje polimorfizmu

* *Overriding with single dispatch* - nie widzę różnicy między polimorfizmem strukturalnym.
* *Wielotypowanie* (ang. *[polytypism]*) jest to odmiana automatycznie generowanego polimorfizmu *[ad hoc]*.
* Polimorfizm ograniczeniowy.
W klasycznym świecie obiektowym jest to polimorfizm parametryczny + polimorfizm podtypowy.
Ale w **[Haskellu]** ograniczenia dla polimorfizmu parametrycznego robi się za pomocą **[klas typów]** i polimorfizmu *[ad hoc]*.
* Rekordy wariantowe.
Nie uważam,
żeby to był rodzaj polimorfizmu. A jeśli tak to należałoby uznać `Either` także za polimorfizm.

## Podsumowanie

Istnieje wiele rodzai polimorfizmu.
Niektóre rodzaje polimorfizmu jak podtypowanie i polimorfizm strukturalny,
są tradycyjnie łączone z programowaniem obiektowym.
Jednak także wielometody i dynamiczny polimorfizm *[ad hoc]* pozwalają programować obiektowo.
Dlatego warto czasem spojrzeć poza języki obiektowe,
żeby nauczyć się czegoś nowego i poszerzyć swoje horyzonty.

Jaka jest różnica?
Tradycyjne języki obiektowe często gwałcą polimorfizmem.
Tzn. wpychają go nam wtedy,
gdy go nie potrzebujemy.
Np. w **[Javie]** nie ma innego sposobu na napisanie kodu niż umieszczenie go wewnątrz klasy i metody.
Dodatkowo metoda ta jest domyślnie wirtualna.
Odwrotnie jest w językach funkcyjnych,
gdzie dynamiczny polimorfizm jest ostatecznością.
W moim idealnym statycznie typowanym języku programowania kod powinien być domyślnie monomorficzny a struktury danych niezmienne.
Definicję tę jak na razie spełniają języki **[Haskell]**, **[OCaml]** i **[Rust]**.

Trochę lepiej niż w **[Javie]** jest w **[Scali]**,
gdzie nie trzeba tworzyć żadnej klasy,
żeby napisać program,
ale trzeba utworzyć obiekt singletonowy.
Jeszcze lepiej jest w **[Kotlinie]**,
gdzie nie trzeba tworzyć w ogóle obiektów.

[Clojure]:       /langs/clojure
[Common Lisp]:   /langs/common-lisp
[Erlang]:        /langs/erlang
[Go]:            /langs/go
[Haskell]:       /langs/haskell
[Haskellu]:      /langs/haskell
[Java]:          /langs/java
[JavaScript]:    /langs/javascript
[Javie]:         /langs/java
[Javy]:          /langs/java
[Kotlin]:        /langs/kotlin
[Kotlina]:       /langs/kotlin
[Kotlinie]:      /langs/kotlin
[LISP]:          /langs/lisp
[Meta Language]: /langs/meta-language
[OCaml]:         /langs/ocaml
[Perl]:          /langs/perl
[Perlu]:         /langs/perl
[Pony]:          /langs/pony
[Python]:        /langs/python
[Pythonie]:      /langs/python
[Racket]:        /langs/racket
[Ruby]:          /langs/ruby
[Ruscie]:        /langs/rust
[Rust]:          /langs/rust
[SmallTalk]:     /langs/smalltalk
[Scala]:         /langs/scala
[Scali]:         /langs/scala
[Scheme]:        /langs/scheme
[TypeScript]:    /langs/typescript
[Vala]:          /langs/vala
[Vali]:          /langs/vala

[Bash]:          /tools/bash
[Basha]:         /tools/bash

[ARROW]:         /libs/arrow
[Cats]:          /libs/cats
[Scalaz]:        /libs/scalaz

[Ad hoc]:        /tags/ad-hoc
[Dsl]:           /tags/dsl
[Kompilator]:    /tags/compiler
[klas typów]:    /tags/type-class
[Klasy typów]:   /tags/type-class

[Ad hoc polymorphism]:         https://en.wikipedia.org/wiki/Ad_hoc_polymorphism
[Anemic domain model]:         https://en.wikipedia.org/wiki/Anemic_domain_model
[Common Lisp Object System]:   https://en.wikipedia.org/wiki/Common_Lisp_Object_System
[Duck typing]:                 https://en.wikipedia.org/wiki/Duck_typing
[Double dispatch]:             https://en.wikipedia.org/wiki/Double_dispatch
[Early binding]:               https://en.wikipedia.org/wiki/Ad_hoc_polymorphism#Early_binding
[Function overloading]:        https://en.wikipedia.org/wiki/Function_overloading
[Generic function]:            https://en.wikipedia.org/wiki/Generic_function
[Generic programming]:         https://en.wikipedia.org/wiki/Generic_programming
[Higher-kinded polymorphism]:  https://en.wikipedia.org/wiki/Type_class#Higher-kinded_polymorphism
[Interface]:                   https://en.wikipedia.org/wiki/Interface_(computing)
[Late binding]:                https://en.wikipedia.org/wiki/Ad_hoc_polymorphism#Late_binding
[Memento pattern]:             https://en.wikipedia.org/wiki/Memento_pattern
[Multiple dispatch]:           https://en.wikipedia.org/wiki/Multiple_dispatch
[Mixin]:                       https://en.wikipedia.org/wiki/Mixin
[Nominal type system]:         https://en.wikipedia.org/wiki/Nominal_type_system
[Operator overloading]:        https://en.wikipedia.org/wiki/Operator_overloading
[Parametric polymorphism]:     https://en.wikipedia.org/wiki/Parametric_polymorphism
[Polytypism]:                  https://en.wikipedia.org/wiki/Generic_programming#Functional_languages
[Protocol]:                    https://en.wikipedia.org/wiki/Protocol_(object-oriented_programming)
[Row polymorphism]:            https://en.wikipedia.org/wiki/Row_polymorphism
[Runtime]:                     https://en.wikipedia.org/wiki/Runtime_(program_lifecycle_phase)
[Single and dynamic dispatch]: https://en.wikipedia.org/wiki/Dynamic_dispatch
[Structural subtyping]:        https://en.wikipedia.org/wiki/Structural_type_system
[Structural type system]:      https://en.wikipedia.org/wiki/Structural_type_system
[Subtyping]:                   https://en.wikipedia.org/wiki/Subtyping
[Trait]:                       https://en.wikipedia.org/wiki/Trait_(computer_programming)
[Visitor pattern]:             https://en.wikipedia.org/wiki/Visitor_pattern
[wikipedią]:                   https://en.wikipedia.org/wiki/Polymorphism_(computer_science)

[Abstrakcja]:                  https://pl.wikipedia.org/wiki/Abstrakcja_(programowanie)
[Cecha]:                       https://pl.wikipedia.org/wiki/Cecha_(programowanie_obiektowe)
[Domieszka]:                   https://pl.wikipedia.org/wiki/Domieszka_(programowanie_obiektowe)
[Interfejs]:                   https://pl.wikipedia.org/wiki/Interfejs_(programowanie_obiektowe)
[Kompozycja]:                  https://pl.wikipedia.org/wiki/Agregacja_(programowanie_obiektowe)#Kompozycja

[Odwiedzający]:                https://pl.wikipedia.org/wiki/Odwiedzaj%C4%85cy
[Pamiątka]:                    https://pl.wikipedia.org/wiki/Pami%C4%85tka_(wzorzec_projektowy)
[Polimorfizm ad hoc]:          https://pl.wikipedia.org/wiki/Polimorfizm_(informatyka)#Polimorfizm_ad-hoc
[Programowanie uogólnione]:    https://pl.wikipedia.org/wiki/Programowanie_uogólnione
[Przeciążanie funkcji]:        https://pl.wikipedia.org/wiki/Przeci%C4%85%C5%BCanie_funkcji
[Przeciążanie operatorów]:     https://pl.wikipedia.org/wiki/Przeci%C4%85%C5%BCanie_operator%C3%B3w

[Wielometody]:                 https://randomseed.pl/pub/poczytaj-mi-clojure/21-polimorfizm/#Wielometody

[Polytypism]:                  http://zenzike.com/posts/2010-12-10-from-polymorphic-to-polytypic
