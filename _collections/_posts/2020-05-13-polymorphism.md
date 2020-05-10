---
title:    'Sześć rodzajów polimorfizmu'
author:   TheKamilAdam
category: programming
tags:     dsl compiler fp interface interpreter monad protocol trait type-class
langs:    clojure common-lisp go haskell java javascript kotlin meta-language ocaml perl pony racket ruby rust scala scheme smalltalk typescript vala
libs:     arrow cats scalaz
tools:    bash
redirect_from:
  - polymorphism
  - programming/polymorphism
---

Gdy dociera do mnie informacja o kolejnym wspaniałym języku programowania,
którego warto się nauczyć,
to pierwsze co robię to sprawdzam jak wygląda polimorfizm w tym języku programowania.

* Jeśli język nie wspiera polimorfizmu to stwierdzam,
że nie warto się go uczyć.
Znam już C i nie mam zamiaru pisać małych programów, w których polimorfizm nie jest potrzebny.
Znam też [Basha] do pisania prostych skryptów.
Więc o ile nie jest to jakiś **[DSL]** to prawdopodobnie nie można się nauczyć nic ciekawego przy jego nauce.
* Jeśli język wspiera polimorfizm z programowania obiektowego,
czyli podtypowanie to nie widze sensu,
żeby się go uczyć.
Znam programowanie obiektowe z **[Javy]**, **[Kotlina]** i **[Scali]** czy **[Vali]**.
Obiektowe języki programowania są bardzo podobne i różnią się tylko zwięzłością składni,
szybkością działania i ilością dostępnych bibliotek.
Nie ma w nich żadnych nowych idei.
To ciągle te same koncepcje i te same problemy pod nową postacią.
* Jeśli język wspiera inny rodzaj polimorfizmu to znaczy, że jest ciekawy i przy jego nauce można się czegoś nowego nauczyć.

Ale zaraz, zaraz.
Przecież polimorfizm to część programowania obiektowego i poza programowaniem obiektowym polimorfizm jest niemożliwy.
Z takim przynajmniej przeświadczeniem można w Polsce wyjść po niektórych uczelniach.
No nie do końca jest to prawda.

Usłyszałem kiedyś od Jarka Pałki,
że istnieje 6 rodzajów polimorfizmu.
Niestety nie wymienił wszystkich rodzai.
Od tego czasu postanowiłem zebrać je wszystkie razem.
I są to:
* Polimorfizm podtypowy
* Polimorfizm strukturalny
* Wielometody (Multimetody)
* Polimorfizm parametryczny
* Polimorfizm *ad hoc*
* Inne rodzaje polimorfizmu :)

Jednak nie mam pojęcia czy są to te rodzaje polimorfizmu,
które Jarek Pałka miał na myśli.

Polimorfizm można podzielić też na dwa rodzaje ze względu na czas wykonania:
* Polimorfizm statyczny to polimorfizm czasu kompilacji rozwiązywany przez [kompilator].
* Polimorfizm dynamiczny to polimorfizm czasu wykonania.

Polimorfizm statyczny jest bezpieczniejszy i nie wprowadza opóźnień podczas działania aplikacji.
Za to polimorfizm czasu wykonania jest potężniejszy.

## Podtypowanie (ang. *[subtyping]*)

> Subtyping (also called subtype polymorphism or inclusion polymorphism): when a name denotes instances of many different classes related by some common superclass.

Za [wikipedią].

Czyli w wolnym tłumaczeniu:
> Podtypowanie (zwane również polimorfizmem podtypu lub polimorfizmem inkluzyjnym): gdy nazwa oznacza wystąpienie wielu różnych klas powiązanych przez jakąś wspólną nadklasę.

Podtypowanie jest powiązane *nominalny system typów* (ang. *[nominal type system]* lub *name-based type system*)
zwanym też *równoważnością przez nazwę*.
W nominalnym systemie typów jeden typ można zastosować w miejsce drugiego,
jeśli implementują ten sam protokół (ang. *[protocol]*), interfejs (ang. *[interface]*) lub cechę (ang. [trait]).

Często jest nazywany także dziedziczeniem.
Jednak określenie dziedziczenie może być nieprecyzyjne,
ponieważ może odnosić się zarówno do dziedziczenia method,
jak i dziedziczenia pól.

Podtypowanie jest to główny rodzaj polimorfizmu używany w statycznych i obiektowych językach programowania takich jak **[Vala]**, **[Java]**, **[Kotlin]**, **[Scala]**, **[OCaml]** czy **[Pony]**.

Główną zaletą podtypowania jest to,
że jest prawdopodobnie najszybszym rodzajem polimorfizmu dynamicznego.
Główną wadą jest to że jest to bardzo ograniczony rodzaj polimorfizmu.

Polimorfizm ograniczeniowy

## Polimorfizm strukturalny (ang. *[row polymorphism]*)

Polimorfizm strukturalny nazywany także polimorfizmem opartym na sygnaturach (ang. *signature-based polymorphism*) można podzielić na dwa podrodzaje:
* Polimorfizm strukturalny w językach dynamicznie typowanych.
* Polimorfizm strukturalny w językach statycznie typowanych.

Polimorfizm strukturalny w językach statycznie typowanych jest powiązany z *strukturalnym system typów* (ang. *[structural type system]* lub *property-based type system*)
zwanym też *równoważnością strukturalną*.
W systemie typów strukturalnych dwa typy są równe,
jeśli posiadają taki sam zestaw metod.
Czyli żeby użyć jednego typu w miejsce drugiego wystarczy,
że posiada on taki sam zestaw metod.
Polimorfizm strukturalny jest używany jako polimorfizm pomocniczy w niektórych statycznych i obiektowych językach programowania jak **[Scala]**, **[OCaml]** lub **[Pony]**.
Jest także używany jako główny rodzaj polimorfizmu w statycznych językach programowania **[Go]** i **[TypeScript]**.

Polimorfizm strukturalny w językach dynamicznie typowanych jest powiązany z *kaczym typowaniem* (ang. *[duck typing]*).
W takich językach programowania nie ma sprawdzania,
czy typy są równe.
Po prostu są wywoływane metody.
Polimorfizm strukturalny z kaczym typowanie jest głównym sposobem polimorfizmu w dynamicznych i obiektowych językach programowania takich jak **[Perl]**, **[Ruby]**, **[Python]**, **[JavaScript]**.

Ponieważ polimorfizm strukturalny działa często przez refleksję i wybiera metody po nazwie,
jest wolniejsze niż podtypowanie.
W polimorfizmie strukturalnym także używa się dziedziczenia,
ale bardziej do dziedziczenia pól lub jako wskazówkę dla programisty niż dlatego,
że jest to wymagane przez kompilator lub interpreter.

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
że są najpowolniejsze ze wszystkich rodzajów polimorfizmy.
Ponieważ trzeba sprawdzić większą ilość warunków,
żeby wywołać konkretną implementację.

## Polimorfizm parametryczny (ang. *[parametric polymorphism]*)

> Parametric polymorphism: when one or more types are not specified by name but by abstract symbols that can represent any type.

Za [wikipedią].

Czyli w wolnym tłumaczeniu:
> Polimorfizm parametryczny: gdy jeden lub więcej typów nie jest określonych przez nazwę, ale przez abstrakcyjne symbole, które mogą reprezentować dowolny typ.

Polimorfizm parametryczny został wymyślony w 1975 roku dla języka programowania **[Meta Language]**,
który według legendy miał być statycznie typowaną wersją języka **[LISP]**.
Polimorfizm parametryczny pozwolił na [programowanie uogólnione] (ang. *[generic programming]*) w statycznie typowanych językach programowania.

Polimorfizm parametryczny można podzielić na dwa rodzaje:
* Polimorfizm parametryczny oparty na szablonach w C++ i D - kod jest generowany podczas kompilacji dla każdego typu osobno.
* Polimorfizm parametryczny oparty na typach (funkcja, metodach, klasach itd.) uogólnionych (ang. generic types) - dla wszystkich typów jest używany ten sam kod.

Co ciekawe *[Common Lisp Object System]*,
czyli wielometody z **[Common Lisp]** są uważane za przykład funkcji uogólnionych (ang. *[generic function]*).

## [Polimorfizm ad hoc] (ang. *[ad hoc polymorphism]*)

> Ad hoc polymorphism: defines a common interface for an arbitrary set of individually specified types.

Za [wikipedią].

Czyli w wolnym tłumaczeniu:
> Polimorfizm ad hoc: definiuje wspólny interfejs dla dowolnego zestawu indywidualnie określonych typów.

Wymyślony w roku 1968 dla języka programowania **Algol 68**.
Jest to niejako odwrotność polimorfizmu parametrycznego.
Polimorfizm parametryczny służy do opisania ogólnego algorytmu,
za to polimorfizm *ad hoc* służy do opisania szczególnych przypadków.

Polimorfizm *ad hoc* można podzielić na *dwa* rodzaje ze względu na czas wiązania:
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
który nie posiada polimorfizmu *ad hoc*,
trzeba utworzyć każdą funkcję z inną nazwą.
```java
double      fmax (double      a, double      b);
float       fmaxf(float       a, float       b);
long double fmaxl(long double a, long double b);
```

Za statyczny polimorfizm *ad hoc* są uważane także:
* `extensions` w **[Kotlinie]**
* specyfikacja szablonów w C++

### Późne wiązanie i dynamiczny polimorfizm *ad hoc*

Niektóre języki programowania jak np. **[SmallTalk]** posiadają podwójną wysyłkę (ang. *[double dispatch]*).
Metody są tam polimorficzne nie tylko ze względu na obiekt, dla którego wywoływana jest funkcja,
ale także ze względu na argument.
W tradycyjnych językach obiektowych można to zasymulować za pomocą wzorca [odwiedzający], (ang. *[visitor pattern]*).

Za dynamiczny polimorfizm *ad hoc* są uważane także:
* `implicit conversion` w **[Scali]**
* Wielometody w języki **[Clojure]**

A teraz zagadka.
Jeśli podwójna wysyłka i wielokrotna wysyłka są podrodzajami polimorfizmu *ad hoc*
to czemu pojedyncza wysyłka (subtyping i polimorfizm strukturalny) nie jest podrodzajem polimorfizmu *ad hoc*?
Może istnieją tylko dwa rodzaje polimorfizmu?
Polimorfizm parametryczny i polimorfizm *ad hoc*?

### Nie wiadomo jakie wiązanie

Występuje dla przypadku [klasy typów] w **[Haskellu]**.
W większości przypadków [klasy typów] są rozwiązywane podczas kompilacji.
Jednak czasem jest to niemożliwe dla kompilatora.
W takim przypadku kompilator przesuwa czas wiązania na czas wykonania.
Co pozwala osiągnąć polimorfizm podobny do subtypingu i sprawia,
że programowanie obiektowe jest możliwe w **[Haskellu]**.

[Klasy typów] są możliwe do uzyskania także w innych językach programowania jak np.:
* w **[Scali]** przy używaniu bibliotek **[Scalaz]**/**[Cats]**r
* w **[Kotlina]** przy używaniu biblioteki **[ARROW]**

### Wiadomo jakie wiązanie

Cechy (ang. *[trait]*) w języku **[Rust]** są uproszczonymi wersjami [klas typów].
Wiązania w nich są rozwiązywane w czasie kompilacji.
Jednak dzięki wskaźnikom można przesunąć czas rozwiązywania wiązań na czas wykonania.
Co pozwala osiągnąć polimorfizm podobny do subtypingu i sprawia,
że programowanie obiektowe jest możliwe w **[Ruscie]**.

## Inne rodzaje polimorfizmu

* *Overriding with single dispatch* - nie widzę różnicy między polimorfizmem strukturalnym.
* *Wielotypowanie* (ang. *[polytypism]*) jest to odmiana automatycznie generowanego polimorfizmu *ad hoc*.
* Polimorfizm ograniczeniowy.
W klasycznym świecie obiektowym jest to polimorfizm parametryczny + polimorfizm podtypowy.
Ale w **[Haskellu]** ograniczenia dla polimorfizmu parametrycznego robi się za pomocą **[klas typów]** i polimorfizmu *ad hoc*.
* Rekordy wariantowe.
Nie uważam,
żeby to był rodzaj polimorfizmu. A jeśli tak to należałoby uznać `Either` także za polimorfizm.

## Podsumowanie

Istnieje wiele rodzai polimorfizmu.
Niektóre rodzaje polimorfizmu jak podtypowanie i polimorfizm strukturalny,
są tradycyjnie łączone z programowaniem obiektowym.
Jednak także wielometody i dynamiczny polimorfizm *ad hoc* pozwalają programować obiektowo.
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
[SmallTack]:     /langs/smalltalk
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

[Dsl]:           /tags/dsl
[Kompilator]:    /tags/compiler
[klas typów]:    /tags/type-class

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
