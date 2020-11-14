---
title:    'Sześć rodzajów polimorfizmu - polimorfizm jaki znamy z OOP'
author:   TheKamilAdam
category: programming
tags:     dsl compiler fp interface interpreter monad protocol trait type-class
langs:    clojure common-lisp go haskell java javascript kotlin meta-language ocaml perl pony racket ruby rust scala scheme smalltalk typescript vala
libs:     arrow cats scalaz
tools:    bash
redirect_from:
  - polymorphism-as-we-know-it
  - programming/polymorphism-as-we-know-it
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
No ale nie do końca jest to prawda.

Usłyszałem kiedyś od Jarka Pałki,
że istnieje 6 rodzajów polimorfizmu.
Niestety nie wymienił wszystkich rodzai.
Od tego czasu postanowiłem zebrać je wszystkie razem.
I są to:
* Polimorfizm podtypowy
* Polimorfizm strukturalny
* Polimorfizm parametryczny
* Wielometody (Multimetody)
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

Podtypowaniem jest powiązane z *nominalnym system typów* (ang. *[nominal type system]* lub *name-based type system*)
zwanym też *równoważnością przez nazwę*.
W nominalnym systemie typów jeden typ można zastosować w miejsce drugiego,
jeśli implementują ten sam protokół (ang. *[protocol]*), interfejs (ang. *[interface]*) lub cechę (ang. [trait]).

Często jest nazywany także dziedziczeniem.
Jednak określenie dziedziczenie może być nieprecyzyjne,
ponieważ może odnosić się zarówno do dziedziczenia metod,
jak i dziedziczenia pól.

Podtypowanie jest to główny rodzaj polimorfizmu używany w statycznych i obiektowych językach programowania takich jak **[Vala]**, **[Java]**, **[Kotlin]**, **[Scala]**, **[OCaml]** czy **[Pony]**.

Główną zaletą podtypowania jest to,
że jest prawdopodobnie najszybszym rodzajem polimorfizmu dynamicznego.
Główną wadą jest to że jest to bardzo ograniczony rodzaj polimorfizmu.

## Polimorfizm strukturalny (ang. *[row polymorphism]*)

Polimorfizm strukturalny nazywany także polimorfizmem opartym na sygnaturach (ang. *signature-based polymorphism*) można podzielić na dwa podrodzaje:
* Polimorfizm strukturalny w językach statycznie typowanych.
* Polimorfizm strukturalny w językach dynamicznie typowanych.

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

## Polimorfizm parametryczny (ang. *[parametric polymorphism]*)

> Parametric polymorphism: when one or more types are not specified by name but by abstract symbols that can represent any type.

Za [wikipedią].

Czyli w wolnym tłumaczeniu:
> Polimorfizm parametryczny: gdy jeden lub więcej typów nie jest określonych przez nazwę, ale przez abstrakcyjne symbole, które mogą reprezentować dowolny typ.

Polimorfizm parametryczny został wymyślony w 1975 roku dla języka programowania **[Meta Language]**,
który według legendy miał być statycznie typowaną wersją języka **[LISP]**.
A więc jest to polimorfizm pierwotnie związany z funkcyjnymi językami programowania, a nie z obiektowymi.
Polimorfizm parametryczny pozwolił na [programowanie uogólnione] (ang. *[generic programming]*) w statycznie typowanych językach programowania.

Polimorfizm parametryczny można podzielić na dwa rodzaje:
* Polimorfizm parametryczny oparty na szablonach w C++ i D,
gdzie kod jest generowany podczas kompilacji dla każdego typu osobno.
* Polimorfizm parametryczny oparty na typach (funkcja, metodach, klasach itd.) uogólnionych (ang. generic types),
gdzie dla wszystkich typów jest używany ten sam kod.

## Podsumowanie

W tym artykule krótko opisałem trzy rodzaje polimorfizmu kojarzone standardowo z obiektowymi językami programowania.
Pozostałe trzy są opisane w kolejnym artykule [Sześć rodzajów polimorfizmu - polimorfizm jakiego nie znamy z OOP](/polymorphism-that-we-do-not-know)

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
