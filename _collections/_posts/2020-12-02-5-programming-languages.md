---
title:    '5 języków programowania, które warto znać będąc już programistą'
author:   TheKamilAdam
category: programming
tags:     ad-hoc continuation compiler fp hygienic-macro interpreter io json oop trait type-class
langs:    agda clojure common-lisp coq haskell idris java javascript lisp meta-language ocaml purescript racket reasonml rust scala scheme standard-ml typed-clojure typed-racket
tools:    jvm
redirect_from:
  - 5-programming-languages
  - programming/5-programming-languages
---

Jakiś czas temu na [https://4programmers.net](https://4programmers.net/) pojawiło się stwierdzenie:
> Ostatnio oglądałem wypowiedź twórcy języka C++,
gdzie Bjarne Stroustrup powiedział,
że zna ponad 20 języków programowania w przynajmniej podstawowym stopniu,
dodał, że dobry programista powinien znać bardzo dobrze przynajmniej 5 języków,
aby potrafił spojrzeć na problem z różnych perspektyw.
Jaką piątkę wy byście wybrali, w jakiej kolejności i dlaczego?

Przygotowałem listę 5 języków programowania,
które warto znać,
będąc już programistą.
Jeśli jeszcze nie jesteś programistą,
spójrz na cztery inne artykuły,
które opisują jaki język programowania warto poznać na początek:
1. [dynamicznie typowany język skryptowy ogólnego przeznaczenia](/jezyk-skryptowy)
2. [statycznie typowany język korporacyjny używany do pisania długowiecznych aplikacji klasy *enterprise*](/jezyk-korporacyjny)
3. [fullstack język, który można używać do pisania frontendu i backendu](/jezyk-fullstackowy)
4. [szybki język natywny działający bez maszyny wirtualnej i interpretera](/jezyk-natywny)

Języki programowania do listy wybierałem według zasady,
że większość programistów zna języki [OOP].
Więc lista zawiera tylko języki nie będące [OOP],
ale jednak zawierające polimorfizm.

## **[Haskell]**, język bez mutowalności 

I to wcale nie dlatego że nie ma mutowalności,
czyli zmiennego stanu.
Tylko dlatego, że jest:

* Lazy.
  Haskell jest leniwie ewaluowany.
  Jest to dość niestandardowe zachowanie i dobrze znać co najmniej jeden język,
  który domyślnie jest leniwy,
  żeby zobaczyć jakie powoduje to zalety jak i problemy.
* Fulltyped. 
  W **[Haskellu]** typowanie jest o wiele silniejsze niż Javie czy C++.
  Niektórzy mówią, że to zaleta.
  Inni, że to wada. 
* To język z polimorfizmem bez [OOP].
  Dokładniej jest to polimorfizm [ad hoc].
  Większość topowych języków programowania,
  jeśli ma polimorfizm to jest to polimorfizm związany z [OOP]. 
  Warto wiedzieć,
  że istnieje alternatywa.
* Programowanie reaktywne. 
  Mówi się, że programowanie reaktywne jest trudne.
  Ale w **[Haskellu]** jest naturalne.
  Ponieważ większość konstrukcji reaktywnych jak Mono ze Springa to tak naprawdę uproszczona monada **[IO]**.

## Prosty **[LISP]** a dokładniej **[Scheme]**

**[Haskell]** ma opinię języka skomplikowanego.
Głownie przez monady, które umożliwiają zapis operacji imperatywnych w języku deklaratywnym.
Przez to opinię trudnych mają wszystkie języki funkcyjne.
Dlatego warto poznać **[Scheme]**,
żeby poznać minimalistyczny język funkcyjny.
Ogólnie jest to jeden z najbardziej minimalistycznych języków programowania,
Dlatego bardzo łatwo można napisać swój własny interpreter Scheme.
Jest to myśl przewodnia książki [Struktura i interpretacja programów komputerowych](/books/sicp).
Implementacja własnego interpretera **[Scheme]** jest także myślą przewodnią wielu kursów programowania jak np. [Write Scheme in 48 h](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours).

Jednak to, że rdzeń **[Scheme]** jest mały,
nie znaczy,
że biblioteka jest mała.
Istnieje przenośna biblioteka standardowa **[Scheme]** niezależna od implementacji.

Inną z ciekawych rzeczy istniejącą w **[Scheme]** są [makra higieniczne],
które pozwalają dodawać nowe konstrukcje programistyczne.

Jeśli jednak ktoś chciałby implementację **[Scheme]** z dużą biblioteką standardową to polecam **[Racket]**.

### **[Racket]**, **[Scheme]** na sterydach

Ludzie nie są zgodni czy **[Racket]** jest kolejną implementacją **[Scheme]**,
czy już osobnym językiem programowania z powodu dużej ilości rozszerzeń.

**[Racket]** posiada też dużą ilość bibliotek oraz możliwość pisania frontów do kompilatora **[Racket]**.
Dzięki temu udało się stworzyć osobny język **[Typed Racket]**.

Największą wadą **[Racketa]** jest to,
że jest to język programowania dla naukowców i w ogóle nie da się w nim znaleźć pracy dla normalnego programisty.
Problemu tego nie posiada **[Clojure]**.

### **[Clojure]**, **[Lisp]** na [JVM]

Prawdopodobnie najpopularniejszy dialekt **[Lispa]** w Polsce,
a może nawet w Europie (patrząc po ofertach pracy). 
Jego ogromną zaletą jest to,
że działa na [JVM] i dobrze integruje się z klasami Javowymi.
Przez co jest prawdopodobnie najbardziej klasycznie obiektowym dialektem języka **[Lisp]**.
Inne dialekty **[Lispa]** także posiadają polimorfizm i możliwość programowania obiektowego,
ale zwykle w niestandardowy sposób.

Jeśli chcesz programować zawodowo w **[Lispie]**,
ucz się **[Clojure]**.

## Niskopoziomowy **[Rust]**

Niskopoziomowy język programowania, 
który chce zastąpić C++. 

Jak na niego patrzę to widze uproszczonego **[Haskella]**,
ponieważ rustowe [Traity] są bardzo podobne do haskellowcyh [Klas Typów].
Rust posiada także [makra higieniczne] (podobnie jak **[Scheme]**),
jednak umożliwiające typowanie. 

Dla mnie ten język to fenomen.
Nie stoi za nim żadna wielka korporacja,
a momentalnie wbił się do 20 najpopularniejszych języków programowania według indeksu [Tiobe](https://www.tiobe.com/tiobe-index/).
Jest w nim także praca dla programistów.

## **[Idris]** z typami zależnymi

Poprawiony **[Haskell]** z typami zależnymi.
Jeszcze nie wiem co to typy zależne i czemu to potrzebuję. 
Niby wiem,
że `Typy zależne są potrzebne do dowodzenia poprawności programu`,
ale nie wiem,
co z tego wynika.

W przeciwieństwie do innych języków z typami zależnymi,
jak **[Agda]** czy **[Coq]**,
**[Idris]** jest zaprojektowany jako język ogólnego przeznaczenia,
a nie tylko do dowodzenia twierdzeń.

Jego największą wadą jest to,
że to nisza niszy. 
Jest rozwijany przez jednego człowieka.

Z drugiej strony im popularniejszy jest **[Idris]** tym bardziej prawdopodobne,
że eksperymentalne funkcjonalności tego języka zostaną zaimplementowane także w **[Haskellu]**.

## Inny język programowania

Zostało jedno wolne miejsce,
które jednak nie wiem,
na co przeznaczyć.

### **[PureScript]**, gorliwie ewaluaowany **[Haskell]** dedykowany do pisania frontu.

Odrzuciłem z listy,
ponieważ to tylko minimalnie poprawiony **[Haskell]**.
(Zresztą większość tych poprawek **[Haskell]** 8 także już zawiera)
Oraz dlatego że wszystkie powyższe języki są też kompilowane do weba.
Większość jest kompilowana do **[JavaScriptu]**, **[Rust]** do **[WebAssembly]**.

### **[OCaml]**, pierwszy obiektowo funkcyjny język programowania
**[OCaml]** to obiektowy potomek **[Meta Language]** i **[Standard ML]**.
**[Standard ML]** jest uważany za ostatniego wspólnego przodka **[Haskella]** i **[OCamla]**.
Jakiś czas temu został przeportowany do nowych środowisk jako nowe języki F# i **[ReasonML]**.

Odrzuciłem go z listy,
ponieważ nie uczy niczego nowego w stosunku do wymienionych powyżej języków programowania.
* Ma gorszy system typów niż **[Haskell]**.
* Jest mniej niskopoziomowy niż **[Rust]**.
* Ma trudniejszą składnię niż **[Typed Racket]**.

W dodatku posiada klasyczny polimorfizm obiektowy,
co mnie trochę odrzuca.

### **[Scala]**, czyli skrzyżowanie Javy i **[OCamla]**.

Tak właśnie z **[OCamlem]**.
Przez lata nie było tego widać,
ale teraz gdy **[Scala]** 3 posiada znaczące wcięcia widać to jak na dłoni.
Nie jest to język idealny.
W zasadzie,
jeśli chodzi o czystość składni,
jest nawet gorszy niż **[OCaml]**,
ale ma jedną dużą zaletę.
* Można w nim pisać [Klasy Typów]
* Można w nim zarabiać.

I tak dochodzimy do ostatniego proponowanego przeze mnie języka.

### Język, w którym można zarabiać.

Powyższe języki mają dużo wad,
z czego największą jest mała ilość pracy w nich.
Dlatego piąte miejsce zostawiam dla języka z klasycznym obiektowym polimorfizmem,
w którym można pracować i zarabiać.
Te klasyczne OOP języki programowania także cały czas się rozwijają.
I także trzeba się ich cały czas uczyć.
Jeśli ktoś tego zaniedba,
to często po latach okazuje się,
że pracuje w języku,
którego już nie zna.

## Podsumowanie

Mój plan nauki języków to:

1. **[Haskell]** i dokończyć eso-assembler **[WebAssembly]** do języków ezoterycznych.
2. **[Scheme]** i napisać kompilator [eso C] do języków ezoterycznych.
3. **[Rust]** i przepisać interpretery języków ezoterycznych.
4. **[Idris]**,
   jeszcze nie wiem,
   do czego mi to potrzebne,
   ale jak się dowiem,
   to powiem o tym wszystkim.
5. **[Scala]** 3 i dostać w niej pracę.
  Może reaktywuję swojego toola do formatowania [jsona]?

[Agda]:              /langs/agda
[Clojure]:           /langs/clojure
[Common Lisp]:       /langs/common-lisp
[Coq]:               /langs/coq
[Haskell]:           /langs/haskell
[Haskella]:          /langs/haskell
[Haskellu]:          /langs/haskell
[Idris]:             /langs/idris
[Java]:              /langs/java
[JavaScriptu]:       /langs/javascript
[LISP]:              /langs/lisp
[Lispa]:             /langs/lisp
[Lispie]:            /langs/lisp
[Meta Language]:     /langs/meta-language
[OCaml]:             /langs/ocaml
[OCamla]:            /langs/ocaml
[OCamlem]:           /langs/ocaml
[PureScript]:        /langs/purescript
[Racket]:            /langs/racket
[Racketa]:           /langs/racket
[ReasonML]:          /langs/reasonml
[Rust]:              /langs/rust
[Scala]:             /langs/scala
[Scali]:             /langs/scala
[Scheme]:            /langs/scheme
[Standard ML]:       /langs/standard-ml
[Typed Clojure]:     /langs/typed-clojure
[Typed Racket]:      /langs/typed-racket
[WebAssembly]:       /langs/webassembly

[jvm]:               /tools/jvm

[ad hoc]:            /tags/ad-hoc
[Kontynuacje]:       /tags/continuation
[Kompilator]:        /tags/compiler
[Makra Higieniczne]: /tags/hygienic-macro
[interpreter]:       /tags/interpreter
[io]:                /tags/io
[jsona]:             /tags/json
[OOP]:               /tags/oop
[Traity]:            /tags/trait
[Klas Typów]:        /tags/type-class
[Klasy Typów]:       /tags/type-class

[Eso C]:             /eso/eso-c
