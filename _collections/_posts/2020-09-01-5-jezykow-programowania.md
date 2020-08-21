
Ostatnio oglądałem wypowiedź twórcy języka C++,
gdzie Bjarne Stroustrup powiedział, że zna ponad 20 języków programowania w przynajmniej podstawowym stopniu,
dodał, że dobry programista powinien znać bardzo dobrze przynajmniej 5 języków,
aby potrafił spojrzeć na problem z różnych perspektyw.
Jaką piątkę wy byście wybrali, w jakiej kolejności i dlaczego?


U mnie to wygląda tak:

## Haskell 

I to wcale nie dlatego że czysto funkcyjny

bo fulltyped
bo lazy

## Scheme

Lisp 

### Common Lisp

### Scheme

Prosty język z małym rdzeniem

Write Scheme in 48 h

To że rdzeń jest mały nie znaczy że biblioteka jest mała.
Istnieje przenośna biblioteka standardowa niezależna od implementacji

Higieniczne makra


### Racket

Ludzie nie są zgodni czy Racket jest kolejną implementacją Scheme czy już osobnym językiem programowania z powodu dużej ilości rozszeżeńļ

Racket posiada też Duża ilość bibliotek
Oraz możliwość pisania frontów do kompilatora racket.
Dzięki temu udało się tstworzyć osobny język [Typed Racket]

### Clojure

Prawdopodobnie najpopularniejszy dialekt Lispa w Polsce, a może nawet w Europie.
Jego ogromną zaletą jest to że działą na JVM i dobrze integruje się z klasami Javowymi.
Przez co jest prawdopodobnie najbardziej klasycznie obiektowym dialektem języka Lisp.
Inne dialekty Lispa także posiadają polimorfizm i mo.żliwość programowania obiektowego, ale zwykle w niestandardowy sposób

## Rust

Niskopoziomowy, uproszczony Haskell

Rustowe Traity to uproszczone KlasyTypów

Dowód na to że jest możliwe


Dla mnie ten język to fenomen.
Nie stoi za nim żadna wielka korporacja a momentalnie wbił się do 20 najpopularniejszych języków programowania według indeksu Tiobe
Higieniczne makra

## Idris

Poprawiony Haskell z typami zależnymi.
Jeszcze nie wiem co to typy zależne i czemu to potrzebuje, ale wiem rze to przyszłość.
Typy zależne są potrzebne do dowodzenia poprawności programu

W przeciwieństwie do innych języków z typami zależnymi jak Adga i  Coq,
Idris jest zaprojektowany jako język ogólnego przeznaczenia, a nie tylko do dowodzenia twierdzeń.

Ogólnego przeznaczenia

Jego największą wadą jest to że to nisza niszy

## Inny

Zostało jedno wolne miejsce, które można by przeznaczyć na

### PureScript
PureScript gorliwie ewaluaowany/oceniany Haskell dedykowany do pisania frontu

Odrzuciłem z listy ponieważ to tylko minimalnie poprawiony Haskell.
(Zreszta wiekszość tych poprawek Haskell 8 także już zawiera)
Oraz dlatego że wszystkie powyższe języki są też kompilowane do weba.
Większość jest kompilowana do JSa a Rucket do WebAssembly.

### OCaml
Obiektowy potomek [MetaLanguage] i [Standard ML].
[Standard ML] jest uważany za ostatniego wspólnego przodka Haskella i OCalma



F# i [ReasonML]

Odrzuciłem z listy, ponieważ nie uczy niczego nowego w stosunku do wymienionych.
Ma gorszy system typów niż [Haskell],
jest mniej niskopoziomowy niż [Rust]
oraz ma trudniejszą składnię niż [Typed Racket] 

Jednak ten klasyczny polimorfizm obiektowy odrzuca

### Scala

Czyli skrzyżowanie Javy i OCamla które 

### Język, w którym można zarabiać.

Powyższe języki mają dużo wad z czego największą jest mała ilość pracy w nich.
Dlatego piąte miejsce zostawiam dla języka z klasycznym obiektowym polimorfizmem w którym można pracować i zarabiać
A w czasie wolnym uczyć się powyższych czterech

Ja widzę tu jednak Scalę :D

## Podsumowanie

Mój plan nauki języków to:

1. Haskell i dokończyć eso-assembler WebAssembly do języków ezoterycznych'a
2. Scheme i napisać ezo-kompilator C do języków ezoterycznych
3. Rust i przepisać interpretery języków ezoterycznych
4. Idris jeszcze nie wiem do czego mi to potrzebne, ale jak się dowiem to powiem o tym wszystkim
5. Scala i dostać w niej pracę



