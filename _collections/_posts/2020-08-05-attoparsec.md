---
title:    'Wyrażenia regularne kontra parsery'
author:   TheKamilAdam
category: haskell-eta
tags:     assembler cisc dsl lexer misc parser regexp risc rpn
langs:    dc forth haskell joy mouse perl postscript rpl webassembly
libs:     attoparsec earley fastparser happy-alex megaparsec parsec readp trifecta
tools:
projects: helma helpa helvm
eso:      beatnik brainfuck eas eta false funge piet whitespace
redirect_from:
  - attoparsec
  - haskell-eta/attoparsec
---

Wyrażenia regularne słyną z tego,
że są trudne i mało czytelne.
Czy są dla nich jakieś alternatywy?
Tak, są to parsery.

## Rodzaje parserów

Z grubsza najczęściej występujące parsery można podzielić na:
* Parsery zstępujące (ang. *top-down parsers*).
* parsery wstępujące (ang. *bottom-up parsers*).

Dla języków programowania głównymi przedstawicielami parserów zstępujących są parsery LL, a parserów wstępujących - parsery LR.
Głowna różnica to sposób czytania:
* Parsery LL czytają tekst od lewej i analizują także od lewej.
* Parsery LR czytają tekst od lewej i analizują od prawej.

Parsery LR dzielą się dalej na:
* Kanoniczne parsery LR.
* Parsery SLR.
* Parsery LALR.

Historycznie pierwsze były parsery LL.
Jednak parsery LR są wydajniejsze niż parsery LL.
Dlatego parsery LR wyparły prawie całkowicie parsery LL z zastosowań profesjonalnych,
czyli przy tworzeniu języków programowania ogólnego przeznaczenia.
Wadą parserów LR jest jednak to,
że bardzo trudno jest pisać ręcznie
w przeciwieństwie do parserów LL,
które wręcz idealnie pisze się ręcznie.
Dlatego do implementowania prostych języków **[DSL]** lepsze są parsery LL. 

Dla **[Haskella]** istnieją cztery popularne biblioteki do pisania parserów (plus co najmniej drugie tyle mniej popularnych):
* **[Happy & Alex]**
* **[Parsec]**
* **[MegaParsec]**
* **[AttoParsec]**

Z mniej popularnych można wymienić:
* **[ReadP]**
* **[Trifecta]**
* **[FastParser]**
* **[Earley]**

### Happy & Alex

Happy & Alex haskellowe wersje klasycznej pary Yacc i Lex (lub ich darmowych odpowiedników Bison i Flex).
Jedyne w tym zestawieniu pozwalają na używanie gramatyk LR.
Wadą pary Happy & Alex jest to,
że trzeba napisać gramatykę,
czyli nauczyć się nowego języka niewiele ładniejszego niż wyrażenia regularne.

### Parsec i MegaParsec

Parsec jest to klasyczna haskellowa biblioteka do pisania parserów
oraz klasyczny przedstawiciel biblioteki kombinatorów parserów (ang. *[Parser combinator](https://en.wikipedia.org/wiki/Parser_combinator)*).
Biblioteki kombinatory parserów są dedykowane do ręcznego pisania parserów.
Tak utworzone parsery są bardzo podobne do parserów LL i posiadają ich zalety,
jednocześnie eliminując wady parserów LL,
czyli problem lewostronnej rekurencji. 
Parsec niestety nie jest już rozwijany.
Zamiast tego jest rozwijana jego ulepszona wersja,
czyli MegaParsec.

### AttoParsec
AttoParsec to biblioteka oryginalnie pomyślana do parsowania logów na bieżąco.
Jest szybsza niż MegaParsec czy Parsec oraz jako jedyna umożliwia parsowanie przyrastającego pliku linia po linii.
Niestety jest też o wiele uboższa w składni,
przez co jest uważana za dobrą do parsowania plików o prostszej strukturze.
Ubogość składni przekłada się także na prostszą strukturę samej biblioteki.
Dzięki czemu o wiele prościej się jej nauczyć.

## Tekst o prostej strukturze

Ponieważ moim marzeniem jest skompilowanie C do **[BrainFucka]**.
Jako przykład nie będziemy parsować logów,
tylko plik z językiem asemblera.
Wcześniej jednak zobaczmy rodzaje języków asemblera.

### Rodzaje języków assemblera

Składnia języka asemblera jest mocno powiązana z [modelem programowym procesora](https://pl.wikipedia.org/wiki/Model_programowy_procesora) 
(ang. *[Instruction set architecture](https://en.wikipedia.org/wiki/Instruction_set_architecture)*, **[ISA]**).

Trzy najczęściej spotykane typy modeli programowych procesora to:
* CISC (ang. Complex Instruction Set Computing)
* RISC (ang. Reduced Instruction Set Computing)
* MISC (ang. Minimal Instruction Set Computing)

#### CISC
**[CISC]** historycznie był pierwszym modelem.
Charakteryzował się skomplikowanymi rozkazami ze skomplikowanymi sposobami adresowania.
Miało to ułatwić pisanie kompilatorów.
Nie ułatwiło.
Żyjącym przedstawicielem tego modelu jest x86.
Także mikroprocesor 8051 bywa uważany za przedstawiciela modelu **[CISC]**.

#### RISC
**[RISC]** został stworzony jako reakcja na to,
że model CISC okazał się jednak ślepą uliczką.
Skomplikowane rozkazy i sposoby adresowania nie pomagały w pisaniu kompilatorów.
W związku z tym postanowiono pójść w drugą skrajność maksymalnie upraszczając listę rozkazów oraz sposoby adresowania.
Wynikiem tego była prostsza jednostka arytmetyczno-logiczna.
Zaoszczędzone tranzystory przeznaczano na większą ilość rejestrów ogólnego przeznaczenia.
Prawdopodobnie najpopularniejszym przedstawicielem tego modelu jest ARM.
Także mikroprocesory AVR bywają uważane za przedstawicieli modelu **[RISC]**.

#### MISC
**[MISC]** był rozwijany niezależnie.
Charakteryzuje się bardzo małą ilością rozkazów.
Niektóre języki assemblera **[MISC]** wyglądają wręcz jak języki ezoteryczne.

Nie jest to w zasadzie wymagane,
ale zwykle maszyny MISC to [maszyny stosowe](https://pl.wikipedia.org/wiki/Maszyna_stosowa) nie posiadające rejestrów,
tylko wszystkie operacje wykonujące na stosie.
Rozwiązuje to także problem adresowania, 
bo większość operacji wykonywanych na stosie to rozkazy 0-adresowe,
ewentualnie 1-adresowe.

Co ciekawe rzeczywiste procesory są rzadko budowane jako maszyny stosowe.
Prawdopodobnie najbardziej znanym procesorem stosowym był [Transputer](https://en.wikipedia.org/wiki/Transputer).
Także koprocesor matematyczny był maszyną stosową.

O wiele częściej maszynami stosowymi są maszyny wirtualne jak **[JVM]**, [Wirtualna Maszyna Perla](https://pl.wikipedia.org/wiki/Kod_bajtowy_Perla) czy **[WebAssembly]**.
Wiele języków ezoterycznych jak **[ETA]**, **[False]**, **[Funge]**, **[Piet]**, **[WhiteSpace]** to języki stosowe.
Istnieją też nieezoteryczne wysokopoziomowe języki stosowe (ang. stack-based) jak **[dc]**, **[Joy]**, **[Forth]**, **[Mouse]**, **[PostScript]** i **[RPL]**.

Najprostszy możliwy zestaw instrukcji dla maszyny stosowej został opisany jako [A Minimal CISC](http://homepage.divms.uiowa.edu/~jones/arch/cisc/)).
Zawiera on tylko 8 instrukcji.
Ja jednak zdecydowałem się na inny zestaw instrukcji będący językiem ezoterycznym **[ETA]**.

### ETA i EAS

Język **[ETA]** posiada także prosty asembler (i język asemblerowy o tej samej nazwie) **[EAS]**.
Jest on co prawda zaimplementowany Perlu,
ale przy pomocy wyrażeń regularnych.
Oryginalny jest dostępny [tutaj](http://www.miketaylor.org.uk/tech/eta/src/eas),
a mirror [tutaj](https://github.com/helvm/eta/blob/master/src/eas).
Implementacja tego asemblera będzie tematem kolejnego artykułu. 

*Update: Implementacja parsera (i całego programu montującego) jest dostępna w następnym artykule **[attoparsec-eas](/attoparsec-eas)***.

## Podsumowanie

**[Haskell]** posiada wiele sposobów na sparsowanie tekstu. 
Warto je rozważyć,
zanim sięgniemy po wyrażenia regularne.
Zwłaszcza jeśli wyrażenia regularne miałyby być bardzo skomplikowane. 


[dc]:                   /langs/dc
[Forth]:                /langs/forth
[Haskell]:              /langs/haskell
[Haskella]:             /langs/haskell
[Joy]:                  /langs/joy
[Perl]:                 /langs/perl
[Mouse]:                /langs/mouse
[PostScript]:           /langs/postscript
[RPL]:                  /langs/rpl
[WebAssembly]:          /langs/webassembly

[AttoParsec]:           /libs/attoparsec
[Earley]:               /libs/earley
[FastParser]:           /libs/fastparser
[Happy & Alex]:         /libs/happy-alex
[MegaParsec]:           /libs/megaparsec
[Parsec]:               /libs/parsec
[ReadP]:                /libs/readp
[Trifecta]:             /libs/trifecta

[HelMA]:               /projects/helma
[HelPA]:                /projects/helpa
[HelVM]:                /projects/helvm

[Beatnik]:              /eso/beatnik
[BrainFucka]:           /eso/brainfuck
[EAS]:                  /eso/eas
[ETA]:                  /eso/eta
[False]:                /eso/false
[Funge]:                /eso/funge
[Piet]:                 /eso/piet
[WhiteSpace]:           /eso/whitespace

[asembler]:             /tags/assembler
[CISC]:                 /tags/cisc
[DSL]:                  /tags/dsl
[lekser]:               /tags/lexer
[MISC]:                 /tags/misc
[parser]:               /tags/parser
[regexp]:               /tags/regexp
[RISC]:                 /tags/risc
[RPN]:                  /tags/rpn
