---
title:    'Wyrażenia regularne kontra parsery'
author:   TheKamilAdam
category: haskell-eta
tags:     assembler cisc dsl lexer misc parser regexp risc rpn
langs:    dc forth haskell joy mouse perl postscript rpl webassembly
libs:     attoparsec earley fastparser happy-alex megaparsec parsec readp trifecta
tools:
projects: helcam helpa helvm
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

Z grubsza najczęściej występujące parsery można podzielić na zstępujące (ang. *top-down parsers*) i wstępujące (ang. *bottom-up parsers*).
Dla języków programowania głównymi przedstawicielami parserów zstępujących są parsery LL,
a parserów wstępujących - parsery LR (które dalej dzielą się na kanoniczne parsery LR, parsery SLR, i parsery LALR).
Parsery LL czytają tekst od lewej i analizują także od lewej.
Parsery LR czytają tekst od lewej i analizują od prawej.

Historycznie pierwsze były parsery LL.
Jednak parsery LR są wydajniejsze niż parsery LL.
Dlatego parsery LR wyparły prawie całkowicie parsery LL z zastosowań profesjonalnych,
czyli przy tworzeniu języków programowania ogólnego przeznaczenia.
Wadą parserów LR jest jednak to,
że bardzo trudno jest pisać ręcznie
w przeciwieństwie do parserów LL,
które wręcz idealnie pisze się ręcznie.
Dlatego do implementowania prostych języków **[DSL]** lepsze są parsery LL. 

Dla Haskella istnieją cztery popularne biblioteki do pisania parserów (plus co najmniej drugie tyle mniej popularnych):
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

Ponieważ moim marzeniem jest skompilowanie C do BrainFucka.
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

**[CISC]** historycznie był pierwszym modelem.
Charakteryzował się skomplikowanymi rozkazami ze skomplikowanymi sposobami adresowania.
Miało to ułatwić pisanie kompilatorów.
Nie ułatwiło.
Żyjącym przedstawicielem tego modelu jest x86.
Także mikroprocesor 8051 bywa uważany za przedstawiciela modelu **[CISC]**.

**[RISC]** został stworzony jako reakcja na to,
że model CISC okazał się jednak ślepą uliczką.
Skomplikowane rozkazy i sposoby adresowania nie pomagały w pisaniu kompilatorów.
W związku z tym postanowiono pójść w drugą skrajność maksymalnie upraszczając listę rozkazów oraz sposoby adresowania.
Wynikiem tego była prostsza jednostka arytmetyczno-logiczna.
Zaoszczędzone tranzystory przeznaczano na większą ilość rejestrów ogólnego przeznaczenia.
Prawdopodobnie najpopularniejszym przedstawicielem tego modelu jest ARM.
Także mikroprocesory AVR bywają uważane za przedstawicieli modelu **[RISC]**.

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

Najprostszy możliwi zestaw instrukcji dla maszyny stosowej został opisany jako [A Minimal CISC](http://homepage.divms.uiowa.edu/~jones/arch/cisc/)).
Zawiera on tylko 8 instrukcji.
Ja jednak zdecydowałem się na inny zestaw instrukcji będący językiem ezoterycznym **[ETA]**.

### ETA i EAS

Język **[ETA]** posiada także prosty assembler, i język asemblerowy o tej samej nazwie, **[EAS]**.
Jest on co prawda zaimplementowany Perlu,
ale przy pomocy wyrażeń regularnych.
Oryginalny jest dostępny [tutaj](http://www.miketaylor.org.uk/tech/eta/src/eas),
a mirror [tutaj](https://github.com/helvm/eta/blob/master/src/eas).

## Struktura asemblera, czyli programu asemblującego (montującego)

Cztery główne moduły asemblera to:
* Parser asemblera EAS
* Konsolidator (ang. Linker)
* Reduktor (ang. Reducer) instrukcji
* Generator kodu wynikowego, czyli kodu ETA

```haskell
assemblyIO :: String -> String -> IO (Either String String)
assemblyIO dirName fileName = runExceptT $ assembly dirName fileName

assembly :: String -> String -> ExceptT String IO String
assembly dirName fileName = generateCode . reduce <$> link dirName fileName
```

Najpierw naprzemiennie następuje faza parsowania i konsolidacji.
Następnie następuje redukcja instrukcji i generacja kodu docelowego.

### Parser języka asemblera

Pierwszym modułem programu jest parser:
```haskell
parseAssembler :: T.Text -> Either String InstructionList
parseAssembler = parseOnly instructionListParser

instructionListParser :: Parser InstructionList
instructionListParser = skipManyComment *> skipHorizontalSpace *> many (instructionParser <* skipHorizontalSpace)
```
Parser pobiera zmienną typu `Text` i w przypadku powodzenia zwraca listę sparsowanych instrukcji.
W przypadku niepowodzenia zwraca mało intuicyjny komunikat o błędzie generowany przez bibliotekę **[AttoParsec]**.

Parsowanie pojedynczej instrukcji dzieli się na kilka przypadków:
```haskell
instructionParser :: Parser Instruction
instructionParser =
  try zeroOperandInstructionParser
  <|> naturalNumberParser
  <|> unescapedStringParser
  <|> labelDefinitionParser
  <|> includeFileParser
  <|> lineBreakParser
  <|> commentParser
```
Czyli:
* parsowanie instrukcji bezargumentowej
* parsowanie liczby naturalnej
* parsowanie literału stringa
* parsowanie deklaracji etykiety
* parsowanie dołączenia pliku
* parsowanie końca linii
* parsowanie komentarza

**[EAS]** jak to język assemblera dla maszyny stosowej zawiera wiele rozkazów bezargumentowych:
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

Oryginalnie EAS posiada tylko jeden rozkaz jednoargumentowy. 
Jest tu umieszczenie liczby naturalnej na stosie.
Przy czym liczbą może być wartość podana wprost,
wartość literału znakowego lub adres etykiety.
Ja rozszerzyłem to jeszcze o możliwość umieszczenie nieescepowanego literału znakowego na stosie.
Dodatkowo posiada także możliwość dołączenia (ang. *incluDe*) pliku oraz zdefiniowania etykiety (ang. *Label*):
```haskell
naturalNumberParser :: Parser Instruction
naturalNumberParser = N <$> (
      naturalValueParser
  <|> (asciiCI "N" *> skipHorizontalSpace *> naturalValueParser)
  <|> (asciiCI "Number" *> endWordParser *> skipHorizontalSpace *> naturalValueParser)
  )

unescapedStringParser :: Parser Instruction
unescapedStringParser = U <$> stringParser

labelDefinitionParser :: Parser Instruction
labelDefinitionParser = L <$> (char '>' *> identifierParser <* char ':')

includeFileParser :: Parser Instruction
includeFileParser = D <$> (char '*' *> fileNameParser <* char '\n')
```

Ponieważ w **[EAS]** znaki końca linii są znaczące to trzeba je parsować w sposób świadomy:
```haskell
lineBreakParser :: Parser Instruction
lineBreakParser = R <$ (skipMany1EndLine *> skipManyComment)

commentParser :: Parser Instruction
commentParser = skipComment *> lineBreakParser

skipManyComment :: Parser [()]
skipManyComment = many (skipComment <* skipMany1EndLine)

skipComment :: Parser ()
skipComment = char commentChar *> skipAllToEndOfLine

skipMany1EndLine :: Parser String
skipMany1EndLine = many1 (char '\n')
```

Problem końca wyrazu (identyfikatora) to coś,
co zajęło mi jeden wieczór.
W wyrażeniach regularnych byłoby to proste `\b`.
Tutaj jednak trzeba było napisać ręcznie funkcję wykrywania końca wyrazu oraz funkcję pobierającą wszystko przed końcem wyrazu.
Ostatecznie stwierdziłem,
że koniec wyrazu to albo biały znak,
albo początek komentarza:
```haskell
endWordParser :: Parser T.Text
endWordParser = takeTill isEndWord

isEndWord :: Char -> Bool
isEndWord c = isSpace c || (c == commentChar)

commentChar :: Char
commentChar = '#'
```

### Konsolidator

Konsolidator (ang. linker) to program łączący pliki.
Dołącza on plik biblioteczny w miejsce wystąpienia `*nazwa_biblioteki.wsa`:
```haskell
link :: String -> String -> ExceptT String IO InstructionList
link dirName fileName = includeFiles $ ExceptT $ parseAssembler <$> T.readFile (dirName ++ "/" ++ fileName) where

  includeFiles :: ExceptT String IO InstructionList -> ExceptT String IO InstructionList
  includeFiles expect = loadFiles =<< expect

  loadFiles :: InstructionList -> ExceptT String IO InstructionList
  loadFiles il = concat <$> mapM loadFile il

  loadFile :: Instruction -> ExceptT String IO InstructionList
  loadFile (D libName) = link dirName libName
  loadFile i = pure [i]
```
Ponieważ dołączana biblioteka może zawierać inne biblioteki to funkcja link jest funkcją rekurencyjną.

### Reduktor instrukcji

Redukcja siły operatorów (ang. [Strength reduction](https://en.wikipedia.org/wiki/Strength_reduction))
jest częścią optymalizacji.
W naszym przypadku mamy redukcję skomplikowanych instrukcji na instrukcje proste.
Reduktor instrukcji jest to w zasadzie uproszczony selektor instrukcji,
który normalnie jest częścią generatora kodu docelowego.

Redukcja etykiet,
czyli wyliczanie adresów skoków:
```haskell
replaceLabels ::  LabelAddresses -> InstructionList -> InstructionList
replaceLabels addresses il = replaceLabel addresses <$> il

replaceLabel :: LabelAddresses -> Instruction -> Instruction
replaceLabel addresses (N (Variable l)) = N $ Literal $ findOrError l addresses
replaceLabel _          i               = i
```

Redukcja literałów stringów,
czyli zamiana na literały znaków:
```haskell
replaceStrings :: InstructionList -> InstructionList
replaceStrings il = replaceString =<< il

replaceString :: Instruction -> InstructionList
replaceString (U s) = charToInstruction <$> reverse s 
replaceString  i    = [i]

charToInstruction :: Char -> Instruction
charToInstruction c = N $ Literal $ fromIntegral $ ord c
```

### Właściwy generator kodu docelowego

Ponieważ wszystkie potrzebne operacje zostały już wykonane,
to właściwy generator kodu docelowego zamienia naszą zredukowaną listę instrukcji na język **ETA**:
```haskell
generateCode :: InstructionList -> String
generateCode il = show . WhiteInstruction =<< il

newtype WhiteInstruction = WhiteInstruction Instruction

instance Show WhiteInstruction where
  show (WhiteInstruction (N (Literal  n))) = "N" ++ showValue n ++ "e"
  show (WhiteInstruction (N (Variable i))) = error $ show i
  show (WhiteInstruction (D i))            = error $ show i
  show (WhiteInstruction (U i))            = error $ show i
  show (WhiteInstruction (L _))            = ""
  show (WhiteInstruction R)                = "\n"
  show (WhiteInstruction i)                = show i

showValue :: Natural -> String
showValue value = naturalToChar <$> naturalToDigits7 value

naturalToChar :: Natural -> Char
naturalToChar index = ['h', 't', 'a', 'o', 'i', 'n', 's'] `genericIndex` index
```

## Podsumowanie

AttoParsec jest prostą biblioteką.
Czy tak samo prostą jak wyrażenia regularne?
Tego nie wiem,
ale na pewno jest dużo czytelniejszy.
Jednocześnie AttoParsec posiada o wiele większe możliwości.
Szkoda,
że nie jest przeportowany do innych języków programowania niż **[Haskell]**.

Kod assemblera **[EAS]** jest dostępny na [GitHubie](https://github.com/helvm/helpa/releases/tag/v0.2.1.0).
Kod interpretera **[ETA]** też jest dostępny na [GitHubie](https://github.com/helvm/helcam/releases/tag/v0.6.0.0).

[dc]:                   /langs/dc
[Forth]:                /langs/forth
[Haskell]:              /langs/haskell
[Joy]:                  /langs/joy
[Perl]:                 /langs/perl
[Mouse]:                /langs/mouse
[PostScript]:           /langs/postscript
[RPL]:                  /langs/rpl
[WebAssembly]:          /langs/webassembly

[AttoParsec]:           /libs/attoparsec
[Earley]:               /libs/earley
[Happy & Alex]:         /libs/happy-alex
[MegaParsec]:           /libs/megaparsec
[Parsec]:               /libs/parsec
[ReadP]:                /libs/readp
[Trifecta]:             /libs/trifecta

[HelCam]:               /projects/helcam
[HelPA]:                /projects/helpa
[HelVM]:                /projects/helvm

[Beatnik]:              /eso/beatnik
[BrainFucka]:           /eso/brainfuck
[EAS]:                  /eso/eas
[ETA]:                  /eso/eta
[False]:                /eso/false
[Funge]:                /eso/funge
[Piet]:                 /eso/piet
[WhiteScape]:           /eso/whitespace

[asembler]:             /tags/assembler
[CISC]:                 /tags/cisc
[DSL]:                  /tags/dsl
[lekser]:               /tags/lexer
[MISC]:                 /tags/misc
[parser]:               /tags/parser
[regexp]:               /tags/regexp
[RISC]:                 /tags/risc
[RPN]:                  /tags/rpn
