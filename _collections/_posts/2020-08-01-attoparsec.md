---
title:    'Wyrażenia regularne kontra parsery'
author:   TheKamilAdam
category: haskell-eta
tags:     assembler cisc dsl misc parser lexer regexp risc rpn
langs:    dc forth haskell joy mouse postscript rpl webassembly
libs:     attoparsec megaparsec
tools:
projects: helcam helpa helvm
eso:      beatnik eas eta false funge piet whitespace
redirect_from:
  - attoparsec
  - haskell-eta/attoparsec
---

Wyrażenia regularne są trudne i mało czytelne.
Czy są dla nich jakieś alternatywy?
Tak, są to parsery.



## Rodzaje parserów

Z grubsza najczęściej występujące parsery można podzielić na zstępująca (ang. *top-down parsers*) i wstępujące (ang. *bottom-up parsers*)
Dla języków programowania głównymi przedstawicielami parserów zstępujących są parsery LL,
a parserów wstępujących - parsery LR (które dalej dzielą się na kanoniczne parsery LR, parsery SLR, i parsery LALR)
* Parsery LL czytają tekst od lewej i analizują także od lewej.
* Parsery LR czytają tekst od lewej i analizują od prawej.

Historycznie pierwsze były parsery LL.
Jednak parsery LR są wydajniejsze niż parsery LL.
Dlatego parsery LR wyparły prawie całkowicie parsery LL z zastosowań profesjonalnych.
Wadą parserów LR jest jednak to,
że bardzo trudno pisać ręcznie, w przeciwieństwie do parserów LL,
które wręcz idealnie pisze się ręcznie.
Dlatego do implementowania języków **[DSL]** lepsze są parsery LL. 

Dla Haskella istnieją cztery popularne biblioteki do pisania parserów (plus co najmniej drugie tyle mniej popularnych):
* Happy & Alex
* Parsec
* MegaParsec
* AttoParsec

Z mniej popularnych można wymienić:
* ReadP
* Trifecta
* FastParser
* Earley

### Happy & Alex

Happy & Alex haskellowe wersje klasycznej pary Yacc i Lex (lub ich darmowych odpowiedników Bison i Flex).
Jedyne w tym zestawieniu pozwalają na używanie gramatyk LR.
Wadą pary Happy & Alex jest to,
że trzeba napisać gramatykę.
czyli nauczyć się nowego języka niewiele ładniejszego niż wyrażenia regularne.

### Parsec i MegaParsec

Parsec jest to klasyczna haskellowa biblioteka do pisania parserów
oraz klasyczny przedstawiciel biblioteki kombinatorów parserów (ang. *[Parser combinator](https://en.wikipedia.org/wiki/Parser_combinator)*)
Biblioteki kombinatory parserów są dedykowane do ręcznego pisania parserów.
Tak utworzone parsery są bardzo podobne do parserów LL.
Parsec niestety nie jest już rozwijany.
Zamiast tego jest rozwijana jego ulepszona wersja, czyli MegaParsec.

### AttoParsec
AttoParsec to biblioteka oryginalnie pomyślana do parsowania logów na bieżąco.
Jest szybsza niż MegaParsec czy Parsec oraz jako jedyna umożliwia parsowanie przyrastającego pliku linia po linii.
Niestety jest też o wiele uboższa w składni,
przez co jest uważana za dobrą do parsowania plików o prostszej strukturze.
Ubogość składni przekłada się także na prostszą strukturę samej biblioteki.
Dzięki czemu o wiele prościej się jej nauczyć.

## Tekst o prostej strukturze - Assembler





Składnia assemblera jest mocno powiązana  [modelem programowym procesora](https://pl.wikipedia.org/wiki/Model_programowy_procesora) 
(ang. *[Instruction set architecture](https://en.wikipedia.org/wiki/Instruction_set_architecture)*, **[ISA]**)

Trzy najczęściej spotykane typy modeli programowych procesora to:
* CISC (ang. Complex Instruction Set Computing)
* RISC (ang. Reduced Instruction Set Computing)
* MISC (ang. Minimal Instruction Set Computing)

**[CISC]** historycznie był pierwszym modelem.
Charakteryzował się skomplikowanymi rozkazami ze skomplikowanymi sposobami adresowania.
Miało to ułatwić pisanie kompilatorów.
Nie ułatwiło.
Żyjącem przedstawicielem tego modelu jest x86.
Także assembler mikroprocesora 8051 bywa uważany za przedstawiciela modelu CISC.

**[RISC]** powstał jak reakcja na to,
że CISCi okazało się jednak ślepą uliczką.
Skomplikowane rozkazy i sposoby adresowania nie pomagały jednak w pisaniu kompilatorów.
W związku z tym postanowiono pójść w drugą skrajność upraszczając maksymalnie listę rozkazów oraz sposoby adresowania.
Wynikiem tego była prostsza jednostka arytmetyczno-logiczna.
Zaoszczędzone tranzystory przeznaczano na większą ilość rejestrów ogólnego przeznaczenia.
Prawdopodobnie najpopularniejszym przedstawicielem tego modelu jest ARM.
Także assembler mikroprocesorów AVR bywa uważany za przedstawiciela modelu RISC.

**[MISC]** był rozwijany niezależnie.
Charakteryzuje się bardzo małą ilością rozkazów.
Niektóre assemblery **[MISC]** wyglądają wręcz jak języki ezoteryczne.

Nie jest to w zasadzie wymagane, ale zwykle MISCy to zwykle maszyny stosowe.
Rozwiązuje to także problem adresowania (większość operacji wykonywanych na stosie to rozkazy 0 adresowe, ewentualnie 1 adresowe).

Co ciekawe rzeczywiste procesory są rzadko budowane jako maszyny stosowe.
O wiele częściej procesory wirtualne jak JVM, Wirtualna Maszyna Perla czy WebAssembly są maszynami stosowymi.
Wiele języków ezoterycznych jak **[ETA]**  **[False]**, **[Funge]**, **[Piet]**, **[WhiteSpace]** to języki stosowe.

Teoretycznie najprostszy możliwy assembler dla maszyny stosowej wystarczyłoby,
żeby zawierał 8 prostych instrukcji (Zobacz [A Minimal CISC](http://homepage.divms.uiowa.edu/~jones/arch/cisc/)))






Jak już mowa o językach ezoterycznych.
Naszym prostym assemblerem do sparsowania będzie **EAS**.
**EAS** (ETA Assembler) - to para assembler dla ezoterycznego języka programowania **ETA**.

Zarówno jak i **ETA** to języki stosowe (ang. stack-based).

Istnieje też wiele nieezoterycznych języków 





Minimalistyczny asembler stosowy bez makr.

Głównie rozwiązywanie liczb i etykiet.

Minimalizm osiąga się poprzez wykonywanie operacji na stosie.
maszyna stosowa (https://pl.wikipedia.org/wiki/Maszyna_stosowa)


maszyny
https://en.wikipedia.org/wiki/Transputer


Maszyny wirtualne
JVM 
webassembly

Języki stosowe stack-based 
dc forth mouse postcript rpl joy


https://pl.wikipedia.org/wiki/OSV

Object Subject Verb

https://en.wikipedia.org/wiki/Mouse_(programming_language)
https://en.wikipedia.org/wiki/PostScript



## Struktura Asemblera

Cztery główne modułu programu to:
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

### Parser asemblera

Pierwszym modyłem programu jest parser:
```haskell
parseAssembler :: T.Text -> Either String InstructionList
parseAssembler = parseOnly instructionListParser

instructionListParser :: Parser InstructionList
instructionListParser = skipManyComment *> skipHorizontalSpace *> many (instructionParser <* skipHorizontalSpace)
```
Parser pobiera zmienną typu `Text` i przypadku powodzenia zwraca listę sparsowanych instrukcji.
W przypadku niepowodzenia zwraca mało intuicyjny komunikat błędu. 

Parsowanie pojedynczej instrukcji dzieli się na kilka przypadków: 

```haskell
instructionParser :: Parser Instruction
instructionParser =
  try zeroOperandInstructionParser
  <|> numberOperandInstructionParser
  <|> labelParser
  <|> unescapedStringParser
  <|> includeParser
  <|> lineBreakParser
  <|> commentParser
```
Czyli:
* parsowanie instrukcji bezargumentowej
* parsowanie liczby
* parsowanie końca linii
* parsowanie dołączenia pliku
* parsowanie deklaracji etykiety
* parsowanie literału stringa
* parsowanie komentarza

EAS jak to assembler maszyny stosowej zawiera wiele rozkazów bezargumentowych.
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
Jest tu umieszczenie liczby na stosie.
Przy czym liczbą może być wartość podana wprost, wartośc literału znakowego lub adres etykiety.
Dodatkowo posiada także możliwość dołączenia (ang. *incluDe*) pliku oraz zdefiniowania etykiety (ang. *Label*).
Ja rozszeżyłem to jeszcze o możliwość umieszczenie (ang. *pUt*) stringa na stosie
```haskell
numberOperandInstructionParser :: Parser Instruction
numberOperandInstructionParser = N <$> (
      naturalValueParser
  <|> (asciiCI "N" *> skipHorizontalSpace *> naturalValueParser)
  <|> (asciiCI "Number" *> endWordParser *> skipHorizontalSpace *> naturalValueParser)
  )

labelParser :: Parser Instruction
labelParser = L <$> (char '>' *> identifierParser <* char ':')

unescapedStringParser :: Parser Instruction
unescapedStringParser = U <$> stringParser

includeParser :: Parser Instruction
includeParser = D <$> (char '*' *> fileNameParser <* char '\n')
```

Ponieważ w EAS znaki końca linii () są znaczące trzeba je parsować w sposób świadomy 
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


Problem końca wyrazu to coś co zajeło mi jeden wieczór.
W wyrażeniach regularnych było by to proste `\b`.
Tutaj jednak trzeba było napisać ręcznie funkcję wykrywania końca wyrazu oraz funkcję pobierajacą wszystko przed końcem wyrazy.
Ostatecznie stwierdziłęm że koniec wyrazu to albo miały znak, albo początek komentarza
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
Dołącza on plik biblioteczny w miejsce wystąpienia `*nazwa_biblioteki.wsa` 
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

### Reduktor Instrukcji

Redukcja siły operatorów (ang. [Strength reduction](https://en.wikipedia.org/wiki/Strength_reduction))
jest częścią optymalizacji.
W naszy przypadku mamy reukcę skomplikowanych instrukcji na instrukce proste.

* Redukcja etykiet - wyliczanie adresów skoków
* Redukcja stringów - zamiana na znaki

```haskell
replaceLabels ::  LabelAddresses -> InstructionList -> InstructionList
replaceLabels addresses il = replaceLabel addresses <$> il

replaceLabel :: LabelAddresses -> Instruction -> Instruction
replaceLabel addresses (N (Variable l)) = N $ Literal $ findOrError l addresses
replaceLabel _          i               = i
```

```haskell
replaceStrings :: InstructionList -> InstructionList
replaceStrings il = replaceString =<< il

replaceString :: Instruction -> InstructionList
replaceString (U s) = charToInstruction <$> reverse s 
replaceString  i    = [i]

charToInstruction :: Char -> Instruction
charToInstruction c = N $ Literal $ fromIntegral $ ord c

```

### Generator kodu docelowego

Oprocz parsera jest potrzebny także generator kodu docelowego

Kodowanie instrukcji.
Wyliczanie etykiety skoków  i zmiennych.
Kodowanie wartości umieszczanych na stosie.
Obsługa wartości ujemnych (zamiana na odejmowanie)

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

To dopiero początek

Assember


WhiteSpace - etykiety, podprogramy
Piet - makra, call makra
Subleq - higieniczne makr?

BrainFuck - makra wieloniowe, szablony, parsowane z zewnetrzego źródła

