---
title:    'Wyrażenia regularne kontra parsery'
author:   TheKamilAdam
category: haskell-eta
tags:     assembler misc parser lexer
langs:    dc forth haskell joy mouse postscript rpl webassembly
libs:     attoparsec megaparsec
tools:    cabal etlas jvm misc
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

Z grubsza najczęściej występujące parsery można podzielić na RL i LL

Dla Haskella istnieją cztery popularke biblioteki do pisania parserów (plus co najmniej drugie tyle mniej popularnych)

* Happy & Alex
* Parsec
* MegaParsec
* AttoParsec


### Happy & Alex

Happy & Alex haskellowe wersje klasycznej pary Yacc i Lex (lub ich darmowych odpowiedników Bison i Flex).
Jedyne w tym zestawieniu pozwalają na używanie gramatyk LR.
Wadą pary Happy & Alex jest to,
że trzeba napisać gramatykę.
czyli nauczyć się nowego języka niewiele ładniejszego niż wyrażenia regularne.

### Parsec

Parsec jest to klasyczna biblioteka do pisania parserów.
Pozwala Combinatory (ang. [Parser combinator](https://en.wikipedia.org/wiki/Parser_combinator))

### MegaParsec

Parsec niestety nie jest już rozwijany.
Zamiast tego jest rozwijana jego ulepszona wersja, czyli MegaParsec.


### AttoParsec
AttoParsec to biblioteka oryginalnie pomyślana do parsowania logów na bieżąco.
Jest szybsza niż MegaParsec czy Parsec oraz jako jedyna umożliwia parsowanie przyrastającego pliku linia po linii.
Niestety jest o wiele uboższa w składni przez co jest uważana za dobrą do parsowania plików o prostrzej strukturze.
Ubogość sładni przekłada się także na prostrzą strukturę samej biblioteki.
Dzięki czemu o wiele prościej się jej nauczyć.

Skoro już wybrałem AttoParsec to przydałyby się jakieś logi do sparsowania.
Logów jednak nie mam w zwiazku z tym będę parsować kod źródłowy napisany w EAS.

## Wyzwanie - pseudo asembler

Gramatyka assemblera jest mocno powiązana z typem [modelu programowym procesora](https://pl.wikipedia.org/wiki/Model_programowy_procesora) 
(ang. *[Instruction set architecture](https://en.wikipedia.org/wiki/Instruction_set_architecture)*, **[ISA]**)



Trzy najczęściej spotykane typy modeli programowych procesora to:
* CISC (ang. Complex Instruction Set Computing)
* RISC (ang. Reduced Instruction Set Computing)
* MISC (ang. Minimal Instruction Set Computing)


CISC historycznie był pierwszym modelem.
Charakteryzował się skomplikowanymi rozkazami ze skomplikowanymi sposobami adresowania.
Miało to ułatwić pisanie kompilatorów.

RISC powstał jak reakcja na to że CISCi okazało się jednak ślepą uliczką.
Skomplikowane rozkazy i sposoby adresowania nie Nie pomoagały jednak w pisaniu kompilatorów.
W związku z tym postanowiono pójść w drugą skrajnośc upraszczając maksymalnie listę rozkazów oraz sposoby adresowania.
Wynikiem tego była prostsza jednostka arytmetyczno-logiczna.
Zaoszczędzone tranzystory przeznaczano na wiekszą ilość rejestrów ogólnego przeznaczenia.

MISC rozwijany niezależnie.
Charakteryzuje się bardzo małą ilością rozkazów.
Niektóre assemblery MISC wyglądają wręcz jak języki ezoteryczne.

Nie jest to w zasadzie wymagane, ale zwykle MISCy to zwykle maszyny stosowe.
Rozwiązuje to także problem adresowania (operacji wykonywanych na stosie to rozkazy 0 adresowe, ewentualnie 1 adresowe)

Co ciekawe rzeczywiste procesory są rzadko budowane jako maszyny stosowe.
O wiele częściej procesory wirtualne jak JVM, Wirtualna Maszyna Perla czy WebAssembly są maszynami stosowymi.
Wiele języków ezoterycznych jak **[ETA]**  **[False]**, **[Funge]**, **[Piet]**, **[WhiteSpace]** to języki stosowe.

Teoretycznie najprostrzy możliwy assembler dla maszyny stosowej wystarczyło by żeby zawierał 8 prostych instrukcji. 


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

* Parser asemblera, czyli EAS
* Konsolidator (ang. Linker)
* Reduktor (ang. Reducer) instrukcji
* Generator kodu wynikowego, czyli ETA

```haskell
assemblyIO :: String -> String -> IO (Either String String)
assemblyIO dirName fileName = runExceptT $ assembly dirName fileName

assembly :: String -> String -> ExceptT String IO String
assembly dirName fileName = generateCode . reduce <$> link dirName fileName
```

Najpierw naprzemiennie następuje faza parsowania i konsolidacji.
Następnie następuje redukcja instrukcji i generacja kodu docelowego.

### Parser asemblera



Parsowanie liczb nieujemnych całkowitych
Parsowanie liczb ujemnych calkowitych
Parsowanie etykiet
Parsowanie identyfikatorów (zależne od jezyka docelowego)


Naszym tekstem do sparsowania jest
Assembly Programming Language.
Jest to prosta nakladna na języki WS, ETA i SQ.

```haskell
parseAssembler :: T.Text -> Either String InstructionList
parseAssembler = parseOnly instructionListParser

instructionListParser :: Parser InstructionList
instructionListParser = skipManyComment *> skipHorizontalSpace *> many (instructionParser <* skipHorizontalSpace) -- <* endOfInput

instructionParser :: Parser Instruction
instructionParser =
  try zeroOperandInstructionParser
  <|> numberOperandInstructionParser
  <|> rParser
  <|> dParser
  <|> lParser
  <|> uParser
  <|> commentParser
```

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
    where zeroOperandInstruction i ts = (asciiCIChoices ts *> endWordParser) $> i

numberOperandInstructionParser :: Parser Instruction
numberOperandInstructionParser = N <$> (
      naturalValueParser
  <|> (asciiCI "N" *> skipHorizontalSpace *> naturalValueParser)
  <|> (asciiCI "Number" *> endWordParser *> skipHorizontalSpace *> naturalValueParser)
  )

rParser :: Parser Instruction
rParser = (skipMany1EndLine *> skipManyComment) $> R

dParser :: Parser Instruction
dParser = D <$> (char '*' *> fileNameParser <* char '\n')
  
lParser :: Parser Instruction
lParser = L <$> (char '>' *> identifierParser <* char ':')

uParser :: Parser Instruction
uParser = U <$> stringParser

commentParser :: Parser Instruction
commentParser = skipComment *> rParser

skipManyComment :: Parser [()]
skipManyComment = many (skipComment <* skipMany1EndLine)

skipComment :: Parser ()
skipComment = char commentChar *> skipAllToEndOfLine

skipMany1EndLine :: Parser String
skipMany1EndLine = many1 (char '\n')
```

problem Koniec wyrazu

```haskell
endWordParser :: Parser T.Text
endWordParser = takeTill isEndWord

isEndWord :: Char -> Bool
isEndWord c = isSpace c || (c == commentChar)

commentChar :: Char
commentChar = '#'
```

### Konsolidator

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

Assember


WhiteSpace - etykiety, podprogramy
Piet - makra, call makra
Subleq - higieniczne makr?

BrainFuck - makra wieloniowe, szablony, parsowane z zewnetrzego źródła

