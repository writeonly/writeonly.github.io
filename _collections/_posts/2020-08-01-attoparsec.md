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
* Parsec/MegaParsec
* AttoParsec


### Happy & Alex

Happy & Alex haskellowe wersje klasycznej pary Yacc i Lex (lub ich darmowych odpowiedników Bison i Flex).
Jedyne w tym zestawieniu pozwalają na używanie gramatyk LR.
Wadą pary Happy & Alex jest to,
że trzeba napisać gramatykę.
czyli nauczyć się nowego języka niewiele ładniejszego niż wyrażenia regularne.

### Parsec/MegaParsec

Parsec jest to klasyczna biblioteka do pisania parserów.
Pozwala Combinatory (ang. [Parser combinator](https://en.wikipedia.org/wiki/Parser_combinator))

Parsec niestety nie jest już rozwijany.
Zamiast tego jest rozwijana jego ulepszona wersja, czyli MegaParsec.


### AttoParsec
AttoParsec to biblioteka oryginalnie pomyślana do parsowania logów na bieżąco.
Jest szybsza niż MegaParsec czy Parsec oraz jako jedyna umożliwia parsowanie przyrastającego pliku linia po linii.
Niestety jest o wiele uboższa w składni przez co jest uważana za dobrą do parsowania plików o prostrzej strukturze.
Ubogość sładni przekłada się także na prostrzą strukturę samej biblioteki.
Dzięki czemu o wiele prościej się jej nauczyć.

## Wyzwanie - EAS i ETA

Skoro już wybrałem AttoParsec to przydałyby się jakieś logi do sparsowania.
Logów jednak nie mam w zwiazku z tym będę parsować kod źródłowy napisany w EAS.

**EAS** (ETA Assembler) - to para assembler dla exoterycznego jezyka programowania **ETA**.

Zarówno **EAS** jak i **ETA** to języki stosowe (ang. stack-based)
Wiele języków ezoterycznych jak **[False]**, **[Funge]**, **[Piet]**, **[WhiteSpace]** to języki stosowe.


Minimalistyczny asembler stosowy bez makr.

Głównie rozwiązywanie liczb i etykiet.

MISC (ang. Minimal Instruction Set Computer)

Minimalizm osiąga się poprzez wykonywanie operacji na stosie.
maszyna stosowa (https://pl.wikipedia.org/wiki/Maszyna_stosowa)


maszyny
https://en.wikipedia.org/wiki/Transputer


Maszyny wirtualne
JVM 
webassembly

Języki stosowe stack-based 
dc forth mouse postcript rpl joy

z czego na szczególną uwagę zasługują
* forth - jako najpopularniejszy język stosowy
* joy - jako funkcyjny język stosowy
* mouse - jako prawdziwie używany język stosowy który wygląda jak ezoteryczny język
* rpl - nazywany Reversed Polish Lisp

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

