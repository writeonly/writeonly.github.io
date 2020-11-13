---
title:    'Attoparsec i EAS'
author:   TheKamilAdam
category: haskell-eta
tags:     assembler
langs:    haskell
libs:     attoparsec
tools:
projects: helcam helpa helvm
eso:      eas eta
redirect_from:
  - attoparsec-eas
  - haskell-eta/attoparsec-eas
---

W poprzednim artykule [/attoparsec-eas] zdefiniowałem cele napisania prostego asemblera z języka **[EAS]** do ezoterycznego języka programowania [ETA].
W tym artykule przyszła pora na implementację.

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

[Haskell]:              /langs/haskell
[Haskella]:             /langs/haskell

[AttoParsec]:           /libs/attoparsec

[HelCam]:               /projects/helcam
[HelPA]:                /projects/helpa
[HelVM]:                /projects/helvm

[EAS]:                  /eso/eas
[ETA]:                  /eso/eta

[asembler]:             /tags/assembler
[DSL]:                  /tags/dsl
[lekser]:               /tags/lexer
[MISC]:                 /tags/misc
[parser]:               /tags/parser
[regexp]:               /tags/regexp
