---
title:    'Monada Free - wybierz wwolność'
author:   TheKamilAdam
category: haskell-eta
langs:    haskell
libs:     rio zio
projects: helma
eso:      eta whitespace
tags:     library
redirect_from:
- pattern-matching
- haskell-eta/pattern-matching
---

## Wolna monada

Spiewak wideo

Wolność



## Zastosowanie

tranformacja naturalna

### Stackoverflow

MOżemy mieć wiele transformatoróœ
Jak mamy dużo monad i dużo transformatoró monad to może raknąć nam stosu.
Bo stos na JVM jest za mały
Ale dzięki monadzie Free możemy przenieść obliczenia na startę


Krytykwa free - video



### Polimorfizm zależności



video - free vs final Tagless 


Final tagles jest bardziej naturalne dla Haskella



## Alternatywne Rozwiązania


ZIO/RIO rozwiązują problem
* Zagnieżdzonych transformatorów
* Polimorfizmu

Można nie zauważyć podobieństwa pobiędzy tymi dwoma bibliotekami ponieważ
ZIO reklamuje się przedewszystkim jako biblioteka do czystego programowania (coś co jest dostępne w Haskellu out of the box)
RIO reklamuje się jako lepsze prelude


RIO -
Final Tagless must die - 

https://www.youtube.com/watch?v=gu0ZCqQe3BY


### Optymalizacje

Jednak jest coś czego ZIO/RIO nie potrafi.


Możemy dowolnie modyfikować drzewo.
Nie możemy go jednak wyświetlić i zapisać



Deklaratywność

Alternatywa

Parsec MegaParsec AttoParsec

Slick

(Mac to Unix, Android to Linux)

Wygenerować cały program jako free aplicative z Free Alternative


```haskell
data FreeIOF a
 = GetChar (Char -> a)
 | PutChar Char a
 | GetLine (String -> a)
 | PutStr String a
 | PutStrLn String a
 | Flush a
 | GetInt (Int -> a)
 | PutInt Int a
 | LogStr String a
 | LogStrLn String a
 deriving (Functor)

type FreeIO = Free FreeIOF 
```

```haskell
fGetChar :: FreeIO Char
fGetChar = liftF $ GetChar id

fPutChar :: Char -> FreeIO ()
fPutChar c = liftF $ PutChar c ()

fGetLine :: FreeIO String
fGetLine  = liftF $ GetLine id

fPutStr :: String -> FreeIO ()
fPutStr  s = liftF $ PutStr s ()

fPutStrLn :: String -> FreeIO ()
fPutStrLn s = liftF $ PutStrLn s ()

fFlush :: FreeIO ()
fFlush = liftF $ Flush ()

fGetInt :: FreeIO Int
fGetInt = liftF $ GetInt id

fPutInt :: Int -> FreeIO ()
fPutInt i = liftF $ PutInt i ()

fLogStr :: String -> FreeIO ()
fLogStr s = liftF $ LogStr s ()

fLogStrLn :: String -> FreeIO ()
fLogStrLn s = liftF $ LogStrLn s ()
```


```haskell
instance WrapperIO FreeIO where
  wGetChar  = fGetChar
  wPutChar  = fPutChar
  wGetLine  = fGetLine
  wPutStr   = fPutStr
  wPutStrLn = fPutStrLn
  wFlush    = fFlush
  wGetInt   = fGetInt
  wPutInt   = fPutInt
  wLogStr   = fLogStr
  wLogStrLn = fLogStrLn
```