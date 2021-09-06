---
title:    'Obsługa błędów w Haskellu czyli Either, ExceptT i MonadError'
author:   TheKamilAdam
category: haskell-eta
langs:    haskell java swift
lib:      mtl relude transformers
projects: helma helpa
tags:     either exception error monad
redirect_from:
- monad-error
- haskell-eta/monad-error
---

Nie lubię wyjątków.
Jest to dotatkowy ukryty przepływ sterowania, a wszystkie przepływy sterowania powinny być jawne.




Niektórzy bronią wyjątków jak niepodległości.


Jak w Haskellu obsługiwać


Najprostrzy sposób na obsługę błędów w Haskellu to uzycie funkcji `error`.

```haskell
moveHeadRight :: (Symbol e) => FullTapeD e
moveHeadRight (cell:left , right) = pad (left , cell:right)
moveHeadRight ([] , _)            = error "End of the Tipe"

moveHeadLeft :: (Symbol e) => FullTapeD e
moveHeadLeft (left , cell:right) = pad (cell:left , right)
moveHeadLeft (_ , [])            = error "End of the Tipe"
```

Jest to jednak złe rozwiązanie.

## Rodzaje błędów w Haskellu


Najpierw zdefiniujmy czym będzie nasz typ `Error`.
Ponieważ piszemy prostą aplikacje konsolową nie potrzebujemy skomplikowanej logiki obsługi błędów.
Dlatego wys

Można by utworzyć alias typu:
```haskell
type Error = Text
```
Ale lepszy będzie alias:
```haskell
type Errors = [Text]
```
Pozwoli nam to na budowanie 
Czasem jednak potrzebujemy zwrócić kilka informacji o błędzie


## Przypadek kodu czystego, czyli Either

W czystym kodzie najlepszym rozwiązaniem do obsługi błędu jest struktura Either.

`Either` ma dwie wartości:
* Prawą  - poprawną
* Lewą - niepoprawny


Ale jest to uproszczenie.
Po prawej stronie powinna być wartość, którą chcemy dalej przetwarzać za pomocą flatMap.

Można wyobrazić sobie odwrotyny Either (po lewej stronie jest wartość poprawna, po prawej stronie jest wartość niepoprawna).
Dzięki takiej konstrukcji możemy wywoływać po kolej metody aż otrzymamy porpawną wartość
NP
```haskell
callExternalServices params = do
  _ <- callExternalService1 params
  _ <- callExternalService2 params
  _ <- callExternalService3 params
  
```

Ponieważ w naszym przypadku wartością niepoprawną będzie `Errors` możemy utworzyć pomocniczy typ:
```haskell
type Safe = Either Errors
```

I tu by można skończyć, ale piszemy w Haskellu i potrzebujemu dwóch monad.

## Przypadek kodu nieczystego, czyli Either i BusinessIO

Do tej pory:
* w z funkcji bez efektów zwracaliśmy `a`, gdzie `a` reprezentuje jakiś typ
* w kodzie nieczystym zw




W Nieczystycj języmacj programowania jedna struktura `Safe` wystarczy.
Ale w Haskellu potrzubujemy dodatkową monadę do komunikacji ze światem zewnętrnym.
Mamy dodatkową monadę lub monady do kontaktu ze światem zewnętrznym.
Taka monada ma co najmniej dwie implementacje:
* Główną, opartą na strukturze `IO`, dla kodu produkcyjnego
* Dodatkową, opartą na strukturze `State`, dla testów jednostkowych

BusinessIO

W rezultacie mamy dwie struktury (`Safe` i `BusinessIO`) i trzy kombinacje:
* funkcje które mogą zawieść, ale nie mają kontaktu ze światem zewnętrznym zwracają struktórę `Safe a`
* funkcje które nie mogą zawieść, ale mają kontaktu ze światem zewnętrznym zwracają monadę `BusinessIO a`
* funkcje które mogą zawieść i mają kontakt ze światem zewnętrznym zwracają monadę `BusinessIO (Safe a)`

Niestety takich monad nie można składać razem.
Wszystkie przypadki sprowadzić do wspólnego mianownika jakim jest `BusinessIO (Safe a)`.
W rezultacie otrzymujemy monadę w monadzie co jest prolematyczne.
Ponieważ składnia Haskella ułatwia pracowanie tylko na zewnętrznej monadzie.
Dlatego potrzebujemy równoważnej, bardziej płaskiej struktury

Tą strukturą jest ExceptT z biblioteki MTL 

```haskell
type SafeExceptT m = ExceptT Errors m
```

Praca na monadzie `Safe` jest łatwa w czystym (ang. *pure*), funkcyjnym kodzie.
Gdy jednak posiadamy już jakąś monadę do obsługi komunikacji ze światem zewnętrznym sprawa się komplikuje.
Ponieważ dwóch różnych monad nie można łączyć razem za pomocą join czy flatMap.

Dlatego musimy sprowadzić je do wpsólnej postaci.
Pomoże nam w tym monada `ExceptT`.
A dokładniej nasz aliasu typu

```haskell
type SafeExceptT m a = ExceptT Error m a
```

Potrzebujemy jeszcze trzech funkcji do utworzenia naszej monady.
jest to proste ponieaż używamu biblioteki MLT
```haskell

hoistMonad :: (Monad m) => m a -> SafeMonadT m a
hoistMonad a = ExceptT $ safe <$> a

hoistSafe :: (Monad m) => Safe a -> SafeMonadT m a
hoistSafe = hoistEither
--hoistSafe = except

hoistError :: (Monad m) => Error -> SafeMonadT m a
hoistError = hoistSafe . safeError
--hoistError = throwE
```
Zakomentowane linie to alternatywne implementacje oparte na bibliotece tranformacja
Czemu biblioteka mtl a nie transform?
ponieważ mlt jest reimportowane przez bibliotekę relude


Funkcje te podnoszą monadę biznesową, `Safe` i `Errors` do monady `SafeMonadT`

Potrzebujemy jeszcze 
```haskell


unsafeRunExceptT :: Monad m => SafeMonadT m a -> m a
unsafeRunExceptT = fmap unsafe . runExceptT
```

## MonadError
I gdy wydaje się,
że będziemy żyć z tymi dziwactwami wchodzi Monada `MonadError` cała na biało.

Monada `MonadError` robi magię dzięki której znów nie trzeba się przejmować
W rezultacie prawie cały kod do obsługi `SafeExceptT` mogłem wyrzucić

Monada `MonadError` ma dwie metody:
```haskell
class (Monad m) => MonadError e m | m -> e where
    throwError :: e -> m a
    catchError :: m a -> (e -> m a) -> m a
```

```haskell
liftExceptT :: MonadError e m => ExceptT e m a -> m a
liftExceptT m = liftEither =<< runExceptT m
```
* `runExceptT` pozbywa się `ExceptT` (Jest dekonstruktorem, czyli odwrotnością konstrutrora `ExceptT`)
* `liftEither` podnosi `Either` do monady `MonadError`


Monada `MonadError` posiada oczywiście wiele implementacji między innymi dla `Either`, `ExceptT` i `IO`:
```haskell
instance MonadError e (Either e) where
    throwError             = Left
    Left  l `catchError` h = h l
    Right r `catchError` _ = Right r

instance Monad m => MonadError e (ExceptT e m) where
    throwError = ExceptT.throwE
    catchError = ExceptT.catchE

instance MonadError IOException IO where
    throwError = ioError
    catchError = catch
```

I właśnie implementacja dla `IO` jest problematyczna.
Wynika ona z tego że `IO` może zawierać błąd, ale tylko typu `IOException` (zwanego też `IOError`).
Dlatego potrzebujemy konwersję z typu `Errors` na `IOError`

Najpierw definiujemy funkcję zamieniającą `Errors` na `Text`:
```haskell
errorsToText :: Errors -> Text
errorsToText = unlines . reverse
```

Następnie definiujemy fnkcję zamieniającą `Errors` na `String`
```haskell
errorsToString :: Errors -> String
errorsToString = toString . errorsToText
```

```haskell
exceptTToIO :: SafeExceptT IO a -> IO a
exceptTToIO = liftExceptT . withExceptT (userError . errorsToString)
```
* `userError` - tworzy IOError na podstawie parametru typu `String`
* `withExceptT` - pozwala przemapować typ błędu w `ExceptT`

```haskell
type MonadSafeError m = MonadError Errors m
```

## Czy to może być użyteczne w obiektowym języku?

Jakoś tak się złożyło że po zaimplementowaniu Safe, SafeExceptT i MonadSafeError

[HelMA]:        https://github.com/helvm/helma/tree/v0.6.9.0
