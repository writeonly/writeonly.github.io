Spytano mnie raz, co jest trudnego w Haskellu, co jednocześnie jest łatwe w OOP. Np. stworzenie interfejsu kolekcji i danie możliwości implementowania go klientom-użytkownikom. W tym celu potrzebujemy Klasę Typu od dwóch parametrów. Ale żeby mieć dobry interfejs, to nie wystarczy. 
O czym się przekonaliśmy w artykule [Abstrakcja i dopasowanie do wzorców](https://writeonly.pl/pattern-matching).

Okazuje się, że tego, czego nam brakowało to Zależności Funkcyjne (ang. *Functional Dependency*).

## Składnia Zależności Funkcyjnych

Poszukując przykładu interfejsu dla kolekcji można trafić na taki przykład Zależności Funkcyjnych:
```haskell
class Collects e ce | ce -> e where
  empty  :: ce
  insert :: e -> ce -> ce
  member :: e -> ce -> Bool
  toList :: ce -> [e]
```
Czyli jedyna nowość to `| ce -> e `. Zapis ten pozwala na uzależnienie jednego typu od drugiego.

Implementacja wygląda jak implementacja Klasy Typu dla dwóch parametrów:
```haskell
instance Eq e => Collects e [e] where
  empty           = []
  insert e l      = (e:l)
  member e []     = False
  member e (x:xs) 
    | e == x      = True
    | otherwise   = member e xs
  toList l        = l
```

Reszta artykułu na [blogu](https://writeonly.pl/functional-dependency), ale mały spojler. Uważam, że Zależności Funkcyjne nie są pojęciem trudnym, ale jest to kolejna nowa konstrukcja, której trzeba się nauczyć. Dodatkowym problemem jest to że poza twórcą biblioteki ClassyPrelude nikt nie czuje potrzeby posiadania abstrakcyjnego interfejsu dla wszystkich kolekcji