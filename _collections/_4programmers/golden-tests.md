Zainspirowany wpisem o złotych testach na [4programmers](https://4programmers.net/Mikroblogi/View/90241) postanowiłem dodać je do swojego projektu w Haskellu.

Dlaczego w ogóle złote testy (ang. **golden tests**)? Złote testy są dobre dla legacy projektów, gdzie nie wiemy, co zwrócą testowane funkcje. Ja,
pisząc od początku nowy kod, powinienem dobrze wiedzieć co i kiedy może zostać zwrócone. Jednak tak nie jest.
O ile tak jest dla prostych przypadków, o tyle dla długich fragmentów kodu w ezoterycznych asemblerach jak EAS czy WSA nie mam pojęcia co zostanie wygenerowane.
Tutaj idealnie sprawdzają się złote testy.

Niestety HUnit nie wspiera złotych testów, ale już wcześniej byłem zdecydowany na migrację do frameworka testowego HSpe].
Jednak nie sądziłem, że zmiana będzie od razu tak radykalna. Oprócz HSpec, także framework Taste pozwala na używanie złotych testów.
Jednak zdecydowałem się na HSpec ponieważ: 
* HSpec posiada automatyczne generowanie agregatora testów. 
* HSpec ma zagnieżdżoną składnię `describe`/`context`/`it`, którą można ładnie wypaczać.

Jeśli jednak ktoś wolałby złote testy we frameworku Taste znalazłem dwa teksty poświęcone temu zagadnieniu:
* [Introduction to golden testing](https://ro-che.info/articles/2017-12-04-golden-tests)
* [Golden tests are tasty](https://kseo.github.io/posts/2016-12-15-golden-tests-are-tasty.html)

A jako przykład użycia polecam projekt [Husk Schema](https://justinethier.github.io/husk-scheme/).

Reszta artykułu na [blogu writeonly](https://writeonly.pl/golden-tests) :)

