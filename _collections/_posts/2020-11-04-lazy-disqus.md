---
title:    'Leniwe ładowanie komentarzy z Disqus'
author:   TheKamilAdam
category: jekyll
tags:     api css html lazy script
langs:    javascript 
tools:    disqus
libs:     jquery minimal-mistake
redirect_from:
  - lazy-disqus
  - jekyll/lazy-disqus
render_with_liquid: true  
---

Ponieważ korzystam z bloga opartego na generatorze statycznych stron **[Jekyll]** 
i nie mam żadnego backendu,
nie mam możliwości samodzielnego zaimplementowania komentarzy.
Jednak **[Minimal Mistakes]**,
używany przeze mnie szablon,
[proponuje kilka rozwiązań tego problemu](https://mmistakes.github.io/minimal-mistakes/docs/configuration/#comments).
Są to zewnętrzne systemy komentarzy wystawiające publiczne [API] jak:
* **[Disqus]**
* Discourse
* komentarze Facebooka 
* komentarze utterances  
* Staticman


Ja wybrałem **[Disqus]**.
Głównie dlatego,
że jest pierwszy na liście i prawdopodobnie jest najpopularniejszy.
Jednak Disqus posiada jeden poważny problem.
Ładuje ogromną ilość **[JavaScriptu]** oraz **[CSSa]**,
przez co artykuł posiadający komentarze posiada bardzo zły wynik po analizowaniu w [PageSpeed Insights](https://developers.google.com/speed/pagespeed/insights).

Jest na to jednak rozwiązanie.
Jest nim leniwe ładowanie komentarzy z **[Disqus]**.

## Leniwe ładowanie komentarzy Disqus

Są co najmniej dwa sposoby, żeby leniwie ładować komentarze:
* `on click`  - po kliknięciu przycisku o nazwie np. `pokarz komentarze`.
* `on scroll` - po przewinięciu na dół strony, gdzie zwykle są komentarze.

### Leniwe ładowanie komentarzy Disqus po kliknięciu przycisku

Po pierwsze musimy zmodyfikować html.
Do pliku `comments.html`, 
pod częścią odpowiedzialny za klasyczny `disqus`, dodajemy `disqus_loader`
{% raw %}
```html
    {% when "disqus" %}
      <h4 class="page__comments-title">{{ comments_label }}</h4>
      <section id="disqus_thread">
      </section>
    {% when "disqus_loader" %}
      <h4 class="page__comments-title">{{ comments_label }}</h4>
      <section id="disqus_thread">
        <button id="disqus_loader">{{ comments_title }}</button>
      </section>
```
{% endraw %}
Nasza wersja różni się tym,
że mamy przycisk.
Oczywiście przycisk należałoby jeszcze ostylować,
żeby dobrze wyglądał.

Teraz potrzebujemy skryptu **[JavaScript]**, który załaduje nam **[Disqus]** na żądanie.
Oczywiście skrypt jest oparty o nieśmiertelne **[jQuery]**:
{% raw %}
```javascript
$(function() {
  const $disqus_loader = $('#disqus_loader');

  $disqus_loader.click(function() {
    const $disqus_thread = $('#disqus_thread');

    if($disqus_thread) {
      $.ajaxSetup({ cache:true });
      $.getScript('//{{site.comments.disqus.shortname}}.disqus.com/embed.js');
      $.ajaxSetup({ cache:false });
      console.log('Loaded Disqus.');
    }
  });
});
```
{% endraw %}

Teraz jeszcze należałoby ten skrypt **[JS]** dołączyć do każdej strony.
Ja wszystkie swoje skrypty łącze w jeden plik `app.js` za pomocą dyrektywy `include` w Jekyllu.
Tutaj jednak potrzebujemy dołączania warunkowego za pomocą instrukcji `case`:
{% raw %}
```javascript
{% case site.comments.provider %}
  {% when "disqus_loader" %}
    {%- include js/disqus-loader.js -%}
{% endcase %}
```
{% endraw %}

### Leniwe ładowanie Disqus po przewinięciu strony na dół

Po pierwsze musimy zmodyfikować html.
Do pliku `comments.html`, pod częścią odpowiedzialny za klasyczny `disqus` dodajemy `disqus_empty`

{% raw %}
```html
    {% when "disqus" %}
      <h4 class="page__comments-title">{{ comments_label }}</h4>
      <section id="disqus_thread">
      </section>
    {% when "disqus_empty" %}
      <h4 class="page__comments-title">{{ comments_label }}</h4>
      <section id="disqus_thread">
        <div id="disqus_empty"></div>
      </section>

```
{% endraw %}
Nasza wersja różni się tym, że mamy pusty tag `div`.
Dzięki niemu będziemy wiedzieć,
czy już załadowaliśmy komentarze,
czy jeszcze nie.

Teraz potrzebujemy skryptu **[JavaScript]**, który załaduje nam [Disqus] na żądanie.
Oczywiście ten skrypt też jest oparty o nieśmiertelne [jQuery]:
{% raw %}
```javascript
$(document).scroll(function(e) {
  console.log("Scrolled.");
  const $disqus_empty = $('#disqus_empty');

  if ($disqus_empty.length) {
    const $window = $(window);
    const $disqus_thread = $('#disqus_thread');

    if ($disqus_thread[0].getBoundingClientRect().top - 150 < $window.scrollTop()) {
      $.ajaxSetup({ cache:true });
      $.getScript('//{{site.comments.disqus.shortname}}.disqus.com/embed.js');
      $.ajaxSetup({ cache:false });
      console.log('Loaded Disqus.');
    }
  }
});
```
{% endraw %}

Teraz jeszcze należałoby ten skrypt JS dołączyć do każdej strony.
Znów robimy to za pomocą dyrektywy `include` w Jekyllu zawartej w instrukcji `case`:
{% raw %}
```javascript
{% case site.comments.provider %}
  {% when "disqus_empty" %}
    {%- include js/disqus-empty.js -%}
{% endcase %}
```
{% endraw %}

## Podsumowanie

Który ze sposób jest lepszy?
Dla mnie `scroll` ponieważ nie chciało mi się stylować przycisku ładowania komentarzy :D

Czy warto się tak męczyć dla [Disqus]?
Jeszcze nie wiem.
Mam zamiar w najbliższym czasie przetestować też innych dostawców silników do komentarzy.

[JavaScript]:       /langs/javascript
[JavaScriptu]:      /langs/javascript
[JS]:               /langs/javascript

[jQuery]:           /libs/jquery
[minimal mistakes]: /libs/minimal-mistakes

[disqus]:           /tools/disqus

[api]:              /tags/api
[css]:              /tags/css
[cssa]:             /tags/css
[html]:             /tags/html
[lazy]:             /tags/lazy
[script]:           /tags/script
