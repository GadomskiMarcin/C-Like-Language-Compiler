# Latte - Frontend - mg370790 - Marcin Gadomski

## Użycie
Programy zgodnie z opisem kompilują się przy pomocy użycia komendy ``make``  
Pojawiają się programy wkonywalne `latc` i `latc_llvm` (Te same pliki wykonywalne, lecz ze względu na wymagania techniczne znajdują się oba)

## Parser i pliki
Parser wygenerowany przy pomocy bnfc ``bnfc --haskell --functor -m Latte.cf``   
(Do wynerowania są potrzebne biblioteki Happy i Alex, ale na students nie ma bnfc )  
Przy czym bnfc zwracało kilka problemów z gramatyką (BNFC w wersji 2.9.3)

### Testy
Testy pochodza z archiwum `lattests201003.tgz`
Program odrzuca wszystkie złe testy z pliku `bad` i akceptuje oraz kompiluje (z poprawnymi odpowiedziami) pliki z `good`

## Wyniki i komentarz
### Akceptacja
Przy akceptacji frontend zwraca odpowiedź "OK"

### Odrzucenie
W przypadku odrzucenia frontend zwraca wyłącznie błąd. Syntaktyczne za pomocą parsera.  
Semantyczne przy pomocy frontendu, błąd jest w postaci miejsce + treść + idendyfikator (Z wyjątkami jak np. brak maina czy brak idendyfikatora dla 1 + "someString")  

### Język pośredni
Kompilator generuje język pośredni z instrukcjami z `Instr`, następnie po zakończeniu generacji oraz po optymalizacji instrukcje są wypisywane (Z wyjątkiem `IAss` i `IDeclare`, gdyż są to głównie instrukcje pomocnicze)

### Rejestry, wyrażenia i phi
Kompilator optymalizuje proste wyrażenia, generuje rejestry w momencie, gdy nadpisuje wartość poprzez wyrażenie bądź wywołanie funkcji lub gdy potrzebuje go do dalszego przekazania tak jak w przypadku porównania

### Runtime vs Semantic
Program znajduje podstawowe błędy runtimeowe typu dzielenie przez zero i odrzuca je

### Optymalizacje
W pełni działające `lsce` oraz `gsce` z kilkoma wyjątkami (Tj. przekazywanie wartości do phi przy zmianie)
Np.
```
%t1 = someValue
%t2 = someValue * 3
%t3 = someValue  + 2
br %cond %L1 %L2
L1:
%t11 = someValue * 3
br L3
L2:
%t22 = someValue + 2
L3
%res = phi [%t11 %L1, %t22 %L2]  

```