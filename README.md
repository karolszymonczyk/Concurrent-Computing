# Programowanie-Wspolbiezne
Zadania na laboratoria.

## Lista 1
Należy utworzyć projekt programu współbieżnego, stanowiący symulator działania przedsiębiorstwa.
Prezes firmy, w losowych odstępach czasu,  wymyśla kolejne zadania do wykonania dla pracowników i umieszcza je na liście zadań.
Przyjmujemy, że zadanie ma postać rekordu o następujących polach:
* pierwszy argument,
* drugi argument,
* operator arytmetyczny: dodawanie, odejmowanie albo mnożenie.

Zadanie polega na “wytworzeniu” wyniku operacji arytmetycznej.
Każdy pracownik, co pewien czas, pobiera kolejne zadanie z listy zadań i je wykonuje. W wyniku powstaje pewien produkt, który pracownik umieszcza w magazynie.
Do magazynu, co pewien czas, przychodzi klient i zabiera (kupuje) jakieś produkty.

Osoby powinny być symulowane przez osobne wątki.

Ze względu na konieczność synchronizacji dostępu, można zaimplementować też wątki (serwery) do obsługi poszczególnych struktur danych (listy zadań, magazynu).

Symulator ma działać w dwóch trybach:
* w trybie “gadatliwym” albo
* w trybie “spokojnym”.

W trybie “gadatliwym” wypisywane są na bieżąco komunikaty o zdarzeniach które zachodzą w symulowanym przedsiębiorstwie -- taki rodzaj “tekstowej animacji” przedstawiającej jego działanie. Najlepiej gdyby każdy wątek symulujący prezesa, pracownika albo klienta sam drukował komunikat w chwili wykonania istotnego działania.

W trybie “spokojnym”, działający symulator oczekuje na polecenia użytkownika. Mogą to być polecenia żądające wyświetlenia pewnych informacji na temat bieżącego stanu firmy (na przykład wyświetlenie stanu magazynu, albo wyświetlenie listy zadań do wykonania). W tym trybie, oprócz wątków  symulujących firmę, powinien też być wątek służący do interakcji z użytkownikiem, który w pętli wyświetla menu dostępnych poleceń i czeka na wybór użytkownika wczytany z terminala.  

## Lista 2
Należy uaktualnić swoje symulatory działania przedsiębiorstwa w związku z  jego modernizacją i reorganizacją pracy.

* W firmie zakupiono pewną ilość maszyn dodających i maszyn mnożących.
* Wszystkie zadania  muszą być wykonywane na maszynach.
* Zadania umieszczane na liście zadań są w postaci częściowo wypełnionych rekordów o następujących polach:
  * argument_1
  * operator (dodawania albo mnożenia)
  * argument_2
  * pole_na_wynik
* Pracownik pobiera cały rekord z listy zadań, a następnie wykonuje zadanie na maszynie odpowiedniego typu dla danego operatora.
* Instrukcja obsługi maszyny:
  * Pracownik umieszcza rekord z zadaniem do wykonania w maszynie,
  * Maszyna przez pewien czas wykonuje zadaną operację i umieszcza wynik w polu pole_na_wynik.
  * Pracownik może wtedy odebrać rekord z wynikiem i przenieść go do magazynu.
* Każdy pracownik przy swoich narodzinach podejmuje losową decyzję czy jest “cierpliwy”, czy “niecierpliwy”. Cierpliwi pracownicy czekają w kolejce aż maszyna przyjmie ich zadanie do obsługi. Niecierpliwi pracownicy krążą ze swoim zadaniem między maszynami,  czekając jedynie przez krótki czas przy każdej z nich, aż uda im się uzyskać dostęp do jednej z nich.
* Każdy pracownik prowadzi statystyki: ile udało mu się wykonać zadań.
* W trybie “spokojnym” możliwe jest też drukowanie informacji o pracownikach (czy jest “cierpliwy”  czy “niecierpliwy” i ile zadań udało mu się wykonać do tej pory.)
