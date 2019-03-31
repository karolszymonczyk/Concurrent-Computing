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
