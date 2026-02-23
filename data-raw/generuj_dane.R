# data-raw/generuj_dane.R


# Ustawiamy ziarno losowosci, aby wyniki byly powtarzalne

set.seed(123)


# Tworzymy ramke danych symulujaca problem decyzyjny (np. wybor dostawcy)

# Zalozmy, ze mamy 20 alternatyw (wierszy)

# Mamy 4 glowne kryteria, ktore skladaja sie z podkryteriow (zmiennych)


mcda_dane_surowe <- data.frame(

  # --- Identyfikatory ---

  EkspertID = rep(1:5, each = 4), # Symulacja 5 ekspertow oceniajacych po 4 warianty

  Alternatywa = rep(1:20, length.out = 20),


  # --- Kryterium 1: Koszt (Dane ciagle, liczbowe) ---

  # Np. cena surowcow i koszt pracy

  koszt_surowce = runif(20, 500, 2000),

  koszt_praca = runif(20, 200, 800),


  # --- Kryterium 2: Jakosc (Skala Likerta 1-5) ---

  # Np. trwalosc, wykonczenie, brak defektow

  jakosc_trwalosc = sample(1:5, 20, replace = TRUE),

  # Tutaj wprowadzamy wartosc 99 jako blad/brak danych, zeby przetestowac czyszczenie

  jakosc_wykonczenie = sample(c(1:5, 99), 20, replace = TRUE, prob = c(rep(0.18, 5), 0.1)),

  jakosc_defekty = sample(1:5, 20, replace = TRUE),

  jakosc_ux = sample(1:5, 20, replace = TRUE),


  # --- Kryterium 3: Dostawa (Dane mieszane) ---

  dostawa_czas_sredni = runif(20, 2, 14), # Dni

  dostawa_niezawodnosc = runif(20, 80, 100), # Procenty

  dostawa_sledzenie = sample(0:1, 20, replace = TRUE) * 10, # 0 lub 10 pkt


  # --- Kryterium 4: Zrownowazony Rozwoj (Skala Likerta 1-7) ---

  eko_co2 = sample(1:7, 20, replace = TRUE),

  eko_odpady = sample(1:7, 20, replace = TRUE),

  # Wprowadzamy NA (braki danych) dla testow

  eko_materialy = sample(c(1:7, NA), 20, replace = TRUE),

  eko_spoleczne = sample(1:7, 20, replace = TRUE)

)


# KROK KLUCZOWY: Zapisanie danych do folderu pakietu /data

# Funkcja use_data automatycznie kompresuje dane do formatu .rda

usethis::use_data(mcda_dane_surowe, overwrite = TRUE)
