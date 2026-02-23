#' Obliczanie wag metodą Entropii Shannona

#'

#' @description Wyznacza obiektywne wagi kryteriów na podstawie danych,

#' mierząc stopień rozproszenia wartości. Im większa zmienność, tym wyższa waga.

#'

#' @param macierz_decyzyjna Rozmyta macierz (wynik funkcji `przygotuj_dane_mcda`).

#' @return Wektor numeryczny wag sumujący się do 1.

#' @export

oblicz_wagi_entropii <- function(macierz_decyzyjna) {


  # Od-rozmycie macierzy do obliczen entropii (srednia z l, m, u)

  n_kolumn <- ncol(macierz_decyzyjna)

  macierz_ostra <- matrix(0, nrow = nrow(macierz_decyzyjna), ncol = n_kolumn/3)


  k <- 1

  for(j in seq(1, n_kolumn, 3)) {

    # Proste odrozmycie: (l + 4m + u) / 6 lub zwykła średnia arytmetyczna

    macierz_ostra[, k] <- (macierz_decyzyjna[, j] + 4*macierz_decyzyjna[, j+1] + macierz_decyzyjna[, j+2]) / 6

    k <- k + 1

  }


  # Normalizacja (P_ij)

  sumy_kolumn <- colSums(macierz_ostra)

  sumy_kolumn[sumy_kolumn == 0] <- 1 # Unikamy dzielenia przez zero

  P <- sweep(macierz_ostra, 2, sumy_kolumn, "/")


  # Obliczanie Entropii (E_j)

  k_const <- 1 / log(nrow(macierz_decyzyjna))

  E <- numeric(ncol(P))


  for(j in 1:ncol(P)) {

    p_vals <- P[, j]

    p_vals <- p_vals[p_vals > 0] # Ignorujemy zera dla logarytmu

    if(length(p_vals) == 0) {

      E[j] <- 1

    } else {

      E[j] <- -k_const * sum(p_vals * log(p_vals))

    }

  }


  # Obliczanie wag (d_j i w_j)

  d <- 1 - E

  if(sum(d) == 0) return(rep(1/length(d), length(d))) # Zabezpieczenie

  w <- d / sum(d)


  return(w)

}


#' @title Wewnętrzny procesor wag

#' @description Decyduje, skąd wziąć wagi (Ręczne vs BWM).

#' @keywords internal

.pobierz_finalne_wagi <- function(macierz, wagi, bwm_kryteria, bwm_najlepsze, bwm_najgorsze) {


  n_kryteriow <- ncol(macierz) / 3


  # Opcja 1: Wagi podane ręcznie (np. z Entropii lub eksperckie)

  if (!missing(wagi) && !is.null(wagi)) {

    if (length(wagi) == n_kryteriow) {

      # Rozszerzamy wagi ostre na rozmyte (w, w, w)

      return(rep(wagi, each = 3))

    }

    if (length(wagi) != ncol(macierz)) {

      stop("Długość wektora 'wagi' musi odpowiadać liczbie kolumn macierzy (3 * n_kryteriow) lub liczbie kryteriów.")

    }

    return(wagi)

  }


  # Opcja 2: Obliczenie BWM

  if (!missing(bwm_najlepsze) && !missing(bwm_najgorsze)) {


    # Pobieramy nazwy kryteriow

    if (missing(bwm_kryteria)) {

      if (!is.null(attr(macierz, "nazwy_kryteriow"))) {

        bwm_kryteria <- attr(macierz, "nazwy_kryteriow")

      } else {

        bwm_kryteria <- paste0("C", 1:n_kryteriow)

        message("Nie znaleziono nazw kryteriów. Używam domyślnych: ", paste(bwm_kryteria, collapse=", "))

      }

    }


    message("Obliczanie wag metodą BWM...")

    wynik_bwm <- oblicz_wagi_bwm(bwm_kryteria, bwm_najlepsze, bwm_najgorsze)

    wagi_ostre <- wynik_bwm$wagi_kryteriow


    if (length(wagi_ostre) != n_kryteriow) {

      stop("Liczba wag z BWM nie zgadza się z liczbą kryteriów w macierzy.")

    }


    # Konwersja na wagi rozmyte (w, w, w)

    wagi_rozmyte <- rep(wagi_ostre, each = 3)

    return(wagi_rozmyte)

  }


  stop("Musisz podać wektor 'wagi' LUB parametry 'bwm_najlepsze' i 'bwm_najgorsze'.")

}



#' Rozmyta Metoda TOPSIS

#'

#' @description Implementacja Fuzzy TOPSIS. Oblicza odległość od rozwiązania idealnego

#' i anty-idealnego.

#'

#' @param macierz_decyzyjna Macierz ($m \times 3n$).

#' @param typy_kryteriow Wektor znakowy ("max" dla zysku, "min" dla kosztu).

#' @param wagi (Opcjonalnie) Wektor wag.

#' @param bwm_kryteria (Opcjonalnie) Nazwy kryteriów dla BWM.

#' @param bwm_najlepsze (Opcjonalnie) Wektor Best-to-Others.

#' @param bwm_najgorsze (Opcjonalnie) Wektor Others-to-Worst.

#' @return Obiekt klasy `rozmyty_topsis_wynik` z rankingiem.

#' @export

rozmyty_topsis <- function(macierz_decyzyjna, typy_kryteriow, wagi = NULL,

                           bwm_kryteria, bwm_najlepsze, bwm_najgorsze) {


  if (!is.matrix(macierz_decyzyjna)) stop("'macierz_decyzyjna' musi być macierzą.")


  # 1. Ustalenie wag

  finalne_wagi <- .pobierz_finalne_wagi(macierz_decyzyjna, wagi, bwm_kryteria, bwm_najlepsze, bwm_najgorsze)


  # 2. Rozszerzenie typów kryteriów (max/min) na kolumny rozmyte

  n_kolumn <- ncol(macierz_decyzyjna)

  typy_rozmyte <- character(n_kolumn)

  k <- 1

  for (j in seq(1, n_kolumn, 3)) {

    typy_rozmyte[j:(j+2)] <- typy_kryteriow[k]

    k <- k + 1

  }


  # 3. Normalizacja wektorowa

  macierz_norm <- matrix(nrow = nrow(macierz_decyzyjna), ncol = n_kolumn)

  mianowniki <- sqrt(apply(macierz_decyzyjna^2, 2, sum))


  for (i in seq(1, n_kolumn, 3)) {

    macierz_norm[, i] <- macierz_decyzyjna[, i] / mianowniki[i + 2]

    macierz_norm[, i+1] <- macierz_decyzyjna[, i+1] / mianowniki[i + 1]

    macierz_norm[, i+2] <- macierz_decyzyjna[, i+2] / mianowniki[i]

  }


  # 4. Ważenie

  W_diag <- diag(finalne_wagi)

  macierz_wazona <- macierz_norm %*% W_diag


  # 5. Rozwiązania Idealne (FNIS - Fuzzy Positive Ideal Solution, FNIS - N

  idea_poz <- ifelse(typy_rozmyte == "max", apply(macierz_wazona, 2, max), apply(macierz_wazona, 2, min))

  idea_neg <- ifelse(typy_rozmyte == "min", apply(macierz_wazona, 2, max), apply(macierz_wazona, 2, min))


  # 6. Odległości (metoda wierzchołkowa)

  temp_d_poz <- (macierz_wazona - matrix(idea_poz, nrow=nrow(macierz_decyzyjna), ncol=n_kolumn, byrow=TRUE))^2

  temp_d_neg <- (macierz_wazona - matrix(idea_neg, nrow=nrow(macierz_decyzyjna), ncol=n_kolumn, byrow=TRUE))^2


  d_poz_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)

  d_neg_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)


  # Sumujemy kwadraty różnic dla trójek (l, m, u)

  d_poz_rozmyte[,1] <- sqrt(apply(temp_d_poz[, seq(1, n_kolumn, 3), drop=FALSE], 1, sum))

  d_poz_rozmyte[,2] <- sqrt(apply(temp_d_poz[, seq(2, n_kolumn, 3), drop=FALSE], 1, sum))

  d_poz_rozmyte[,3] <- sqrt(apply(temp_d_poz[, seq(3, n_kolumn, 3), drop=FALSE], 1, sum))


  d_neg_rozmyte[,1] <- sqrt(apply(temp_d_neg[, seq(1, n_kolumn, 3), drop=FALSE], 1, sum))

  d_neg_rozmyte[,2] <- sqrt(apply(temp_d_neg[, seq(2, n_kolumn, 3), drop=FALSE], 1, sum))

  d_neg_rozmyte[,3] <- sqrt(apply(temp_d_neg[, seq(3, n_kolumn, 3), drop=FALSE], 1, sum))


  # 7. Współczynnik bliskości (Closeness Coefficient)

  mianownik <- d_neg_rozmyte + d_poz_rozmyte

  CC_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)


  # Dzielenie liczb rozmytych (przyblizone)

  CC_rozmyte[,1] <- d_neg_rozmyte[,1] / mianownik[,3]

  CC_rozmyte[,2] <- d_neg_rozmyte[,2] / mianownik[,2]

  CC_rozmyte[,3] <- d_neg_rozmyte[,3] / mianownik[,1]


  # Defuzzyfikacja wyniku (Metoda Graded Mean Integration)

  wynik_def <- (CC_rozmyte[,1] + 4*CC_rozmyte[,2] + CC_rozmyte[,3]) / 6


  # Dane do wykresu (D+ i D- jako skalary)

  skalar_D_poz <- rowMeans(d_poz_rozmyte)

  skalar_D_neg <- rowMeans(d_neg_rozmyte)


  ramka_wynikow <- data.frame(

    Alternatywa = 1:nrow(macierz_decyzyjna),

    D_plus = skalar_D_poz,

    D_minus = skalar_D_neg,

    Wynik = wynik_def,

    Ranking = rank(-wynik_def, ties.method = "first")

  )


  wynik <- list(

    wyniki = ramka_wynikow,

    metoda = "TOPSIS"

  )

  class(wynik) <- "rozmyty_topsis_wynik"

  return(wynik)

}

#' Rozmyta Metoda VIKOR

#'

#' @description Metoda kompromisowa VIKOR. Oblicza wskaźniki S (użyteczność grupy),

#' R (indywidualny żal) oraz Q (indeks kompromisu).

#'

#' @inheritParams rozmyty_topsis

#' @param v Waga strategii "większości kryteriów" (domyślnie 0.5).

#' @return Obiekt klasy `rozmyty_vikor_wynik`.

#' @export

rozmyty_vikor <- function(macierz_decyzyjna, typy_kryteriow, v = 0.5, wagi = NULL,

                          bwm_kryteria, bwm_najlepsze, bwm_najgorsze) {


  finalne_wagi <- .pobierz_finalne_wagi(macierz_decyzyjna, wagi, bwm_kryteria, bwm_najlepsze, bwm_najgorsze)

  n_kolumn <- ncol(macierz_decyzyjna)


  # Rozszerzenie typów

  typy_rozmyte <- character(n_kolumn)

  k <- 1

  for (j in seq(1, n_kolumn, 3)) {

    typy_rozmyte[j:(j+2)] <- typy_kryteriow[k]

    k <- k + 1

  }


  # 1. Rozwiązania Idealne

  idea_poz <- ifelse(typy_rozmyte == "max", apply(macierz_decyzyjna, 2, max), apply(macierz_decyzyjna, 2, min))

  idea_neg <- ifelse(typy_rozmyte == "min", apply(macierz_decyzyjna, 2, max), apply(macierz_decyzyjna, 2, min))


  # 2. Normalizacja liniowa (specyficzna dla VIKOR) i ważenie

  macierz_d <- matrix(0, nrow = nrow(macierz_decyzyjna), ncol = n_kolumn)


  for (i in seq(1, n_kolumn, 3)) {

    if (typy_rozmyte[i] == "max") {

      mianownik <- idea_poz[i+2] - idea_neg[i]

      if(mianownik == 0) mianownik <- 1e-9

      # Wzór dla Benefit: (f* - f_ij) / (f* - f-)

      macierz_d[, i] <- (idea_poz[i] - macierz_decyzyjna[, i+2]) / mianownik

      macierz_d[, i+1] <- (idea_poz[i+1] - macierz_decyzyjna[, i+1]) / mianownik

      macierz_d[, i+2] <- (idea_poz[i+2] - macierz_decyzyjna[, i]) / mianownik

    } else {

      mianownik <- idea_neg[i+2] - idea_poz[i]

      if(mianownik == 0) mianownik <- 1e-9

      # Wzór dla Cost: (f_ij - f*) / (f- - f*)

      macierz_d[, i] <- (macierz_decyzyjna[, i] - idea_poz[i+2]) / mianownik

      macierz_d[, i+1] <- (macierz_decyzyjna[, i+1] - idea_poz[i+1]) / mianownik

      macierz_d[, i+2] <- (macierz_decyzyjna[, i+2] - idea_poz[i]) / mianownik

    }

  }


  # Mnożenie przez wagi

  W_diag <- diag(finalne_wagi)

  macierz_wazona_d <- macierz_d %*% W_diag


  # 3. Wartości S (suma) i R (max)

  S_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)

  R_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)


  S_rozmyte[,1] <- apply(macierz_wazona_d[, seq(1, n_kolumn, 3), drop=FALSE], 1, sum)

  S_rozmyte[,2] <- apply(macierz_wazona_d[, seq(2, n_kolumn, 3), drop=FALSE], 1, sum)

  S_rozmyte[,3] <- apply(macierz_wazona_d[, seq(3, n_kolumn, 3), drop=FALSE], 1, sum)


  R_rozmyte[,1] <- apply(macierz_wazona_d[, seq(1, n_kolumn, 3), drop=FALSE], 1, max)

  R_rozmyte[,2] <- apply(macierz_wazona_d[, seq(2, n_kolumn, 3), drop=FALSE], 1, max)

  R_rozmyte[,3] <- apply(macierz_wazona_d[, seq(3, n_kolumn, 3), drop=FALSE], 1, max)


  # 4. Indeks Q

  s_star <- min(S_rozmyte[,1])

  s_minus <- max(S_rozmyte[,3])

  r_star <- min(R_rozmyte[,1])

  r_minus <- max(R_rozmyte[,3])


  mianownik_s <- s_minus - s_star

  mianownik_r <- r_minus - r_star

  if (mianownik_s == 0) mianownik_s <- 1

  if (mianownik_r == 0) mianownik_r <- 1


  Q_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)

  czlon1 <- (S_rozmyte - s_star) / mianownik_s

  czlon2 <- (R_rozmyte - r_star) / mianownik_r

  Q_rozmyte <- v * czlon1 + (1 - v) * czlon2


  # Defuzzyfikacja

  def_S <- (S_rozmyte[,1] + 2*S_rozmyte[,2] + S_rozmyte[,3]) / 4

  def_R <- (R_rozmyte[,1] + 2*R_rozmyte[,2] + R_rozmyte[,3]) / 4

  def_Q <- (Q_rozmyte[,1] + 2*Q_rozmyte[,2] + Q_rozmyte[,3]) / 4


  ramka_wynikow <- data.frame(

    Alternatywa = 1:nrow(macierz_decyzyjna),

    Def_S = def_S,

    Def_R = def_R,

    Def_Q = def_Q,

    Ranking = rank(def_Q, ties.method = "first")

  )


  wynik <- list(

    wyniki = ramka_wynikow,

    detale = list(S_rozmyte = S_rozmyte, R_rozmyte = R_rozmyte, Q_rozmyte = Q_rozmyte),

    parametry = list(v = v)

  )


  class(wynik) <- "rozmyty_vikor_wynik"

  return(wynik)
}

  #' Rozmyta Metoda WASPAS

  #'

  #' @description Weighted Aggregated Sum Product Assessment. Łączy podejście addytywne (WSM)

  #' i multiplikatywne (WPM).

  #'

  #' @inheritParams rozmyty_topsis

  #' @param lambda Parametr wagi WSM vs WPM (domyślnie 0.5).

  #' @export

  rozmyty_waspas <- function(macierz_decyzyjna, typy_kryteriow, lambda = 0.5, wagi = NULL,

                             bwm_kryteria, bwm_najlepsze, bwm_najgorsze) {


    finalne_wagi <- .pobierz_finalne_wagi(macierz_decyzyjna, wagi, bwm_kryteria, bwm_najlepsze, bwm_najgorsze)

    n_kolumn <- ncol(macierz_decyzyjna)


    # Rozszerzanie typow

    typy_rozmyte <- character(n_kolumn)

    k <- 1

    for (j in seq(1, n_kolumn, 3)) {

      typy_rozmyte[j:(j+2)] <- typy_kryteriow[k]

      k <- k + 1

    }


    # 1. Normalizacja

    norm_baza <- ifelse(typy_rozmyte == "max", apply(macierz_decyzyjna, 2, max), apply(macierz_decyzyjna, 2, min))

    N_macierz <- matrix(0, nrow(macierz_decyzyjna), n_kolumn)


    for (j in seq(1, n_kolumn, 3)) {

      if (typy_rozmyte[j] == "max") {

        # Max: x_ij / max_x

        N_macierz[, j] <- macierz_decyzyjna[, j] / norm_baza[j+2]

        N_macierz[, j+1] <- macierz_decyzyjna[, j+1] / norm_baza[j+2]

        N_macierz[, j+2] <- macierz_decyzyjna[, j+2] / norm_baza[j+2]

      } else {

        # Min: min_x / x_ij

        N_macierz[, j] <- norm_baza[j] / macierz_decyzyjna[, j+2]

        N_macierz[, j+1] <- norm_baza[j] / macierz_decyzyjna[, j+1]

        N_macierz[, j+2] <- norm_baza[j] / macierz_decyzyjna[, j]

      }

    }


    # 2. WSM (Suma ważona)

    W_diag <- diag(finalne_wagi)

    nw_suma <- N_macierz %*% W_diag


    WSM_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)

    WSM_rozmyte[,1] <- apply(nw_suma[, seq(1, n_kolumn, 3), drop=FALSE], 1, sum)

    WSM_rozmyte[,2] <- apply(nw_suma[, seq(2, n_kolumn, 3), drop=FALSE], 1, sum)

    WSM_rozmyte[,3] <- apply(nw_suma[, seq(3, n_kolumn, 3), drop=FALSE], 1, sum)


    # 3. WPM (Iloczyn ważony) -> Potęgowanie

    nw_iloczyn <- matrix(0, nrow(macierz_decyzyjna), n_kolumn)

    for (j in seq(1, n_kolumn, 3)) {

      # Podnoszenie liczby rozmytej do potęgi wagi (uproszczone dla dodatnich)

      nw_iloczyn[, j] <- N_macierz[, j] ^ finalne_wagi[j+2]

      nw_iloczyn[, j+1] <- N_macierz[, j+1] ^ finalne_wagi[j+1]

      nw_iloczyn[, j+2] <- N_macierz[, j+2] ^ finalne_wagi[j]

    }


    WPM_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)

    WPM_rozmyte[,1] <- apply(nw_iloczyn[, seq(1, n_kolumn, 3), drop=FALSE], 1, prod)

    WPM_rozmyte[,2] <- apply(nw_iloczyn[, seq(2, n_kolumn, 3), drop=FALSE], 1, prod)

    WPM_rozmyte[,3] <- apply(nw_iloczyn[, seq(3, n_kolumn, 3), drop=FALSE], 1, prod)


    # 4. Łączny wynik Q

    def_wsm <- rowSums(WSM_rozmyte) / 3

    def_wpm <- rowSums(WPM_rozmyte) / 3

    Q_wartosc <- lambda * def_wsm + (1 - lambda) * def_wpm


    ramka_wynikow <- data.frame(

      Alternatywa = 1:nrow(macierz_decyzyjna),

      WSM = def_wsm,

      WPM = def_wpm,

      Wynik = Q_wartosc,

      Ranking = rank(-Q_wartosc, ties.method = "first")

    )


    wynik <- list(

      wyniki = ramka_wynikow,

      metoda = "WASPAS",

      lambda = lambda

    )

    class(wynik) <- "rozmyty_waspas_wynik"

    return(wynik)

  }
