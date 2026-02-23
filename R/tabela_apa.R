#' @title Generowanie Tabeli APA

#' @description

#' Funkcja przekształca wyniki analizy MCDA (TOPSIS, VIKOR, WASPAS, Meta-Ranking)

#' w sformatowaną tabelę zgodną ze standardem APA, gotową do publikacji w Wordzie.

#'

#' @param x Obiekt wynikowy z funkcji pakietu (np. `rozmyty_topsis_wynik`).

#' @param tytul Opcjonalny tytuł tabeli.

#' @return Obiekt klasy `flextable` gotowy do druku lub zapisu do Worda.

#' @importFrom rempsyc nice_table

#' @importFrom flextable autofit save_as_docx

#' @export

tabela_apa <- function(x, tytul = NULL) {

  UseMethod("tabela_apa")

}


#' @export

tabela_apa.rozmyty_topsis_wynik <- function(x, tytul = "Wyniki metody Fuzzy TOPSIS") {

  df <- x$wyniki


  # Formatowanie nazw kolumn dla czytelnika

  names(df) <- c("Alternatywa", "D+ (Do Idealu)", "D- (Od Anty)", "Wynik (CC)", "Ranking")


  # Zaokrąglenia

  df$`D+ (Do Idealu)` <- round(df$`D+ (Do Idealu)`, 3)

  df$`D- (Od Anty)` <- round(df$`D- (Od Anty)`, 3)

  df$`Wynik (CC)` <- round(df$`Wynik (CC)`, 4)


  # Tworzenie tabeli

  rempsyc::nice_table(

    df,

    title = c("Tabela 1", tytul),

    note = c("Uwaga. CC - Coefficient of Closeness. Im wyższa wartość, tym lepsza alternatywa.")

  )

}


#' @export

tabela_apa.rozmyty_vikor_wynik <- function(x, tytul = "Wyniki metody Fuzzy VIKOR") {

  df <- x$wyniki


  names(df) <- c("Alternatywa", "S (Grupa)", "R (Zal)", "Q (Kompromis)", "Ranking")


  df$`S (Grupa)` <- round(df$`S (Grupa)`, 3)

  df$`R (Zal)` <- round(df$`R (Zal)`, 3)

  df$`Q (Kompromis)` <- round(df$`Q (Kompromis)`, 4)


  rempsyc::nice_table(

    df,

    title = c("Tabela 2", tytul),

    note = c("Uwaga. S: użyteczność grupy, R: indywidualny żal, Q: indeks kompromisu (im mniej tym lepiej).")

  )

}


#' @export

tabela_apa.rozmyty_waspas_wynik <- function(x, tytul = "Wyniki metody Fuzzy WASPAS") {

  df <- x$wyniki


  names(df) <- c("Alternatywa", "WSM (Suma)", "WPM (Iloczyn)", "Q (Laczny)", "Ranking")


  df$`WSM (Suma)` <- round(df$`WSM (Suma)`, 3)

  df$`WPM (Iloczyn)` <- round(df$`WPM (Iloczyn)`, 3)

  df$`Q (Laczny)` <- round(df$`Q (Laczny)`, 4)


  rempsyc::nice_table(

    df,

    title = c("Tabela 3", tytul),

    note = c("Uwaga. WSM: Weighted Sum Model, WPM: Weighted Product Model.")

  )

}


#' @export

tabela_apa.list <- function(x, tytul = "Meta-Ranking (Konsensus)") {

  # Obsługa Meta-Rankingu
  if(is.null(x$porownanie)) stop("To nie jest obiekt meta-rankingu.")


  df <- x$porownanie


  # Usuwamy "podłogi" z nazw kolumn (np. Meta_Suma -> Meta Suma)

  names(df) <- gsub("_", " ", names(df))


  rempsyc::nice_table(

    df,

    title = c("Tabela 4", tytul),

    note = c("Zestawienie rang uzyskanych różnymi metodami oraz rankingi konsensusu.")

  )

}
