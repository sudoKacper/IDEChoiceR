

#' @title Wewnętrzny motyw graficzny

#' @description Ujednolicony styl wykresów dla całego pakietu.

#' @import ggplot2

#' @keywords internal

.motyw_mcda <- function() {

  list(

    theme_light(base_size = 12),

    scale_fill_gradient(low = "#90A4AE", high = "#2E7D32"), # Od szaro-niebieskiego do zieleni

    scale_size_continuous(range = c(4, 16)),

    theme(

      plot.title = element_text(face = "bold", size = 16),

      plot.subtitle = element_text(color = "grey40", size = 11),

      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),

      legend.position = "right",

      axis.title = element_text(face = "bold")

    )

  )

}


#' Mapa Strategiczna VIKOR

#'

#' @description Wizualizacja typu cIPMA.

#' Oś X: Efektywność grupowa (odwrócone S). Oś Y: Ryzyko/Żal (R).

#' Wielkość bąbla: Siła kompromisu (zależna od Q).

#'

#' @param x Obiekt klasy `rozmyty_vikor_wynik`.

#' @param ... Dodatkowe argumenty (ignorowane).

#' @import ggplot2

#' @import ggrepel

#' @export

plot.rozmyty_vikor_wynik <- function(x, ...) {

  df <- x$wyniki


  # 1. Matematyka wykresu: Odwracamy S (żeby im więcej tym lepiej na osi X)

  s_min <- min(df$Def_S); s_max <- max(df$Def_S)

  # Normalizacja do 0-100

  df$Wydajnosc <- ((s_max - df$Def_S) / (s_max - s_min)) * 100
  # Wielkość bąbla (odwrócone Q - im mniejsze Q tym większy bąbel, bo to lżejszy kompromis)

  q_inv <- 1 - ((df$Def_Q - min(df$Def_Q)) / (max(df$Def_Q) - min(df$Def_Q)))

  df$Rozmiar <- (q_inv + 0.1)^3 # Potęgowanie dla lepszego kontrastu wizualnego


  # Środki do wyznaczenia ćwiartek

  srodek_perf <- median(df$Wydajnosc, na.rm=TRUE)

  srodek_ryzyko <- median(df$Def_R, na.rm=TRUE)


  ggplot(df, aes(x = Wydajnosc, y = Def_R)) +

    # Tło dla strefy Lidera (Prawa dolna ćwiartka: Duża wydajność, Małe ryzyko)

    annotate("rect", xmin=srodek_perf, xmax=Inf, ymin=-Inf, ymax=srodek_ryzyko, fill="#E8F5E9", alpha=0.5) +


    # Linie podziału

    geom_vline(xintercept = srodek_perf, linetype = "dashed", color = "grey50") +

    geom_hline(yintercept = srodek_ryzyko, linetype = "dashed", color = "grey50") +


    # Etykiety stref

    annotate("text", x = max(df$Wydajnosc), y = min(df$Def_R), label = "STABILNY LIDER\n(Wysoka Efekt., Niskie Ryzyko)",

             hjust=1, vjust=0, size=3, fontface="bold.italic", color="darkgreen") +

    annotate("text", x = min(df$Wydajnosc), y = max(df$Def_R), label = "UNIKAĆ\n(Niska Efekt., Wysokie Ryzyko)",

             hjust=0, vjust=1, size=3, fontface="italic", color="#B71C1C") +
    # Bąble

    geom_point(aes(size = Rozmiar, fill = Wydajnosc), shape = 21, color = "black", alpha = 0.8) +

    geom_text_repel(aes(label = paste0("Alt ", Alternatywa)), box.padding = 0.5) +


    scale_x_continuous(expand = expansion(mult = 0.2)) +


    labs(

      title = "Mapa Strategiczna VIKOR",

      subtitle = "Zielona strefa = Najlepszy kompromis.",

      x = "Indeks Wydajności Grupy (odwrócone S)",

      y = "Indeks Ryzyka / Żalu (R)",

      size = "Dominacja",

      fill = "Wynik"

    ) +

    .motyw_mcda()

}


#' Mapa Efektywności TOPSIS

#'

#' @description Pokazuje odległość od ideału. Oś X: Dystans od Najgorszego (D-).

#' Oś Y: Dystans do Najlepszego (D+).

#' Cel: Chcemy być w prawym dolnym rogu (Daleko od D-, Blisko D+).

#'

#' @param x Obiekt klasy `rozmyty_topsis_wynik`.

#' @param ... Dodatkowe argumenty.
#' #' @export

plot.rozmyty_topsis_wynik <- function(x, ...) {

  df <- x$wyniki

  df$Rozmiar <- (df$Wynik)^4 # Podbicie różnic w wielkości


  # Punkt Idealny na wykresie (Target)

  cel_x <- max(df$D_minus) * 1.02

  cel_y <- min(df$D_plus) * 0.98


  # Obliczenie wizualnej odległości euklidesowej na wykresie

  df$OdlegloscWizualna <- sqrt((df$D_minus - cel_x)^2 + (df$D_plus - cel_y)^2)


  ggplot(df, aes(x = D_minus, y = D_plus)) +

    # Linie łączące z punktem idealnym

    geom_segment(aes(xend = cel_x, yend = cel_y), linetype = "dotted", color = "grey50") +


    # Etykieta odległości na linii

    geom_label(aes(x = (D_minus + cel_x) / 2, y = (D_plus + cel_y) / 2,

                   label = sprintf("%.3f", OdlegloscWizualna)),

               size = 2.5, color = "grey30", label.size = 0, alpha = 0.7) +


    # Bąble

    geom_point(aes(size = Rozmiar, fill = Wynik), shape = 21, color = "black", alpha = 0.9) +

    geom_text_repel(aes(label = paste0("Alt ", Alternatywa)), box.padding = 0.6) +


    # Marker Idealu (Złoty romb)

    annotate("point", x = cel_x, y = cel_y, shape=18, size=6, color="#FFD700") +

    annotate("text", x = cel_x, y = cel_y, label="IDEAŁ", vjust=2, size=3.5, fontface="bold") +


    labs(

      title = "Mapa Efektywności TOPSIS",

      subtitle = "Linie przerywane pokazują drogę do rozwiązania idealnego.",

      x = "Dystans od Anty-Wzorca (D-)",

      y = "Dystans do Wzorca (D+)",

      size = "Bliskość^4",

      fill = "Wynik (CC)"

    ) +

    .motyw_mcda()

}
#' Mapa Spójności WASPAS

#'

#' @description Porównuje podejście addytywne (WSM) z multiplikatywnym (WPM).

#' Jeśli punkty leżą na przekątnej, metoda jest spójna.

#'

#' @param x Obiekt klasy `rozmyty_waspas_wynik`.

#' @param ... Dodatkowe argumenty.

#' @export

plot.rozmyty_waspas_wynik <- function(x, ...) {

  df <- x$wyniki


  # Obliczenie spójności (im mniejsza różnica między WSM a WPM tym lepiej)

  df$Odchylenie <- abs(df$WSM - df$WPM)

  df$Spojnosc <- 1 - (df$Odchylenie / max(df$Odchylenie))


  ggplot(df, aes(x = WSM, y = WPM)) +

    # Pasmo spójności

    geom_ribbon(aes(ymin = WSM - 0.05, ymax = WSM + 0.05), fill = "grey90", alpha = 0.5) +

    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +


    geom_point(aes(size = Wynik^3, fill = Spojnosc), shape = 21, color = "black", alpha = 0.8) +

    geom_text_repel(aes(label = paste0("Alt ", Alternatywa)), box.padding = 0.5) +


    labs(

      title = "Mapa Spójności WASPAS",

      subtitle = "Punkty poza szarym pasmem są 'chwiejne' (duża różnica między WSM a WPM).",

      x = "Suma Ważona (WSM)",

      y = "Iloczyn Ważony (WPM)",

      size = "Wynik Q",

      fill = "Spójność"

    ) +

    .motyw_mcda()

}

# Fix dla ostrzeżeń R CMD check o zmiennych globalnych w ggplot2

utils::globalVariables(c("Def_S", "Def_R", "D_plus", "D_minus", "Wynik", "WSM", "WPM", "Wydajnosc", "Rozmiar", "OdlegloscWizualna", "Spojnosc", "Alternatywa"))
