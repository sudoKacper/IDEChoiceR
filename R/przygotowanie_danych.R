#' @title Wewnętrzny parser modelu decyzyjnego IDE
#' @description Funkcja pomocnicza interpretująca składnię definicji
#' obszarów decyzyjnych w pakiecie IDEChoiceR.
#' Przekształca zapis tekstowy typu:
#' "Wydajnosc =~ start + ram" na strukturę listową w R.
#' @keywords internal
.parse_ide_model <- function(model_syntax) {

  clean_syntax <- gsub("\n", "", model_syntax)
  lines <- strsplit(clean_syntax, ";")[[1]]

  mapping <- list()

  for (line in lines) {

    if (trimws(line) == "") next

    parts <- strsplit(line, "=~")[[1]]

    if (length(parts) == 2) {

      area_name <- trimws(parts[1])
      components <- trimws(strsplit(parts[2], "\\+")[[1]])

      mapping[[area_name]] <- components
    }
  }

  return(mapping)
}


#' @title Wewnętrzna normalizacja do skali 1–9
#' @description Skaluje dowolne wartości liczbowe do przedziału 1–9
#' zgodnie z wymaganiami metod MCDA (TOPSIS, VIKOR).
#' @keywords internal
.scale_to_mcda <- function(vector_values) {

  if (any(vector_values < 0, na.rm = TRUE))
    stop("Wykryto wartości ujemne w danych wejściowych.")

  vector_values[is.na(vector_values) | vector_values == 99] <- 0

  valid_mask <- vector_values > 0
  valid_values <- vector_values[valid_mask]

  if (length(valid_values) == 0) return(vector_values)

  min_v <- min(valid_values)
  max_v <- max(valid_values)

  if (min_v == max_v) {
    vector_values[valid_mask] <- 1
  } else {
    vector_values[valid_mask] <-
      1 + (valid_values - min_v) * (8 / (max_v - min_v))
  }

  return(vector_values)
}


#' @title Wewnętrzna transformacja rozmyta (TFN)
#' @description Przekształca wartość ostrą (crisp) na
#' Trójkątną Liczbę Rozmytą (TFN) w postaci (l, m, u).
#' @keywords internal
.fuzzify_vector <- function(vector_values) {

  lower <- pmax(1, vector_values - 1)
  middle <- vector_values
  upper <- pmin(9, vector_values + 1)

  zero_mask <- (vector_values == 0)

  lower[zero_mask] <- 0
  middle[zero_mask] <- 0
  upper[zero_mask] <- 0

  return(cbind(lower, middle, upper))
}


#' @title Przygotowanie macierzy decyzyjnej IDE
#'
#' @description Funkcja przekształca surowe dane dotyczące środowisk IDE
#' w ustrukturyzowaną (opcjonalnie rozmytą) macierz decyzyjną
#' gotową do analizy metodami TOPSIS lub VIKOR.
#'
#' Oblicza wskaźniki kompozytowe dla zdefiniowanych obszarów
#' (np. Wydajność, Funkcjonalność, Użyteczność, Koszt),
#' dokonuje normalizacji do skali 1–9,
#' agreguje oceny ekspertów (jeśli występują)
#' oraz przeprowadza fuzzification.
#'
#' @param data Ramka danych zawierająca surowe oceny IDE.
#' @param model_syntax Definicja obszarów decyzyjnych w formie tekstowej,
#' np. "Wydajnosc =~ start + ram; Koszt =~ licencja + subskrypcja".
#' @param alternative_column Nazwa kolumny identyfikującej IDE.
#' Jeśli NULL, każdy wiersz traktowany jest jako osobna alternatywa.
#' @param aggregation_function Funkcja agregująca opinie ekspertów
#' (domyślnie: mean).
#'
#' @return Rozmyta macierz decyzyjna (m x 3n),
#' gdzie m oznacza liczbę środowisk IDE,
#' a n liczbę obszarów decyzyjnych.
#'
#' @export
prepare_ide_data <- function(data,
                             model_syntax,
                             alternative_column = NULL,
                             aggregation_function = mean) {

  if (!is.data.frame(data))
    stop("Argument 'data' musi być ramką danych (data frame).")

  # 1. Parsowanie modelu
  mapping <- .parse_ide_model(model_syntax)
  area_names <- names(mapping)

  temp_results <- data.frame(row_id = 1:nrow(data))

  # 2. Budowa wskaźników kompozytowych
  for (area in area_names) {

    variables <- mapping[[area]]

    missing_vars <- variables[!variables %in% names(data)]
    if (length(missing_vars) > 0)
      stop(paste("Brakuje zmiennych w danych:",
                 paste(missing_vars, collapse = ", ")))

    if (length(variables) > 1) {
      raw_score <- rowMeans(data[, variables, drop = FALSE], na.rm = TRUE)
    } else {
      raw_score <- data[[variables]]
    }

    temp_results[[area]] <- .scale_to_mcda(raw_score)
  }

  # 3. Agregacja ekspertów → IDE
  if (!is.null(alternative_column)) {

    if (!alternative_column %in% names(data))
      stop("Nie znaleziono kolumny identyfikującej IDE.")

    temp_results$IDE_ID <- data[[alternative_column]]

    aggregated_data <- aggregate(. ~ IDE_ID,
                                 data = temp_results[, -1],
                                 FUN = aggregation_function)

    aggregated_data <- aggregated_data[order(aggregated_data$IDE_ID), ]

    row_names <- aggregated_data$IDE_ID
    decision_matrix <- as.matrix(aggregated_data[, area_names])

  } else {

    decision_matrix <- as.matrix(temp_results[, area_names])
    row_names <- 1:nrow(decision_matrix)
  }

  # 4. Fuzzification
  fuzzy_list <- list()

  for (i in seq_along(area_names)) {
    area <- area_names[i]
    fuzzy_list[[area]] <- .fuzzify_vector(decision_matrix[, i])
  }

  final_matrix <- do.call(cbind, fuzzy_list)

  rownames(final_matrix) <- row_names
  attr(final_matrix, "area_names") <- area_names

  return(final_matrix)
}
