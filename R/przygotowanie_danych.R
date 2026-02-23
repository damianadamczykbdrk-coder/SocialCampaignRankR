#' @title Wewnętrzny parser składni MCDA
#' @description
#' Zamienia tekst typu "Kryterium =~ zm1 + zm2; Inne =~ zm3" na listę mapowania.
#' @keywords internal
.parsuj_skladnie_mcda <- function(skladnia) {
  czysta <- gsub("\n", "", skladnia)
  linie <- strsplit(czysta, ";")[[1]]

  mapowanie <- list()
  for (linia in linie) {
    if (trimws(linia) == "") next
    czesci <- strsplit(linia, "=~")[[1]]
    if (length(czesci) == 2) {
      nazwa_kryterium <- trimws(czesci[1])
      elementy <- trimws(strsplit(czesci[2], "\\+")[[1]])
      mapowanie[[nazwa_kryterium]] <- elementy
    }
  }
  mapowanie
}

#' @title Wewnętrzny skaler do skali Saaty'ego (1-9)
#' @description
#' Skaluje dowolny wektor dodatni do przedziału 1..9 (z obsługą NA).
#' @keywords internal
.skaluj_do_saaty <- function(wektor) {
  if (any(wektor < 0, na.rm = TRUE)) {
    stop("Wykryto wartości ujemne w danych wejściowych.")
  }

  # braki -> 0 (umowne "brak informacji")
  wektor[is.na(wektor)] <- 0

  maska <- wektor > 0
  wartosci <- wektor[maska]
  if (length(wartosci) == 0) return(wektor)

  min_v <- min(wartosci)
  max_v <- max(wartosci)

  if (min_v == max_v) {
    wektor[maska] <- 1
  } else {
    wektor[maska] <- 1 + (wartosci - min_v) * (8 / (max_v - min_v))
  }
  wektor
}

#' @title Wewnętrzny fuzzifier TFN
#' @description
#' Zamienia liczbę x na TFN (l,m,u) = (x-1, x, x+1) z ograniczeniem do `1..9`.
#' Zera (braki) zostają zerami.
#' @keywords internal
.rozmyj_wektor <- function(wektor) {
  l <- pmax(1, wektor - 1)
  m <- wektor
  u <- pmin(9, wektor + 1)

  jest_zerem <- (wektor == 0)
  l[jest_zerem] <- 0
  m[jest_zerem] <- 0
  u[jest_zerem] <- 0

  out <- cbind(l, m, u)
  colnames(out) <- c("l", "m", "u")
  out
}

#' Przygotowanie danych do rozmytej analizy MCDA
#'
#' @description
#' Funkcja bierze surowe dane kampanii/kanałów social media,
#' buduje wyniki kompozytowe wg składni, skaluje do skali Saaty'ego (1-9),
#' agreguje po alternatywie (np. Channel), a następnie rozmywa do TFN.
#'
#' @param dane data.frame z surowymi metrykami
#' @param skladnia string w formacie:
#'   "Reach =~ impressions + reach; Engagement =~ likes + comments"
#' @param kolumna_alternatyw nazwa kolumny identyfikującej alternatywy (np. "Channel").
#'   Jeśli NULL, każdy wiersz traktujemy jako osobną alternatywę.
#' @param funkcja_agregacji funkcja agregacji opinii/pomiarów w ramach alternatywy (domyślnie mean)
#'
#' @return Macierz m x (3n) (TFN dla każdego kryterium).
#' @export
przygotuj_dane_mcda <- function(dane, skladnia, kolumna_alternatyw = NULL, funkcja_agregacji = mean) {
  if (!is.data.frame(dane)) stop("Argument 'dane' musi być data.frame.")

  # 1) Parsowanie składni
  mapowanie <- .parsuj_skladnie_mcda(skladnia)
  nazwy_kryteriow <- names(mapowanie)
  if (length(nazwy_kryteriow) == 0) stop("Nie udało się sparsować składni. Sprawdź format 'Kryt =~ a + b; ...'.")

  # 2) Wyniki kompozytowe + skala Saaty (na poziomie wiersza)
  tmp <- data.frame(row_id = seq_len(nrow(dane)))

  for (kryt in nazwy_kryteriow) {
    zmienne <- mapowanie[[kryt]]

    brakujace <- zmienne[!zmienne %in% names(dane)]
    if (length(brakujace) > 0) {
      stop(paste0("Brakuje kolumn w danych dla kryterium '", kryt, "': ", paste(brakujace, collapse = ", ")))
    }

    # composite score
    if (length(zmienne) > 1) {
      surowy <- rowMeans(dane[, zmienne, drop = FALSE], na.rm = TRUE)
    } else {
      surowy <- dane[[zmienne]]
    }

    tmp[[kryt]] <- .skaluj_do_saaty(surowy)
  }

  # 3) Agregacja po alternatywie (np. Channel)
  if (!is.null(kolumna_alternatyw)) {
    if (!kolumna_alternatyw %in% names(dane)) stop("Nie znaleziono kolumny alternatyw w danych.")
    tmp$ID_Alternatywy <- dane[[kolumna_alternatyw]]

    zag <- aggregate(. ~ ID_Alternatywy, data = tmp[, -1, drop = FALSE], FUN = funkcja_agregacji)
    zag <- zag[order(zag$ID_Alternatywy), , drop = FALSE]

    nazwy_wierszy <- zag$ID_Alternatywy
    macierz_ostra <- as.matrix(zag[, nazwy_kryteriow, drop = FALSE])
  } else {
    nazwy_wierszy <- seq_len(nrow(tmp))
    macierz_ostra <- as.matrix(tmp[, nazwy_kryteriow, drop = FALSE])
  }

  # 4) Rozmycie do TFN (każde kryterium -> 3 kolumny)
  lista <- list()
  for (i in seq_along(nazwy_kryteriow)) {
    kryt <- nazwy_kryteriow[i]
    tfn <- .rozmyj_wektor(macierz_ostra[, i])
    colnames(tfn) <- paste0(kryt, ".", c("l", "m", "u"))

    lista[[kryt]] <- tfn
  }

  finalna <- do.call(cbind, lista)
  rownames(finalna) <- nazwy_wierszy
  attr(finalna, "nazwy_kryteriow") <- nazwy_kryteriow
  finalna
}
