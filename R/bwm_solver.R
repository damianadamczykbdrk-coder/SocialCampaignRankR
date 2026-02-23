#' @title Wewnętrzna walidacja wejścia BWM
#' @keywords internal
.sprawdz_bwm <- function(kryteria, b_to_o, o_to_w) {
  if (is.null(kryteria) || length(kryteria) < 2) {
    stop("BWM: 'kryteria' musi zawierać co najmniej 2 elementy.")
  }
  if (length(b_to_o) != length(kryteria) || length(o_to_w) != length(kryteria)) {
    stop("BWM: długości 'b_to_o' i 'o_to_w' muszą być równe liczbie kryteriów.")
  }
  if (any(is.na(b_to_o)) || any(is.na(o_to_w))) stop("BWM: wykryto NA w porównaniach.")
  if (any(b_to_o <= 0) || any(o_to_w <= 0)) stop("BWM: wartości muszą być dodatnie.")
  invisible(TRUE)
}

#' Obliczanie wag kryteriów metodą BWM (Best–Worst Method)
#'
#' @description
#' Implementacja BWM jako zadania programowania liniowego.
#' Użytkownik podaje wektor preferencji "Best-to-Others" i "Others-to-Worst".
#'
#' @param kryteria wektor nazw kryteriów (np. c("Reach","Engagement","Cost","Conversion"))
#' @param b_to_o wektor długości n: porównania najlepszego kryterium do pozostałych (skala 1..9)
#' @param o_to_w wektor długości n: porównania pozostałych do najgorszego (skala 1..9)
#'
#' @return lista: wagi_kryteriow, epsilon, wskaznik_spojnosci, kryteria
#' @export
oblicz_wagi_bwm <- function(kryteria, b_to_o, o_to_w) {
  .sprawdz_bwm(kryteria, b_to_o, o_to_w)

  if (!requireNamespace("Rglpk", quietly = TRUE)) {
    stop("Pakiet 'Rglpk' nie jest zainstalowany. Zainstaluj go: install.packages('Rglpk').")
  }

  n <- length(kryteria)

  # Zmienne decyzyjne: w1..wn oraz epsilon (ostatnia)
  # Minimalizujemy epsilon
  obj <- c(rep(0, n), 1)

  # Ograniczenia:
  # dla każdego j: w_best - a_Bj * w_j <= epsilon
  #               -w_best + a_Bj * w_j <= epsilon
  #
  # dla każdego j: w_j - a_jW * w_worst <= epsilon
  #               -w_j + a_jW * w_worst <= epsilon
  #
  # + suma wag = 1, w_i >= 0

  # indeks best i worst (tam gdzie porównanie = 1)
  idx_best <- which(b_to_o == 1)
  idx_worst <- which(o_to_w == 1)

  if (length(idx_best) != 1) stop("BWM: w 'b_to_o' dokładnie jedno kryterium musi mieć wartość 1 (Best).")
  if (length(idx_worst) != 1) stop("BWM: w 'o_to_w' dokładnie jedno kryterium musi mieć wartość 1 (Worst).")

  # Budowa macierzy ograniczeń
  A <- list()
  dir <- c()
  rhs <- c()

  # Best-to-Others
  for (j in seq_len(n)) {
    # w_best - a_Bj * w_j <= epsilon
    row1 <- rep(0, n + 1)
    row1[idx_best] <- 1
    row1[j] <- row1[j] - b_to_o[j]
    row1[n + 1] <- -1
    A[[length(A) + 1]] <- row1
    dir <- c(dir, "<=")
    rhs <- c(rhs, 0)

    # -w_best + a_Bj * w_j <= epsilon
    row2 <- rep(0, n + 1)
    row2[idx_best] <- -1
    row2[j] <- row2[j] + b_to_o[j]
    row2[n + 1] <- -1
    A[[length(A) + 1]] <- row2
    dir <- c(dir, "<=")
    rhs <- c(rhs, 0)
  }

  # Others-to-Worst
  for (j in seq_len(n)) {
    # w_j - a_jW * w_worst <= epsilon
    row1 <- rep(0, n + 1)
    row1[j] <- 1
    row1[idx_worst] <- row1[idx_worst] - o_to_w[j]
    row1[n + 1] <- -1
    A[[length(A) + 1]] <- row1
    dir <- c(dir, "<=")
    rhs <- c(rhs, 0)

    # -w_j + a_jW * w_worst <= epsilon
    row2 <- rep(0, n + 1)
    row2[j] <- -1
    row2[idx_worst] <- row2[idx_worst] + o_to_w[j]
    row2[n + 1] <- -1
    A[[length(A) + 1]] <- row2
    dir <- c(dir, "<=")
    rhs <- c(rhs, 0)
  }

  # suma wag = 1
  row_sum <- rep(0, n + 1)
  row_sum[1:n] <- 1
  A[[length(A) + 1]] <- row_sum
  dir <- c(dir, "==")
  rhs <- c(rhs, 1)

  A_mat <- do.call(rbind, A)

  # ograniczenia dolne/górne
  bounds <- list(
    lower = list(ind = 1:(n + 1), val = c(rep(0, n), 0)),
    upper = list(ind = 1:(n + 1), val = c(rep(1, n), Inf))
  )

  sol <- Rglpk::Rglpk_solve_LP(
    obj = obj,
    mat = A_mat,
    dir = dir,
    rhs = rhs,
    bounds = bounds,
    max = FALSE
  )

  if (sol$status != 0) {
    stop("BWM: solver LP nie znalazł rozwiązania (status != 0). Sprawdź dane wejściowe.")
  }

  wagi <- sol$solution[1:n]
  eps <- sol$solution[n + 1]

  # normalizacja (na wypadek minimalnych błędów numerycznych)
  wagi[wagi < 0] <- 0
  if (sum(wagi) > 0) wagi <- wagi / sum(wagi)

  # Prosty wskaźnik spójności: mniejsze epsilon => większa spójność
  # (To nie jest jedyny możliwy CI, ale jest praktyczny i czytelny na zaliczenie.)
  wsk_spoj <- 1 / (1 + eps)

  out <- list(
    kryteria = kryteria,
    wagi_kryteriow = setNames(as.numeric(wagi), kryteria),
    epsilon = as.numeric(eps),
    wskaznik_spojnosci = as.numeric(wsk_spoj)
  )
  class(out) <- "bwm_wagi"
  out
}
