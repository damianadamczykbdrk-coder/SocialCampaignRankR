# ============================================================
# 0) WALIDACJE + POMOCNICZE
# ============================================================

#' @keywords internal
.sprawdz_macierz_tfn <- function(macierz) {
  if (!is.matrix(macierz)) stop("macierz_decyzyjna musi być macierzą (matrix).")
  if (ncol(macierz) %% 3 != 0) stop("Liczba kolumn musi być wielokrotnością 3 (TFN: l,m,u).")
  if (nrow(macierz) < 2) stop("Macierz musi mieć co najmniej 2 alternatywy.")
  invisible(TRUE)
}

#' @keywords internal
.defuzzyfikuj_tfn <- function(macierz_tfn) {
  # (l + 4m + u)/6 dla każdej trójki
  n_kol <- ncol(macierz_tfn)
  n_kryt <- n_kol / 3
  M <- matrix(0, nrow = nrow(macierz_tfn), ncol = n_kryt)
  k <- 1
  for (j in seq(1, n_kol, 3)) {
    M[, k] <- (macierz_tfn[, j] + 4 * macierz_tfn[, j + 1] + macierz_tfn[, j + 2]) / 6
    k <- k + 1
  }
  colnames(M) <- attr(macierz_tfn, "nazwy_kryteriow")
  M
}

#' @keywords internal
.rozszerz_typy <- function(typy_kryteriow, n_kolumn) {
  # typy_kryteriow: długość n (kryteria)
  n_kryt <- n_kolumn / 3
  if (length(typy_kryteriow) != n_kryt) {
    stop("typy_kryteriow musi mieć długość równą liczbie kryteriów (ncol(macierz)/3).")
  }
  typy_rozmyte <- character(n_kolumn)
  k <- 1
  for (j in seq(1, n_kolumn, 3)) {
    typy_rozmyte[j:(j + 2)] <- typy_kryteriow[k]
    k <- k + 1
  }
  typy_rozmyte
}

# ============================================================
# 1) ENTROPIA SHANNONA (WAGI OBIEKTYWNE)
# ============================================================

#' Obliczanie wag metodą Entropii Shannona
#'
#' @description Wyznacza obiektywne wagi kryteriów na podstawie danych (po defuzzyfikacji TFN).
#' Im większa zmienność kryterium, tym większa waga.
#'
#' @param macierz_decyzyjna Rozmyta macierz TFN (m x 3n) z `przygotuj_dane_mcda()`.
#' @return Wektor wag (długości n), sumuje się do 1.
#' @export
oblicz_wagi_entropii <- function(macierz_decyzyjna) {
  .sprawdz_macierz_tfn(macierz_decyzyjna)

  M <- .defuzzyfikuj_tfn(macierz_decyzyjna)

  # Normalizacja P_ij
  col_sum <- colSums(M)
  col_sum[col_sum == 0] <- 1
  P <- sweep(M, 2, col_sum, "/")

  # Entropia
  k_const <- 1 / log(nrow(P))
  E <- numeric(ncol(P))

  for (j in 1:ncol(P)) {
    p <- P[, j]
    p <- p[p > 0]
    if (length(p) == 0) {
      E[j] <- 1
    } else {
      E[j] <- -k_const * sum(p * log(p))
    }
  }

  d <- 1 - E
  if (sum(d) == 0) return(rep(1 / length(d), length(d)))
  d / sum(d)
}

# ============================================================
# 2) WYBÓR WAG: RĘCZNE -> BWM -> ENTROPIA
# ============================================================

#' @keywords internal
.pobierz_finalne_wagi <- function(macierz_decyzyjna,
                                  wagi = NULL,
                                  bwm_kryteria = NULL,
                                  bwm_najlepsze = NULL,
                                  bwm_najgorsze = NULL) {

  .sprawdz_macierz_tfn(macierz_decyzyjna)
  n_kryt <- ncol(macierz_decyzyjna) / 3

  # A) ręczne
  if (!is.null(wagi)) {
    if (length(wagi) == n_kryt) return(rep(wagi, each = 3))
    if (length(wagi) == ncol(macierz_decyzyjna)) return(wagi)
    stop("wagi muszą mieć długość n (liczba kryteriów) albo 3n (kolumn TFN).")
  }

  # B) BWM
  if (!is.null(bwm_najlepsze) && !is.null(bwm_najgorsze)) {
    if (is.null(bwm_kryteria)) {
      bwm_kryteria <- attr(macierz_decyzyjna, "nazwy_kryteriow")
      if (is.null(bwm_kryteria)) bwm_kryteria <- paste0("C", 1:n_kryt)
    }

    # WAŻNE: Twoja sygnatura to (kryteria, b_to_o, o_to_w)
    wynik_bwm <- oblicz_wagi_bwm(bwm_kryteria, bwm_najlepsze, bwm_najgorsze)

    if (is.null(wynik_bwm$wagi_kryteriow) || length(wynik_bwm$wagi_kryteriow) != n_kryt) {
      stop("BWM zwrócił błędny wynik: brak 'wagi_kryteriow' lub zła liczba wag.")
    }

    return(rep(wynik_bwm$wagi_kryteriow, each = 3))
  }

  # C) Entropia (fallback)
  w <- oblicz_wagi_entropii(macierz_decyzyjna)
  rep(w, each = 3)
}

# ============================================================
# 3) FUZZY TOPSIS
# ============================================================

#' Rozmyta metoda TOPSIS
#'
#' @description
#' Implementacja Fuzzy TOPSIS: normalizacja, ważenie, rozwiązanie idealne i anty-idealne,
#' odległości i współczynnik bliskości (CC), ranking malejąco po CC.
#'
#' @param macierz_decyzyjna Rozmyta macierz TFN (m x 3n).
#' @param typy_kryteriow Wektor "min"/"max" dla każdego kryterium (długość n).
#' @param wagi (opcjonalnie) Wagi ręczne (długość n lub 3n).
#' @param bwm_kryteria (opcjonalnie) Nazwy kryteriów dla BWM.
#' @param bwm_najlepsze (opcjonalnie) Wektor Best-to-Others (1-9).
#' @param bwm_najgorsze (opcjonalnie) Wektor Others-to-Worst (1-9).
#' @return Obiekt klasy `rozmyty_topsis_wynik`.
#' @export
rozmyty_topsis <- function(macierz_decyzyjna,
                           typy_kryteriow,
                           wagi = NULL,
                           bwm_kryteria = NULL,
                           bwm_najlepsze = NULL,
                           bwm_najgorsze = NULL) {

  .sprawdz_macierz_tfn(macierz_decyzyjna)
  n_kol <- ncol(macierz_decyzyjna)

  # 1) Wagi (ręczne -> BWM -> Entropia)
  finalne_wagi <- .pobierz_finalne_wagi(macierz_decyzyjna, wagi, bwm_kryteria, bwm_najlepsze, bwm_najgorsze)

  # 2) Typy kryteriów na kolumny TFN
  typy_rozmyte <- .rozszerz_typy(typy_kryteriow, n_kol)

  # 3) Normalizacja wektorowa (z korektą kolejności TFN)
  macierz_norm <- matrix(0, nrow = nrow(macierz_decyzyjna), ncol = n_kol)
  mianowniki <- sqrt(apply(macierz_decyzyjna^2, 2, sum))
  mianowniki[mianowniki == 0] <- 1

  for (i in seq(1, n_kol, 3)) {
    macierz_norm[, i]   <- macierz_decyzyjna[, i]   / mianowniki[i + 2]
    macierz_norm[, i+1] <- macierz_decyzyjna[, i+1] / mianowniki[i + 1]
    macierz_norm[, i+2] <- macierz_decyzyjna[, i+2] / mianowniki[i]
  }

  # 4) Ważenie
  macierz_wazona <- macierz_norm %*% diag(finalne_wagi)

  # 5) Ideał i anty-ideał
  idea_poz <- ifelse(typy_rozmyte == "max",
                     apply(macierz_wazona, 2, max),
                     apply(macierz_wazona, 2, min))

  idea_neg <- ifelse(typy_rozmyte == "min",
                     apply(macierz_wazona, 2, max),
                     apply(macierz_wazona, 2, min))

  # 6) Odległości (trójkami)
  temp_d_poz <- (macierz_wazona - matrix(idea_poz, nrow=nrow(macierz_decyzyjna), ncol=n_kol, byrow=TRUE))^2
  temp_d_neg <- (macierz_wazona - matrix(idea_neg, nrow=nrow(macierz_decyzyjna), ncol=n_kol, byrow=TRUE))^2

  d_poz <- cbind(
    sqrt(apply(temp_d_poz[, seq(1, n_kol, 3), drop=FALSE], 1, sum)),
    sqrt(apply(temp_d_poz[, seq(2, n_kol, 3), drop=FALSE], 1, sum)),
    sqrt(apply(temp_d_poz[, seq(3, n_kol, 3), drop=FALSE], 1, sum))
  )

  d_neg <- cbind(
    sqrt(apply(temp_d_neg[, seq(1, n_kol, 3), drop=FALSE], 1, sum)),
    sqrt(apply(temp_d_neg[, seq(2, n_kol, 3), drop=FALSE], 1, sum)),
    sqrt(apply(temp_d_neg[, seq(3, n_kol, 3), drop=FALSE], 1, sum))
  )

  # 7) CC rozmyte -> defuzzyfikacja
  denom <- d_neg + d_poz
  denom[denom == 0] <- 1e-12

  CC <- cbind(
    d_neg[,1] / denom[,3],
    d_neg[,2] / denom[,2],
    d_neg[,3] / denom[,1]
  )

  wynik_def <- (CC[,1] + 4*CC[,2] + CC[,3]) / 6

  ramka <- data.frame(
    Alternatywa = rownames(macierz_decyzyjna) %||% as.character(1:nrow(macierz_decyzyjna)),
    D_plus  = rowMeans(d_poz),
    D_minus = rowMeans(d_neg),
    Wynik = wynik_def,
    Ranking = rank(-wynik_def, ties.method = "first")
  )

  out <- list(wyniki = ramka, metoda = "TOPSIS")
  class(out) <- "rozmyty_topsis_wynik"
  out
}

# pomocnik dla rownames w ramce
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ============================================================
# 4) FUZZY VIKOR
# ============================================================

#' Rozmyta metoda VIKOR
#'
#' @description
#' Metoda kompromisowa VIKOR. Zwraca S (suma), R (maks.), Q (kompromis).
#' Ranking rośnie po Q (im mniejsze Q tym lepiej).
#'
#' @inheritParams rozmyty_topsis
#' @param v Parametr strategii większości (domyślnie 0.5).
#' @return Obiekt klasy `rozmyty_vikor_wynik`.
#' @export
rozmyty_vikor <- function(macierz_decyzyjna,
                          typy_kryteriow,
                          v = 0.5,
                          wagi = NULL,
                          bwm_kryteria = NULL,
                          bwm_najlepsze = NULL,
                          bwm_najgorsze = NULL) {

  .sprawdz_macierz_tfn(macierz_decyzyjna)
  n_kol <- ncol(macierz_decyzyjna)

  finalne_wagi <- .pobierz_finalne_wagi(macierz_decyzyjna, wagi, bwm_kryteria, bwm_najlepsze, bwm_najgorsze)
  typy_rozmyte <- .rozszerz_typy(typy_kryteriow, n_kol)

  # 1) Ideały na surowej macierzy (bez ważenia)
  f_star  <- ifelse(typy_rozmyte == "max", apply(macierz_decyzyjna, 2, max), apply(macierz_decyzyjna, 2, min))
  f_minus <- ifelse(typy_rozmyte == "min", apply(macierz_decyzyjna, 2, max), apply(macierz_decyzyjna, 2, min))

  # 2) Normalizacja liniowa VIKOR + ważenie
  D <- matrix(0, nrow=nrow(macierz_decyzyjna), ncol=n_kol)

  for (i in seq(1, n_kol, 3)) {
    if (typy_rozmyte[i] == "max") {
      denom <- f_star[i+2] - f_minus[i]
      if (denom == 0) denom <- 1e-12
      D[, i]   <- (f_star[i]   - macierz_decyzyjna[, i+2]) / denom
      D[, i+1] <- (f_star[i+1] - macierz_decyzyjna[, i+1]) / denom
      D[, i+2] <- (f_star[i+2] - macierz_decyzyjna[, i])   / denom
    } else {
      denom <- f_minus[i+2] - f_star[i]
      if (denom == 0) denom <- 1e-12
      D[, i]   <- (macierz_decyzyjna[, i]   - f_star[i+2]) / denom
      D[, i+1] <- (macierz_decyzyjna[, i+1] - f_star[i+1]) / denom
      D[, i+2] <- (macierz_decyzyjna[, i+2] - f_star[i])   / denom
    }
  }

  D_w <- D %*% diag(finalne_wagi)

  # 3) S i R rozmyte
  S <- cbind(
    apply(D_w[, seq(1, n_kol, 3), drop=FALSE], 1, sum),
    apply(D_w[, seq(2, n_kol, 3), drop=FALSE], 1, sum),
    apply(D_w[, seq(3, n_kol, 3), drop=FALSE], 1, sum)
  )

  R <- cbind(
    apply(D_w[, seq(1, n_kol, 3), drop=FALSE], 1, max),
    apply(D_w[, seq(2, n_kol, 3), drop=FALSE], 1, max),
    apply(D_w[, seq(3, n_kol, 3), drop=FALSE], 1, max)
  )

  s_star <- min(S[,1]); s_minus <- max(S[,3])
  r_star <- min(R[,1]); r_minus <- max(R[,3])
  denom_s <- s_minus - s_star; if (denom_s == 0) denom_s <- 1e-12
  denom_r <- r_minus - r_star; if (denom_r == 0) denom_r <- 1e-12

  # 4) Q rozmyte
  Q <- v * ((S - s_star) / denom_s) + (1 - v) * ((R - r_star) / denom_r)

  # defuzzyfikacja (prosty kompromis)
  def_S <- (S[,1] + 2*S[,2] + S[,3]) / 4
  def_R <- (R[,1] + 2*R[,2] + R[,3]) / 4
  def_Q <- (Q[,1] + 2*Q[,2] + Q[,3]) / 4

  ramka <- data.frame(
    Alternatywa = rownames(macierz_decyzyjna) %||% as.character(1:nrow(macierz_decyzyjna)),
    Def_S = def_S,
    Def_R = def_R,
    Def_Q = def_Q,
    Ranking = rank(def_Q, ties.method = "first")
  )

  out <- list(wyniki = ramka, detale = list(S=S, R=R, Q=Q), parametry = list(v=v))
  class(out) <- "rozmyty_vikor_wynik"
  out
}

# ============================================================
# 5) FUZZY WASPAS
# ============================================================

#' Rozmyta metoda WASPAS
#'
#' @description
#' Łączy WSM (suma ważona) i WPM (iloczyn ważony).
#' Ranking malejąco po wyniku Q.
#'
#' @inheritParams rozmyty_topsis
#' @param lambda Udział WSM vs WPM (domyślnie 0.5).
#' @return Obiekt klasy `rozmyty_waspas_wynik`.
#' @export
rozmyty_waspas <- function(macierz_decyzyjna,
                           typy_kryteriow,
                           lambda = 0.5,
                           wagi = NULL,
                           bwm_kryteria = NULL,
                           bwm_najlepsze = NULL,
                           bwm_najgorsze = NULL) {

  .sprawdz_macierz_tfn(macierz_decyzyjna)
  n_kol <- ncol(macierz_decyzyjna)

  finalne_wagi <- .pobierz_finalne_wagi(macierz_decyzyjna, wagi, bwm_kryteria, bwm_najlepsze, bwm_najgorsze)
  typy_rozmyte <- .rozszerz_typy(typy_kryteriow, n_kol)

  # 1) Normalizacja
  baza <- ifelse(typy_rozmyte == "max",
                 apply(macierz_decyzyjna, 2, max),
                 apply(macierz_decyzyjna, 2, min))

  baza[baza == 0] <- 1e-12

  N <- matrix(0, nrow=nrow(macierz_decyzyjna), ncol=n_kol)

  for (j in seq(1, n_kol, 3)) {
    if (typy_rozmyte[j] == "max") {
      N[, j]   <- macierz_decyzyjna[, j]   / baza[j+2]
      N[, j+1] <- macierz_decyzyjna[, j+1] / baza[j+2]
      N[, j+2] <- macierz_decyzyjna[, j+2] / baza[j+2]
    } else {
      # dla kosztów: min/x (z zachowaniem kolejności TFN)
      N[, j]   <- baza[j] / pmax(macierz_decyzyjna[, j+2], 1e-12)
      N[, j+1] <- baza[j] / pmax(macierz_decyzyjna[, j+1], 1e-12)
      N[, j+2] <- baza[j] / pmax(macierz_decyzyjna[, j],   1e-12)
    }
  }

  # 2) WSM
  Nw <- N %*% diag(finalne_wagi)
  WSM <- cbind(
    apply(Nw[, seq(1, n_kol, 3), drop=FALSE], 1, sum),
    apply(Nw[, seq(2, n_kol, 3), drop=FALSE], 1, sum),
    apply(Nw[, seq(3, n_kol, 3), drop=FALSE], 1, sum)
  )

  # 3) WPM (potęgowanie)
  Pm <- matrix(1, nrow=nrow(N), ncol=n_kol)
  for (j in seq(1, n_kol, 3)) {
    Pm[, j]   <- (pmax(N[, j],   1e-12)) ^ finalne_wagi[j+2]
    Pm[, j+1] <- (pmax(N[, j+1], 1e-12)) ^ finalne_wagi[j+1]
    Pm[, j+2] <- (pmax(N[, j+2], 1e-12)) ^ finalne_wagi[j]
  }
  WPM <- cbind(
    apply(Pm[, seq(1, n_kol, 3), drop=FALSE], 1, prod),
    apply(Pm[, seq(2, n_kol, 3), drop=FALSE], 1, prod),
    apply(Pm[, seq(3, n_kol, 3), drop=FALSE], 1, prod)
  )

  # 4) Wynik łączny (defuzzy)
  def_wsm <- rowMeans(WSM)
  def_wpm <- rowMeans(WPM)
  Q <- lambda * def_wsm + (1 - lambda) * def_wpm

  ramka <- data.frame(
    Alternatywa = rownames(macierz_decyzyjna) %||% as.character(1:nrow(macierz_decyzyjna)),
    WSM = def_wsm,
    WPM = def_wpm,
    Wynik = Q,
    Ranking = rank(-Q, ties.method = "first")
  )

  out <- list(wyniki = ramka, metoda = "WASPAS", lambda = lambda)
  class(out) <- "rozmyty_waspas_wynik"
  out
}
