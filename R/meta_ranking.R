#' @keywords internal
.oblicz_ranking_dominacji <- function(r1, r2, r3) {
  n <- length(r1)
  finalny <- rep(0, n)
  mac <- cbind(r1, r2, r3)
  dostepne <- rep(TRUE, n)

  for (poz in 1:n) {
    tmp <- mac
    tmp[!dostepne, ] <- Inf

    k1 <- which.min(tmp[, 1])
    k2 <- which.min(tmp[, 2])
    k3 <- which.min(tmp[, 3])

    kand <- c(k1, k2, k3)
    tab <- table(kand)
    zwyc <- as.numeric(names(tab)[which.max(tab)])

    # remis 3 różne -> wygrywa najmniejsza suma rang
    if (length(tab) == 3) {
      sumy <- rowSums(mac[kand, , drop = FALSE])
      zwyc <- kand[which.min(sumy)]
    }

    finalny[zwyc] <- poz
    dostepne[zwyc] <- FALSE
  }

  finalny
}

#' Rozmyty meta-ranking
#'
#' @param macierz_decyzyjna Rozmyta macierz TFN (m x 3n)
#' @param typy_kryteriow Wektor "min"/"max" (długość n)
#' @param wagi (opcjonalnie) wagi ręczne (długość n lub 3n)
#' @param bwm_kryteria (opcjonalnie) nazwy kryteriów
#' @param bwm_najlepsze (opcjonalnie) Best-to-Others
#' @param bwm_najgorsze (opcjonalnie) Others-to-Worst
#' @param lambda WASPAS lambda
#' @param v VIKOR v
#' @return lista: porownanie (data.frame) i korelacje (macierz Spearmana)
#' @export
rozmyty_meta_ranking <- function(macierz_decyzyjna,
                                 typy_kryteriow,
                                 wagi = NULL,
                                 bwm_kryteria = NULL,
                                 bwm_najlepsze = NULL,
                                 bwm_najgorsze = NULL,
                                 lambda = 0.5,
                                 v = 0.5) {

  # wspólne argumenty – te same wagi / BWM / fallback entropia
  args_base <- list(
    macierz_decyzyjna = macierz_decyzyjna,
    typy_kryteriow = typy_kryteriow,
    wagi = wagi,
    bwm_kryteria = bwm_kryteria,
    bwm_najlepsze = bwm_najlepsze,
    bwm_najgorsze = bwm_najgorsze
  )

  # Uruchom metody
  res_topsis <- do.call(rozmyty_topsis, args_base)
  res_vikor  <- do.call(rozmyty_vikor,  c(args_base, list(v = v)))
  res_waspas <- do.call(rozmyty_waspas, c(args_base, list(lambda = lambda)))

  r_t <- res_topsis$wyniki$Ranking
  r_v <- res_vikor$wyniki$Ranking
  r_w <- res_waspas$wyniki$Ranking

  # A) suma rang
  suma <- r_t + r_v + r_w
  meta_suma <- rank(suma, ties.method = "first")

  # B) dominacja
  meta_dom <- .oblicz_ranking_dominacji(r_t, r_v, r_w)

  # C) RankAggreg (Spearman)
  mac_ra <- rbind(order(r_t), order(r_v), order(r_w))
  n_alt <- nrow(macierz_decyzyjna)

  if (n_alt <= 10) {
    ra <- RankAggreg::BruteAggreg(mac_ra, n_alt, distance = "Spearman")
  } else {
    ra <- RankAggreg::RankAggreg(mac_ra, n_alt, method = "GA", distance = "Spearman", verbose = FALSE)
  }

  top_list <- ra$top.list
  meta_ra <- numeric(n_alt)
  for (i in 1:n_alt) meta_ra[as.numeric(top_list[i])] <- i

  por <- data.frame(
    Alternatywa = rownames(macierz_decyzyjna),
    R_TOPSIS = r_t,
    R_VIKOR = r_v,
    R_WASPAS = r_w,
    Meta_Suma = meta_suma,
    Meta_Dominacja = meta_dom,
    Meta_Agregacja = meta_ra
  )

  list(
    porownanie = por,
    korelacje = stats::cor(por[, -1], method = "spearman")
  )
}
