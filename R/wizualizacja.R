#' @keywords internal
.motyw_mcda <- function() {
  ggplot2::theme_light(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 16),
      plot.subtitle = ggplot2::element_text(color = "grey40", size = 11),
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.4),
      legend.position = "right",
      axis.title = ggplot2::element_text(face = "bold")
    )
}

#' @title Mapa TOPSIS
#' @param x Obiekt klasy `rozmyty_topsis_wynik`
#' @param ... Ignorowane
#' @export
plot.rozmyty_topsis_wynik <- function(x, ...) {
  df <- x$wyniki
  df$Rozmiar <- (df$Wynik - min(df$Wynik, na.rm = TRUE) + 1e-9)^2

  ggplot2::ggplot(df, ggplot2::aes(x = D_minus, y = D_plus)) +
    ggplot2::geom_point(ggplot2::aes(size = Rozmiar, fill = Wynik),
                        shape = 21, color = "black", alpha = 0.85) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(df$Alternatywa)),
                       vjust = -1, size = 3) +
    ggplot2::labs(
      title = "Mapa Efektywności TOPSIS",
      subtitle = "Cel: daleko od anty-wzorca (D-) i blisko wzorca (D+).",
      x = "Dystans od anty-wzorca (D-)",
      y = "Dystans do wzorca (D+)",
      size = "Siła",
      fill = "Wynik (CC)"
    ) +
    .motyw_mcda()
}

#' @title Mapa VIKOR
#' @param x Obiekt klasy `rozmyty_vikor_wynik`
#' @param ... Ignorowane
#' @export
plot.rozmyty_vikor_wynik <- function(x, ...) {
  df <- x$wyniki
  df$Rozmiar <- (max(df$Def_Q, na.rm = TRUE) - df$Def_Q + 1e-9)^2

  ggplot2::ggplot(df, ggplot2::aes(x = Def_S, y = Def_R)) +
    ggplot2::geom_point(ggplot2::aes(size = Rozmiar, fill = Def_Q),
                        shape = 21, color = "black", alpha = 0.85) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(df$Alternatywa)),
                       vjust = -1, size = 3) +
    ggplot2::labs(
      title = "Mapa Strategiczna VIKOR",
      subtitle = "Mniejsze Q = lepszy kompromis.",
      x = "S (użyteczność grupy)",
      y = "R (żal)",
      size = "Siła",
      fill = "Q"
    ) +
    .motyw_mcda()
}

#' @title Mapa WASPAS
#' @param x Obiekt klasy `rozmyty_waspas_wynik`
#' @param ... Ignorowane
#' @export
plot.rozmyty_waspas_wynik <- function(x, ...) {
  df <- x$wyniki
  df$Rozmiar <- (df$Wynik - min(df$Wynik, na.rm = TRUE) + 1e-9)^2

  ggplot2::ggplot(df, ggplot2::aes(x = WSM, y = WPM)) +
    ggplot2::geom_abline(linetype = "dashed") +
    ggplot2::geom_point(ggplot2::aes(size = Rozmiar, fill = Wynik),
                        shape = 21, color = "black", alpha = 0.85) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(df$Alternatywa)),
                       vjust = -1, size = 3) +
    ggplot2::labs(
      title = "Mapa Spójności WASPAS",
      subtitle = "Jeśli punkty są blisko przekątnej, WSM i WPM są zgodne.",
      x = "WSM (suma ważona)",
      y = "WPM (iloczyn ważony)",
      size = "Siła",
      fill = "Wynik Q"
    ) +
    .motyw_mcda()
}

utils::globalVariables(c("D_minus", "D_plus", "Wynik", "Def_S", "Def_R", "Def_Q", "WSM", "WPM", "Alternatywa", "Rozmiar"))
