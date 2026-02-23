---
  title: "Analiza efektywności kampanii social media metodami rozmytej MCDA"
author: "SocialCampaignRankR"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Analiza efektywności kampanii social media}
%\VignetteEngine{knitr::rmarkdown}
%\VignetteEncoding{UTF-8}
---

  ```{r, include = FALSE}
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(SocialCampaignRankR)
```

# 1. Wprowadzenie

Pakiet **SocialCampaignRankR** umożliwia ocenę i ranking kanałów social media
z wykorzystaniem rozmytych metod wielokryterialnego wspomagania decyzji (MCDA).

W analizie wykorzystujemy:

  - rozmyte liczby trójkątne (TFN),
- metodę wag Best–Worst Method (BWM),
- metody rankingowe: Fuzzy TOPSIS, Fuzzy VIKOR, Fuzzy WASPAS,
- meta-ranking (konsensus metod).

# 2. Dane przykładowe

```{r}
data("social_campaign_raw")
head(social_campaign_raw)
```

Dane reprezentują metryki kampanii marketingowych.
Alternatywami są kanały social media (`Channel`).

# 3. Definicja modelu kryteriów

Kryteria definiujemy za pomocą składni:

  ```{r}
skladnia <- "
Reach =~ impressions + reach;
Engagement =~ likes + comments + shares + engagement_rate;
Cost =~ cpc + cpa;
Conversion =~ ctr + conversions
"
```

# 4. Budowa macierzy rozmytej (TFN)

```{r}
M <- przygotuj_dane_mcda(
  dane = social_campaign_raw,
  skladnia = skladnia,
  kolumna_alternatyw = "Channel"
)

attr(M, "nazwy_kryteriow")
```

Macierz zawiera wartości w postaci trójkątnych liczb rozmytych (l, m, u).

# 5. Typy kryteriów

```{r}
typy <- c("max", "max", "min", "max")
```

# 6. Wyznaczanie wag kryteriów

## 6.1 Metoda BWM

```{r}
kryteria <- c("Reach","Engagement","Cost","Conversion")

b_to_o <- c(4, 3, 8, 1)
o_to_w <- c(6, 5, 1, 7)

wagi_bwm <- oblicz_wagi_bwm(kryteria, b_to_o, o_to_w)
wagi_bwm$wagi_kryteriow
```

## 6.2 Automatyczne wagi (Entropia)

```{r}
wagi_entropia <- oblicz_wagi_entropii(M)
wagi_entropia
```

# 7. Ranking metodą Fuzzy TOPSIS

```{r}
topsis <- rozmyty_topsis(
  macierz_decyzyjna = M,
  typy_kryteriow = typy,
  bwm_kryteria = kryteria,
  bwm_najlepsze = b_to_o,
  bwm_najgorsze = o_to_w
)

topsis$wyniki
```

```{r, fig.height=5}
plot(topsis)
```

# 8. Ranking metodą Fuzzy VIKOR

```{r}
vikor <- rozmyty_vikor(
  macierz_decyzyjna = M,
  typy_kryteriow = typy,
  bwm_kryteria = kryteria,
  bwm_najlepsze = b_to_o,
  bwm_najgorsze = o_to_w
)

vikor$wyniki
```

```{r, fig.height=5}
plot(vikor)
```

# 9. Ranking metodą Fuzzy WASPAS

```{r}
waspas <- rozmyty_waspas(
  macierz_decyzyjna = M,
  typy_kryteriow = typy
)

waspas$wyniki
```

```{r, fig.height=5}
plot(waspas)
```

# 10. Meta-ranking (konsensus)

```{r}
meta <- rozmyty_meta_ranking(
  macierz_decyzyjna = M,
  typy_kryteriow = typy,
  bwm_kryteria = kryteria,
  bwm_najlepsze = b_to_o,
  bwm_najgorsze = o_to_w
)

meta$porownanie
round(meta$korelacje, 2)
```

# 11. Interpretacja wyników

W zależności od przyjętej metody rankingowej, pozycja alternatyw może się różnić.
Meta-ranking pozwala uzyskać stabilny konsensus wyników.

Pakiet umożliwia kompleksową analizę efektywności kampanii
z uwzględnieniem niepewności danych oraz zróżnicowanej ważności kryteriów.
