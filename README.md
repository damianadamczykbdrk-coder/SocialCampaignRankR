SocialCampaignRankR

SocialCampaignRankR to pakiet R służący do rankingowania kanałów social media
(lub kampanii marketingowych) z wykorzystaniem metod wielokryterialnego wspomagania decyzji (MCDA),
z uwzględnieniem wag kryteriów (BWM) oraz elementów rozmycia (TFN – triangular fuzzy numbers).

Pakiet został przygotowany jako projekt zaliczeniowy i demonstracyjny.

🔹 Główne możliwości

przygotowanie danych do analizy MCDA na podstawie surowych metryk,

definiowanie kryteriów za pomocą czytelnej składni (Kryterium =~ zm1 + zm2),

skalowanie wyników do skali Saaty’ego (1–9),

rozmycie danych do postaci TFN (Triangular Fuzzy Numbers),

wyznaczanie wag kryteriów:

ręcznie,

metodą BWM (Best–Worst Method),

metodą Entropii Shannona (automatyczny fallback),

ranking alternatyw metodami:

Fuzzy TOPSIS

Fuzzy VIKOR

Fuzzy WASPAS

budowa meta-rankingu (konsensus) i analiza zgodności rankingów.

🔹 Instalacja (lokalnie)

Pakiet można testować lokalnie z katalogu projektu:

devtools::load_all()

🔹 Dane przykładowe

Pakiet zawiera przykładowy zbiór danych:

data("social_campaign_raw")
head(social_campaign_raw)

Dane reprezentują metryki kampanii/kanałów social media, a alternatywami są kanały (Channel).

🔹 Szybki przykład użycia

library(SocialCampaignRankR)

# Definicja kryteriów
skladnia <- "
Reach =~ impressions + reach;
Engagement =~ likes + comments + shares + engagement_rate;
Cost =~ cpc + cpa;
Conversion =~ ctr + conversions
"

# Przygotowanie macierzy TFN
M <- przygotuj_dane_mcda(
  dane = social_campaign_raw,
  skladnia = skladnia,
  kolumna_alternatyw = "Channel"
)

# Typy kryteriów
typy <- c("max", "max", "min", "max")

# Wagi metodą BWM
kryteria <- c("Reach","Engagement","Cost","Conversion")
b_to_o <- c(4, 3, 8, 1)
o_to_w <- c(6, 5, 1, 7)

# Ranking TOPSIS
res_topsis <- rozmyty_topsis(
  macierz_decyzyjna = M,
  typy_kryteriow = typy,
  bwm_kryteria = kryteria,
  bwm_najlepsze = b_to_o,
  bwm_najgorsze = o_to_w
)

res_topsis$wyniki

🔹 Meta-ranking

Pakiet umożliwia agregację rankingów metod bazowych:

meta <- rozmyty_meta_ranking(
  macierz_decyzyjna = M,
  typy_kryteriow = typy,
  bwm_kryteria = kryteria,
  bwm_najlepsze = b_to_o,
  bwm_najgorsze = o_to_w
)

meta$porownanie
round(meta$korelacje, 2)

🔹 Dokumentacja

Pełny opis działania pakietu wraz z przykładem krok po kroku znajduje się w vignette:

browseVignettes("SocialCampaignRankR")

🔹 Zastosowania

analiza efektywności kanałów social media,

porównywanie kampanii marketingowych,

demonstracja metod MCDA i BWM w praktyce,

projekty dydaktyczne i analityczne.
