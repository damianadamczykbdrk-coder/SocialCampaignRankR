# SocialCampaignRankR

SocialCampaignRankR jest pakietem języka R przeznaczonym do
rankingowania kanałów social media oraz kampanii marketingowych z
wykorzystaniem metod wielokryterialnego wspomagania decyzji (MCDA --
Multi-Criteria Decision Analysis).

Pakiet umożliwia wyznaczanie wag kryteriów (w tym metodą Best--Worst
Method) oraz zastosowanie podejścia rozmytego opartego na trójkątnych
liczbach rozmytych (TFN -- Triangular Fuzzy Numbers).

Projekt został opracowany jako rozwiązanie demonstracyjne i
zaliczeniowe, prezentujące praktyczne zastosowanie metod MCDA w analizie
danych marketingowych.

------------------------------------------------------------------------

## Zakres funkcjonalny

Pakiet umożliwia:

-   przygotowanie danych do analizy MCDA na podstawie surowych metryk
    marketingowych,

-   definiowanie kryteriów agregujących zmienne wejściowe przy użyciu
    deklaratywnej składni:

    Kryterium =\~ zm1 + zm2

-   skalowanie wartości do skali preferencji Saaty'ego (1--9),

-   transformację danych do postaci trójkątnych liczb rozmytych (TFN),

-   wyznaczanie wag kryteriów:

    -   manualnie,
    -   metodą Best--Worst Method (BWM),
    -   metodą entropii Shannona (automatycznie),

-   budowę rankingów alternatyw z wykorzystaniem metod:

    -   Fuzzy TOPSIS,
    -   Fuzzy VIKOR,
    -   Fuzzy WASPAS,

-   agregację rankingów bazowych (meta-ranking) wraz z analizą zgodności
    wyników.

------------------------------------------------------------------------

## Instalacja (lokalnie)

Pakiet można testować bezpośrednio z katalogu projektu:

    devtools::load_all()

------------------------------------------------------------------------

## Dane przykładowe

Pakiet zawiera przykładowy zbiór danych:

    data("social_campaign_raw")
    head(social_campaign_raw)

Alternatywy identyfikowane są za pomocą zmiennej `Channel`, natomiast
pozostałe kolumny stanowią wskaźniki wykorzystywane w analizie
wielokryterialnej.

------------------------------------------------------------------------

## Przykład zastosowania

### Definicja kryteriów

    skladnia <- "
    Reach =~ impressions + reach;
    Engagement =~ likes + comments + shares + engagement_rate;
    Cost =~ cpc + cpa;
    Conversion =~ ctr + conversions
    "

### Przygotowanie macierzy decyzyjnej

    M <- przygotuj_dane_mcda(
      dane = social_campaign_raw,
      skladnia = skladnia,
      kolumna_alternatyw = "Channel"
    )

### Określenie typów kryteriów

    typy <- c("max", "max", "min", "max")

### Wyznaczenie wag metodą BWM

    kryteria <- c("Reach","Engagement","Cost","Conversion")
    b_to_o <- c(4, 3, 8, 1)
    o_to_w <- c(6, 5, 1, 7)

### Ranking metodą Fuzzy TOPSIS

    res_topsis <- rozmyty_topsis(
      macierz_decyzyjna = M,
      typy_kryteriow = typy,
      bwm_kryteria = kryteria,
      bwm_najlepsze = b_to_o,
      bwm_najgorsze = o_to_w
    )

    res_topsis$wyniki

------------------------------------------------------------------------

## Meta-ranking

    meta <- rozmyty_meta_ranking(
      macierz_decyzyjna = M,
      typy_kryteriow = typy,
      bwm_kryteria = kryteria,
      bwm_najlepsze = b_to_o,
      bwm_najgorsze = o_to_w
    )

    meta$porownanie
    round(meta$korelacje, 2)

Wyniki obejmują: - zestawienie rankingów cząstkowych, - ranking
zagregowany, - macierz korelacji między metodami.

------------------------------------------------------------------------

## Dokumentacja

Szczegółowy opis działania pakietu wraz z przykładem krok po kroku
dostępny jest w vignette:

    browseVignettes("SocialCampaignRankR")

------------------------------------------------------------------------

## Informacje techniczne

Szczegóły dotyczące zależności, licencji oraz autorów znajdują się w
pliku DESCRIPTION pakietu.
