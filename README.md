# SocialCampaignRankR

SocialCampaignRankR is an R package designed for ranking social media
channels and marketing campaigns using Multi-Criteria Decision Analysis
(MCDA).

The package supports criterion weighting (including the Best--Worst
Method) and fuzzy extensions based on Triangular Fuzzy Numbers (TFN).\
The project was developed as a demonstrative and academic implementation
of MCDA methods in marketing analytics.

------------------------------------------------------------------------

## Features

-   Preparation of raw marketing metrics for MCDA analysis

-   Declarative criterion definition syntax:

    Kryterium =\~ zm1 + zm2

-   Scaling to Saaty preference scale (1--9)

-   Transformation to Triangular Fuzzy Numbers (TFN)

-   Criterion weighting:

    -   Manual weights
    -   Best--Worst Method (BWM)
    -   Shannon entropy (automatic fallback)

-   Ranking methods:

    -   Fuzzy TOPSIS
    -   Fuzzy VIKOR
    -   Fuzzy WASPAS

-   Meta-ranking with consistency and correlation analysis

------------------------------------------------------------------------

## Installation (Local Development)

Load the package from the project directory:

    devtools::load_all()

------------------------------------------------------------------------

## Example Data

    data("social_campaign_raw")
    head(social_campaign_raw)

Alternatives are identified by the `Channel` column.

------------------------------------------------------------------------

## Example Workflow

### Define Criteria

    skladnia <- "
    Reach =~ impressions + reach;
    Engagement =~ likes + comments + shares + engagement_rate;
    Cost =~ cpc + cpa;
    Conversion =~ ctr + conversions
    "

### Prepare Decision Matrix

    M <- przygotuj_dane_mcda(
      dane = social_campaign_raw,
      skladnia = skladnia,
      kolumna_alternatyw = "Channel"
    )

### Define Criterion Types

    typy <- c("max", "max", "min", "max")

### Compute Weights (BWM)

    kryteria <- c("Reach","Engagement","Cost","Conversion")
    b_to_o <- c(4, 3, 8, 1)
    o_to_w <- c(6, 5, 1, 7)

### Fuzzy TOPSIS Ranking

    res_topsis <- rozmyty_topsis(
      macierz_decyzyjna = M,
      typy_kryteriow = typy,
      bwm_kryteria = kryteria,
      bwm_najlepsze = b_to_o,
      bwm_najgorsze = o_to_w
    )

    res_topsis$wyniki

------------------------------------------------------------------------

## Meta-Ranking

    meta <- rozmyty_meta_ranking(
      macierz_decyzyjna = M,
      typy_kryteriow = typy,
      bwm_kryteria = kryteria,
      bwm_najlepsze = b_to_o,
      bwm_najgorsze = o_to_w
    )

    meta$porownanie
    round(meta$korelacje, 2)

------------------------------------------------------------------------

## Documentation

    browseVignettes("SocialCampaignRankR")

------------------------------------------------------------------------

## Applications

-   Social media performance evaluation
-   Marketing campaign comparison
-   Demonstration of MCDA and BWM methods
-   Educational and analytical projects

------------------------------------------------------------------------

## License

Specify license in DESCRIPTION file (e.g., MIT, GPL-3).

------------------------------------------------------------------------

## Author

Provide author information in the DESCRIPTION file.
