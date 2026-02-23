# data-raw/generuj_dane.R
set.seed(123)

# Alternatywy: kanały social media
social_campaign_raw <- data.frame(
  Channel = rep(c("Instagram", "TikTok", "Facebook", "YouTube"), each = 10),

  impressions = round(runif(40, 50000, 300000)),
  reach       = round(runif(40, 20000, 200000)),

  likes       = round(runif(40, 500, 20000)),
  comments    = round(runif(40, 10, 2000)),
  shares      = round(runif(40, 5, 5000)),
  engagement_rate = runif(40, 0.5, 8),

  ctr         = runif(40, 0.2, 4),
  conversions = round(runif(40, 10, 800)),

  cpc         = runif(40, 0.2, 4),
  cpa         = runif(40, 5, 120)
)

usethis::use_data(social_campaign_raw, overwrite = TRUE)
