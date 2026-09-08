# =============================================================================
# Home advantage: Step 1 — download
#
# 11 divisions across 9 countries, seasons 2016/17 to 2024/25.
# Nine seasons gives three clean pre-COVID years, the behind-closed-doors
# period, and four seasons of recovery afterwards.
#
# Source: football-data.co.uk, one row per match, with shots, fouls, corners,
# cards and the referee's name. That last column is what makes the mechanism
# test possible.
# =============================================================================

library(tidyverse)

LEAGUES <- c(E0 = "England, Premier League",  D1  = "Germany, Bundesliga",
             I1 = "Italy, Serie A",           SP1 = "Spain, La Liga",
             F1 = "France, Ligue 1",          N1  = "Netherlands, Eredivisie",
             P1 = "Portugal, Primeira Liga",  B1  = "Belgium, Pro League",
             T1 = "Turkey, Super Lig",        G1  = "Greece, Super League",
             SC0 = "Scotland, Premiership")

SEASONS <- c("1617","1718","1819","1920","2021","2122","2223","2324","2425")

dir.create("data-raw", showWarnings = FALSE)

fetch <- function(season, div) {
  dest <- file.path("data-raw", paste0(div, "_", season, ".csv"))
  if (file.exists(dest)) return(invisible(dest))
  url <- sprintf("https://football-data.co.uk/mmz4281/%s/%s.csv", season, div)
  try(download.file(url, dest, quiet = TRUE), silent = TRUE)
  Sys.sleep(0.3)
  invisible(dest)
}

grid <- expand_grid(season = SEASONS, div = names(LEAGUES))
walk2(grid$season, grid$div, fetch)

got <- list.files("data-raw", pattern = "\\.csv$")
cat("downloaded", length(got), "of", nrow(grid), "league-seasons\n")
