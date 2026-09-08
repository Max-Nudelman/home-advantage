# =============================================================================
# Home advantage: Step 2 — tidy, and define the natural experiment
#
# THE DESIGN
# In March 2020 every European league stopped. When they restarted, they played
# in empty stadiums. Crowds returned at different times in different countries.
# That gives a rare thing in observational sport data: the same teams, the same
# referees, the same competition, with one input switched off and back on.
#
# THE HONEST PART
# This dataset does not record attendance. Era is assigned from documented
# national policy windows, which are approximate at the edges. A minority of
# matches inside the EMPTY window did have limited crowds, mostly in autumn
# 2020 before the second wave closed grounds again. That contamination biases
# every estimate here TOWARD ZERO, so the true effect is at least as large as
# what is measured, never smaller. Stating the direction of a bias is more
# useful than pretending it is absent.
# =============================================================================

library(tidyverse)

LEAGUES <- tribble(
  ~div,  ~country,       ~league,
  "E0",  "England",      "Premier League",
  "D1",  "Germany",      "Bundesliga",
  "I1",  "Italy",        "Serie A",
  "SP1", "Spain",        "La Liga",
  "F1",  "France",       "Ligue 1",
  "N1",  "Netherlands",  "Eredivisie",
  "P1",  "Portugal",     "Primeira Liga",
  "B1",  "Belgium",      "Pro League",
  "T1",  "Turkey",       "Super Lig",
  "G1",  "Greece",       "Super League",
  "SC0", "Scotland",     "Premiership"
)

KEEP <- c("Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR","Referee",
          "HS","AS","HST","AST","HF","AF","HC","AC","HY","AY","HR","AR")

read_one <- function(path) {
  div <- str_extract(basename(path), "^[A-Z0-9]+")
  raw <- suppressWarnings(read_csv(path, show_col_types = FALSE,
                                   name_repair = "unique",
                                   locale = locale(encoding = "latin1")))
  missing <- setdiff(KEEP, names(raw))
  for (m in missing) raw[[m]] <- NA
  raw %>%
    select(all_of(KEEP)) %>%
    filter(!is.na(HomeTeam), HomeTeam != "") %>%
    mutate(div = div,
           season = str_extract(basename(path), "\\d{4}(?=\\.csv)"),
           across(c(FTHG, FTAG, HS, AS, HST, AST, HF, AF, HC, AC, HY, AY, HR, AR),
                  ~suppressWarnings(as.numeric(.x))),
           Date = suppressWarnings(dmy(Date)))
}

matches <- list.files("data-raw", full.names = TRUE, pattern = "\\.csv$") %>%
  map_dfr(read_one) %>%
  filter(!is.na(Date), !is.na(FTHG), !is.na(FTAG)) %>%
  left_join(LEAGUES, by = "div")

# --- era assignment ----------------------------------------------------------
# PRE    : everything before the March 2020 suspension
# EMPTY  : restart in mid 2020 through the end of the 2020/21 season
# RETURN : 2021/22 onward, when crowds were broadly back
SUSPENDED_FROM <- ymd("2020-03-09")
EMPTY_TO       <- ymd("2021-06-30")

matches <- matches %>%
  mutate(era = case_when(
    Date <  SUSPENDED_FROM ~ "Crowds",
    Date <= EMPTY_TO       ~ "Empty",
    TRUE                   ~ "Crowds back"
  ) %>% factor(levels = c("Crowds", "Empty", "Crowds back")))

# --- the outcome measures ----------------------------------------------------
matches <- matches %>%
  mutate(
    home_pts   = case_when(FTR == "H" ~ 3, FTR == "D" ~ 1, TRUE ~ 0),
    home_win   = as.integer(FTR == "H"),
    gd         = FTHG - FTAG,               # goal difference, home minus away
    shot_diff  = HS - AS,                   # did the home team play better?
    sot_diff   = HST - AST,
    corner_diff= HC - AC,
    foul_diff  = HF - AF,                   # fouls GIVEN AGAINST the home team
    card_diff  = (HY + 2*HR) - (AY + 2*AR)  # a red counts double
  )

write_csv(matches, "data-clean/matches.csv")

cat("matches:", nrow(matches), "\n")
cat("leagues:", n_distinct(matches$div), " seasons:", n_distinct(matches$season), "\n")
print(count(matches, era))
cat("\ncoverage of the mechanism columns (needed for the referee test):\n")
matches %>% summarise(across(c(HS, HF, HY, HR, Referee), ~mean(!is.na(.x)))) %>%
  mutate(across(everything(), ~scales::percent(.x, .1))) %>% print()
