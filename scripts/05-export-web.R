# =============================================================================
# Home advantage: Step 5 — JSON for the interactive writeup
# The site reads this, so the page and the analysis cannot drift apart.
# =============================================================================
library(tidyverse); library(jsonlite)

m <- read_csv("data-clean/matches.csv", show_col_types = FALSE) %>%
  mutate(era = factor(era, levels = c("Crowds","Empty","Crowds back")))

monthly <- m %>%
  mutate(mth = format(floor_date(Date, "month"), "%Y-%m")) %>%
  group_by(mth) %>% filter(n() >= 40) %>%
  summarise(hw = round(mean(home_win), 4), n = n(), .groups = "drop")

by_country <- m %>%
  group_by(country, era) %>%
  summarise(hw = mean(home_win), gd = mean(gd),
            shot = mean(shot_diff, na.rm=TRUE), card = mean(card_diff, na.rm=TRUE),
            foul = mean(foul_diff, na.rm=TRUE), n = n(), .groups="drop") %>%
  mutate(across(c(hw, gd, shot, card, foul), ~round(.x, 4)))

# per-country, per-channel share of the home edge lost
edges <- m %>% filter(era != "Crowds back") %>%
  group_by(country, era) %>%
  summarise(Shots = mean(shot_diff, na.rm=TRUE),
            `Shots on target` = mean(sot_diff, na.rm=TRUE),
            Corners = mean(corner_diff, na.rm=TRUE),
            `Foul calls` = -mean(foul_diff, na.rm=TRUE),   # signed so + favours home
            Cards = -mean(card_diff, na.rm=TRUE),
            .groups="drop") %>%
  pivot_longer(-c(country, era), names_to="channel", values_to="edge") %>%
  pivot_wider(names_from=era, values_from=edge) %>%
  mutate(lost = round((Crowds - Empty) / Crowds, 4),
         Crowds = round(Crowds,4), Empty = round(Empty,4))


# Pooled across every match, not an average of country averages. The two differ
# because divisions play different numbers of matches, and the site must show
# the same number the analysis reports.
pooled <- m %>% filter(era != "Crowds back") %>%
  group_by(era) %>%
  summarise(Shots = mean(shot_diff, na.rm=TRUE),
            `Shots on target` = mean(sot_diff, na.rm=TRUE),
            Corners = mean(corner_diff, na.rm=TRUE),
            `Foul calls` = -mean(foul_diff, na.rm=TRUE),
            Cards = -mean(card_diff, na.rm=TRUE), .groups="drop") %>%
  pivot_longer(-era, names_to="channel", values_to="edge") %>%
  pivot_wider(names_from=era, values_from=edge) %>%
  mutate(lost = round((Crowds - Empty)/Crowds, 4), country = "All")

# Which divisions show the referee channel losing more than performance?
# Cards is the reference referee channel: foul ratios are unstable where the
# pre-COVID foul edge is near zero (Spain's baseline is ~0, so its ratio explodes).
consistency <- edges %>%
  select(country, channel, lost) %>%
  pivot_wider(names_from = channel, values_from = lost) %>%
  transmute(country, perf = Shots, cards = Cards,
            ref_dominant = Cards > Shots)

refs <- m %>% filter(!is.na(Referee), Referee != "", era != "Crowds back") %>%
  group_by(Referee, era) %>% filter(n() >= 8) %>%
  summarise(fd = round(mean(foul_diff, na.rm=TRUE), 3), n = n(), .groups="drop") %>%
  pivot_wider(names_from=era, values_from=c(fd, n)) %>% drop_na()

write_json(list(monthly = monthly, by_country = by_country, edges = edges,
                pooled = pooled, consistency = consistency, refs = refs),
           "outputs/ha-web.json", auto_unbox = TRUE, na = "null")
cat("wrote outputs/ha-web.json\n")
cat("  months:", nrow(monthly), " countries:", n_distinct(by_country$country),
    " edge rows:", nrow(edges), " referees:", nrow(refs), "\n")
cat("  divisions where the referee channel lost more than performance:",
    sum(consistency$ref_dominant), "of", nrow(consistency), "\n")
