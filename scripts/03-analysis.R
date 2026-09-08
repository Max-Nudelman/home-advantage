# =============================================================================
# Home advantage: Step 3 — how big, and what is it made of
#
# THE QUESTION
# Everyone knows home teams win more. Almost nobody can tell you WHY. The two
# candidate mechanisms are:
#
#   PLAYERS   the home side performs better, driven by the crowd, familiarity
#             and no travel. If true, empty stadiums should shrink the home
#             team's edge in shots, shots on target and corners.
#
#   REFEREES  officials favour the home side under crowd pressure. If true,
#             empty stadiums should shrink the home team's edge in fouls
#             called and cards shown, while shots barely move.
#
# These make opposite, testable predictions. That is the whole design.
# =============================================================================

library(tidyverse)

m <- read_csv("data-clean/matches.csv", show_col_types = FALSE) %>%
  mutate(era = factor(era, levels = c("Crowds", "Empty", "Crowds back")))

# ---- 1. the headline effect -------------------------------------------------
cat("=== 1. HOME ADVANTAGE BY ERA (all 11 divisions) ===\n")
headline <- m %>%
  group_by(era) %>%
  summarise(matches = n(),
            home_win = mean(home_win),
            home_ppg = mean(home_pts),
            gd       = mean(gd), .groups = "drop")
print(headline %>% mutate(across(c(home_win), ~scales::percent(.x, .1)),
                          across(c(home_ppg, gd), ~round(.x, 3))))

drop_pp <- (headline$home_win[1] - headline$home_win[2]) * 100
cat(sprintf("\nHome win rate fell %.1f percentage points when the crowds left.\n", drop_pp))

# ---- 2. the mechanism test --------------------------------------------------
# Each channel is measured as home minus away, so a positive number means the
# home side is favoured on that dimension. The question is which channels move.
cat("\n=== 2. WHICH CHANNEL ACTUALLY MOVED? ===\n")

channels <- c(`Shots`          = "shot_diff",
              `Shots on target`= "sot_diff",
              `Corners`        = "corner_diff",
              `Fouls against`  = "foul_diff",
              `Cards`          = "card_diff")

mech <- map_dfr(names(channels), function(lbl) {
  v <- channels[[lbl]]
  m %>% filter(!is.na(.data[[v]])) %>%
    group_by(era) %>% summarise(val = mean(.data[[v]]), .groups="drop") %>%
    pivot_wider(names_from = era, values_from = val) %>%
    mutate(channel = lbl, .before = 1)
}) %>%
  mutate(change    = Empty - Crowds,
         pct_of_pre = change / abs(Crowds),
         recovered = `Crowds back` - Empty)

print(mech %>% mutate(across(where(is.numeric), ~round(.x, 3))))

cat("\nRead the pct_of_pre column. That is each channel's change as a share of\n")
cat("its own pre-COVID size, which makes channels on different units comparable.\n")

# ---- 3. robustness: the same referees, before and during ---------------------
# Referee names are only present for about a sixth of matches, almost all of
# them English. Restricting to referees who officiated in BOTH eras removes
# the worry that the effect is just a change in who was appointed.
cat("\n=== 3. SAME REFEREES, BEFORE AND DURING ===\n")
ref <- m %>% filter(!is.na(Referee), Referee != "", era != "Crowds back")
both <- ref %>% distinct(Referee, era) %>% count(Referee) %>% filter(n == 2) %>% pull(Referee)
ref2 <- ref %>% filter(Referee %in% both)

cat("referees officiating in both eras:", length(both),
    "| matches:", nrow(ref2), "\n")
ref2 %>% group_by(era) %>%
  summarise(matches = n(),
            foul_diff = round(mean(foul_diff, na.rm=TRUE), 3),
            card_diff = round(mean(card_diff, na.rm=TRUE), 3),
            shot_diff = round(mean(shot_diff, na.rm=TRUE), 3), .groups="drop") %>%
  print()

# ---- 4. did it come back? ---------------------------------------------------
cat("\n=== 4. RECOVERY BY COUNTRY (home win rate) ===\n")
m %>% group_by(country, era) %>%
  summarise(hw = mean(home_win), .groups = "drop") %>%
  pivot_wider(names_from = era, values_from = hw) %>%
  mutate(fall = Crowds - Empty, vs_baseline = `Crowds back` - Crowds) %>%
  arrange(desc(fall)) %>%
  mutate(across(where(is.numeric), ~round(.x * 100, 1))) %>%
  print(n = Inf)

write_csv(mech, "data-clean/mechanism.csv")
write_csv(headline, "data-clean/headline.csv")

# ---- 5. is any of this distinguishable from noise? ---------------------------
# Everything above is a difference in means. With 4,277 matches in the empty
# window that is a decent sample, but "decent sample" is not evidence. Test it.
cat("\n=== 5. SIGNIFICANCE ===\n")

pre   <- filter(m, era == "Crowds")
empty <- filter(m, era == "Empty")

# home win rate: two-proportion test
pt <- prop.test(c(sum(pre$home_win), sum(empty$home_win)), c(nrow(pre), nrow(empty)))
cat(sprintf("home win rate  : %.1f pp fall, 95%% CI [%.1f, %.1f], p = %s\n",
            (mean(pre$home_win) - mean(empty$home_win)) * 100,
            pt$conf.int[1]*100, pt$conf.int[2]*100, format.pval(pt$p.value, digits = 2)))

# each channel: Welch t-test on the home-minus-away differential
for (lbl in names(channels)) {
  v <- channels[[lbl]]
  # sign convention matches section 2: Empty minus Crowds, so a negative number
  # means the home team's edge on that channel shrank.
  tt <- t.test(empty[[v]], pre[[v]])
  cat(sprintf("%-16s: change %+.3f, 95%% CI [%+.3f, %+.3f], p = %s\n",
              lbl, tt$estimate[1] - tt$estimate[2], tt$conf.int[1], tt$conf.int[2],
              format.pval(tt$p.value, digits = 2)))
}

# the decisive comparison: did the REFEREE channel fall by proportionally more
# than the PERFORMANCE channel? Bootstrap the difference of the two ratios.
cat("\n--- referee channel vs performance channel, bootstrapped ---\n")
set.seed(1)
ratio_gap <- replicate(2000, {
  a <- pre[sample(nrow(pre), nrow(pre), TRUE), ]
  b <- empty[sample(nrow(empty), nrow(empty), TRUE), ]
  perf <- (mean(b$shot_diff, na.rm=TRUE) - mean(a$shot_diff, na.rm=TRUE)) / abs(mean(a$shot_diff, na.rm=TRUE))
  refc <- (mean(b$card_diff, na.rm=TRUE) - mean(a$card_diff, na.rm=TRUE)) / abs(mean(a$card_diff, na.rm=TRUE))
  refc - abs(perf)
})
cat(sprintf("referee channel shrank %.0f%% more than performance, 95%% CI [%.0f%%, %.0f%%]\n",
            mean(ratio_gap)*100, quantile(ratio_gap,.025)*100, quantile(ratio_gap,.975)*100))
cat(sprintf("share of bootstrap draws where referee > performance: %.1f%%\n",
            mean(ratio_gap > 0)*100))
