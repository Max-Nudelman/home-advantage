# =============================================================================
# Home advantage: Step 4 — figures
# Each chart makes one argument. Styled to match the portfolio site.
# =============================================================================
library(tidyverse)

INK<-"#0B0D10"; LINE<-"#262C35"; TEXT<-"#E8EAED"; MUTED<-"#A0A8B4"
ACC<-"#5EE6A8"; WARN<-"#F5A524"; BLUE<-"#7FB2F0"; DIM<-"#6C7684"

theme_ha <- function() theme_minimal(base_size = 13) + theme(
  plot.background=element_rect(fill=INK,colour=NA), panel.background=element_rect(fill=INK,colour=NA),
  panel.grid.major=element_line(colour=LINE,linewidth=.3), panel.grid.minor=element_blank(),
  text=element_text(colour=TEXT), plot.title=element_text(colour=TEXT,face="bold",size=15),
  plot.subtitle=element_text(colour=MUTED,size=11,lineheight=1.25),
  plot.caption=element_text(colour=DIM,size=9,hjust=0),
  axis.text=element_text(colour=MUTED), axis.title=element_text(colour=MUTED,size=11),
  legend.text=element_text(colour=MUTED), legend.title=element_text(colour=MUTED),
  strip.text=element_text(colour=TEXT,face="bold"), plot.margin=margin(16,18,12,16))
sf <- function(p,n,w=9,h=5.4) ggsave(file.path("outputs",n),p,width=w,height=h,dpi=150,bg=INK)

m <- read_csv("data-clean/matches.csv", show_col_types=FALSE) %>%
  mutate(era = factor(era, levels=c("Crowds","Empty","Crowds back")))

# --- FIG 1: the effect, by month, so you can see it happen ------------------
f1 <- m %>%
  mutate(mth = floor_date(Date, "month")) %>%
  group_by(mth) %>% filter(n() >= 40) %>%
  summarise(hw = mean(home_win), n = n(), .groups="drop") %>%
  ggplot(aes(mth, hw)) +
  annotate("rect", xmin=ymd("2020-03-09"), xmax=ymd("2021-06-30"), ymin=-Inf, ymax=Inf,
           fill=WARN, alpha=.09) +
  annotate("text", x=ymd("2020-11-15"), y=.575, label="empty stadiums", colour=WARN, size=3.6) +
  geom_hline(yintercept=.461, colour=DIM, linetype="dashed", linewidth=.4) +
  annotate("text", x=ymd("2016-10-01"), y=.472, label="pre-COVID average, 46.1%", colour=DIM, size=3.2, hjust=0) +
  geom_line(colour="#39414C", linewidth=.5) +
  geom_point(aes(size=n), colour=ACC, alpha=.8) +
  scale_size_continuous(range=c(.8,3), guide="none") +
  scale_y_continuous(labels=scales::percent) +
  labs(title="Home advantage fell off a cliff, then came most of the way back",
       subtitle="Share of matches won by the home side, by month, across 11 European divisions.\nEach point is one month; larger points are months with more fixtures.",
       x=NULL, y="Home win rate",
       caption="31,356 matches, 2016/17 to 2024/25. Source: football-data.co.uk") +
  theme_ha()
sf(f1,"01-timeline.png")

# --- FIG 2: THE ARGUMENT — which channel moved ------------------------------
# Every channel is restated as a HOME EDGE, so that positive always means
# "favours the home team". For fouls and cards that means flipping the sign,
# because a home advantage there shows up as FEWER calls against the home side.
# Once every channel points the same way, the bars are directly comparable and
# a bar past 100% means the edge did not merely vanish, it reversed.
ch <- tribble(
  ~channel,             ~edge_crowds, ~edge_empty,
  "Shots",                     2.532,       1.339,
  "Shots on target",           0.969,       0.536,
  "Corners",                   1.078,       0.535,
  "Foul calls (fewer against)", 0.300,      -0.177,   # sign flipped
  "Cards (fewer shown)",        0.373,       0.032    # sign flipped
) %>%
  mutate(lost = (edge_crowds - edge_empty) / edge_crowds,
         grp  = if_else(str_detect(channel, "Foul|Cards"),
                        "Referee decisions", "Team performance"),
         channel = fct_reorder(channel, lost))

f2 <- ggplot(ch, aes(lost, channel, fill = grp)) +
  geom_vline(xintercept = 0, colour = "#3A434F") +
  geom_vline(xintercept = 1, colour = DIM, linetype = "dashed") +
  geom_col(width = .62) +
  geom_text(aes(label = scales::percent(lost, 1)), hjust = -0.18, colour = TEXT, size = 3.8) +
  annotate("text", x = 1.02, y = 5.42, label = "edge completely gone",
           colour = DIM, size = 3.1, hjust = 0) +
  scale_fill_manual(values = c("Referee decisions" = WARN, "Team performance" = ACC), name = NULL) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1.95),
                     breaks = c(0, .5, 1, 1.5)) +
  labs(title = "Players got somewhat worse. Referees stopped favouring them altogether.",
       subtitle = "Share of the home team's pre-COVID edge that disappeared when crowds were removed.\nEvery channel is signed so that positive means the home side is favoured.\nA bar past 100% means the edge did not just vanish, it reversed.",
       x = "Share of the home edge lost", y = NULL,
       caption = "Bootstrapped, the referee channel shrank 44% more than the performance channel, 95% CI [26%, 62%].") +
  theme_ha() + theme(legend.position = "top")
sf(f2, "02-mechanism.png", w = 9.6, h = 5.6)

# --- FIG 3: same referees, before and during --------------------------------
# The obvious objection to the mechanism finding is selection: maybe different
# officials were appointed during the closures. This restricts to referees who
# worked in BOTH eras, so each point is one person compared against themselves.
ref <- m %>% filter(!is.na(Referee), Referee != "", era != "Crowds back")
both <- ref %>% distinct(Referee, era) %>% count(Referee) %>% filter(n == 2) %>% pull(Referee)
rd <- ref %>% filter(Referee %in% both) %>%
  group_by(Referee, era) %>% filter(n() >= 8) %>%
  summarise(fd = mean(foul_diff, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = era, values_from = fd) %>% drop_na()

f3 <- ggplot(rd, aes(x = Crowds, y = Empty)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf, fill = WARN, alpha = .07) +
  geom_abline(slope = 1, intercept = 0, colour = "#3A434F", linetype = "dashed") +
  geom_hline(yintercept = 0, colour = "#4A5462") +
  geom_vline(xintercept = 0, colour = "#4A5462") +
  geom_point(colour = ACC, size = 3.2, alpha = .85) +
  annotate("text", x = -2.5, y = 2.85, hjust = 0, colour = WARN, size = 3.2, lineheight = 1.15,
           label = "shaded: now calls MORE fouls\non the home team than the away team") +
  annotate("text", x = 0.15, y = -1.75, hjust = 0, colour = DIM, size = 3.1, lineheight = 1.15,
           label = "dashed line: no change\nfrom their own baseline") +
  labs(title = "The same officials, reversing their own bias",
       subtitle = "Each point is one referee who worked both before and during the closures, compared against\nthemselves. The axis is fouls called against the home team minus the away team, so a negative\nvalue means that referee favoured the home side.",
       x = "With crowds", y = "Empty stadiums",
       caption = "30 English referees with 8 or more matches in each era. 21 of them favoured the home side before the closures; during them, 21 were\ncalling more fouls against home teams than away. 19 of 30 moved in the anti-home direction. Individual referee samples are small, so the\nper-point noise is large; the claim rests on the direction of the group, not on any one official.") +
  theme_ha()
sf(f3, "03-referees.png", w = 9.6, h = 5.8)

# --- FIG 4: it did not fully come back --------------------------------------
f4 <- m %>% group_by(country, era) %>% summarise(hw=mean(home_win), .groups="drop") %>%
  pivot_wider(names_from=era, values_from=hw) %>%
  mutate(country=fct_reorder(country, Crowds-Empty)) %>%
  pivot_longer(-country, names_to="era", values_to="hw") %>%
  mutate(era=factor(era, levels=c("Crowds","Empty","Crowds back"))) %>%
  ggplot(aes(hw, country, colour=era)) +
  geom_line(aes(group=country), colour="#2E3640", linewidth=1.4) +
  geom_point(size=3.4) +
  scale_colour_manual(values=c("Crowds"=BLUE,"Empty"=WARN,"Crowds back"=ACC), name=NULL) +
  scale_x_continuous(labels=scales::percent) +
  labs(title="Every country moved the same direction, by different amounts",
       subtitle="Greece lost 10.7 points of home advantage, Italy only 2.4. In nine of eleven countries the\nrecovery has still not reached the pre-COVID level.",
       x="Home win rate", y=NULL,
       caption="Ordered by size of the fall. Recovery is measured across 2021/22 to 2024/25.") +
  theme_ha() + theme(legend.position="top")
sf(f4,"04-countries.png", h=5.8)

cat("wrote:\n"); cat(paste0("  outputs/", list.files("outputs", pattern="png")), sep="\n")
