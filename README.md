# What Empty Stadiums Revealed About Home Advantage

**Everyone knows home teams win more often. Almost nobody can tell you why.** In 2020 every
stadium in Europe emptied at the same time, which is about as close to a controlled
experiment as football will ever offer.

31,356 matches, 11 divisions, nine countries, 2016/17 to 2024/25.

Full writeup: [max-nudelman.github.io](https://max-nudelman.github.io/projects/home-advantage.html)

## The design

The usual explanations for home advantage are a list rather than an answer: the crowd, the
travel, the familiar pitch, the referee. They are impossible to separate because they always
happen together. The shutdown removed exactly one of them.

The two candidate mechanisms make **opposite, testable predictions**:

- **Players.** If home advantage comes from the team performing better, empty stadiums should
  shrink the home edge in shots and corners.
- **Referees.** If it comes from officials responding to crowd pressure, empty stadiums should
  shrink the home edge in fouls and cards while play barely moves.

So measure both.

## The headline

| era | home win | draw | away win | home edge |
|---|---|---|---|---|
| Crowds | 46.1% | 24.4% | 29.6% | **16.5 pts** |
| Empty stadiums | 40.4% | 25.6% | 33.9% | **6.5 pts** |
| Crowds back | 43.9% | 24.8% | 31.3% | **12.6 pts** |

Home win rate fell **5.6 percentage points**, 95% CI [3.9, 7.4], p ≈ 1.4e-10. About 60% of
home advantage vanished with the crowds. It did not vanish entirely, and that residual is the
part a crowd cannot explain: no travel, own bed, familiar pitch.

## The finding

Every channel is signed so positive means the home side is favoured, which requires flipping
the sign on fouls and cards, since an advantage there means *fewer* calls against you.

| channel | share of the home edge lost |
|---|---|
| Foul calls | **159%** (reversed) |
| Cards | **91%** |
| Corners | 50% |
| Shots | 47% |
| Shots on target | 45% |

**The players got somewhat worse. The referees stopped favouring them altogether.**

Bootstrapped, the referee channel shrank **44% more** than the performance channel,
95% CI [26%, 62%], holding in 100% of 2,000 resamples. Split by division, **10 of 11 show the
same ordering**. Greece is the single exception.

## The objection, and the answer

Maybe different officials were appointed during the closures. So restrict to referees who
worked in **both** eras and compare each against themselves:

| | fouls against home | cards |
|---|---|---|
| Crowds | −0.380 | −0.239 |
| Empty | **+0.624** | **+0.036** |

30 referees, those with at least eight matches in each era, 21 of whom favoured the home
side beforehand. During the closures, 21 were calling more fouls against home teams than
away. The same individuals reversed their own bias, so this is not about who got appointed.

Dropping the eight match minimum admits three more officials and gives −0.384 and +0.603,
so the threshold is not carrying the result.

## What would undermine this

- **Empty stadiums were not the only change.** 2020/21 also brought five substitutes and heavy
  fixture congestion. Both plausibly affect play; neither has an obvious reason to reverse a
  referee's foul bias, which is why the mechanism split is more robust than the headline.
- **The treatment is approximate.** This data records no attendance, so era comes from national
  policy dates. Some matches inside the empty window had partial crowds. That contamination
  biases estimates **toward zero**, so the true effect is at least as large as measured.
- **Ratios are unstable near zero.** Spain's pre-COVID foul edge was almost exactly zero, so
  its foul ratio exceeds 1600%. That is arithmetic, not a finding, which is why the
  country-level comparison uses cards.
- **Fouls measure play as well as officiating.** The card result, which fell almost to zero, is
  the harder one to explain away.

## Running it

Open `home-advantage.Rproj`, then:

```r
source("scripts/01-download.R")   # 99 league-seasons from football-data.co.uk
source("scripts/02-tidy.R")       # harmonize, assign eras
source("scripts/03-analysis.R")   # headline, mechanism, referees, significance
source("scripts/04-figures.R")    # four charts
source("scripts/05-export-web.R") # JSON for the interactive writeup
```

Data is not committed; step 1 rebuilds it.

## Next

- Match-level model with team and referee fixed effects rather than group means
- Proper difference-in-differences using the staggered timing of crowd returns by country
- Attendance figures where they exist, so the treatment is a dose rather than a switch
