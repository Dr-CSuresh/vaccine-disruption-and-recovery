# vaccine-disruption-and-recovery
Longitudinal analysis of global childhood vaccination disruption and recovery during the COVID-19 pandemic 
# Vaccine Disruption and Recovery

### A longitudinal analysis of childhood vaccination during the COVID-19 pandemic

This project examines changes in the number of one-year-olds receiving routine childhood vaccines before and during the COVID-19 pandemic, with a particular focus on whether declines observed in 2020 had recovered by 2021.

The analysis uses R to explore global vaccination trends, country-level disruption and subsequent recovery for three core childhood vaccines:

- DTP3
- Polio third dose
- Measles-containing vaccine first dose (MCV1)

---

## Research question

How did the number of one-year-olds receiving routine childhood vaccines change during 2020, and how consistently had affected countries recovered by 2021?

---

### Vaccination counts fell substantially in 2020

Across 195 entities with complete observations:

- DTP3: 145/195 (74.4%) recorded lower vaccination counts in 2020 than 2019.
- MCV1: 144/195 (73.8%) recorded a decline.
- Polio third dose: 153/195 (78.5%) recorded a decline.

Median country-level changes were:

- DTP3: −2.70%
- MCV1: −2.57%
- Polio third dose: −2.76%

Paired Wilcoxon signed-rank tests showed strong evidence of differences between 2019 and 2020 for all three vaccines after false-discovery-rate adjustment.

---

## The decline continued into 2021

Summed vaccination counts across entities were:

| Vaccine | 2019 → 2020 | 2020 → 2021 | 2019 → 2021 |
|---|---:|---:|---:|
| DTP3 | −5.05% | −3.66% | −8.53% |
| MCV1 | −4.36% | −3.88% | −8.07% |
| Polio third dose | −5.73% | −3.72% | −9.23% |

Rather than showing a improvement after 2020, all three core vaccine series remained below their 2019 levels in 2021.

---

## Recovery was limited among countries affected in 2020

Those that experienced a decline in 2020:

### DTP3

- 73.1% continued to decline in 2021.
- 17.2% partially recovered.
- 9.7% returned to or exceeded their 2019 level.

### MCV1

- 72.2% continued to decline.
- 19.4% partially recovered.
- 8.3% returned to their 2019 level.

### Polio third dose

- 72.5% continued to decline.
- 16.3% partially recovered.
- 11.1% returned to their 2019 level.

These findings suggest that the fall observed during 2020 was geographically widespread and, for most affected entities, had not reversed by 2021.

---

## Country-level heterogeneity

The global pattern concealed substantial variation.

For DTP3, the largest absolute declines between 2019 and 2021 were:

- China: approximately 3.45 million fewer vaccinated one-year-olds
- India: approximately 2.04 million fewer
- Indonesia: approximately 852,000 fewer
- Myanmar: approximately 483,000 fewer

Other countries showed recovery.

Fourteen entities that experienced a DTP3 decline during 2020 had returned to or exceeded their 2019 count by 2021, illustrating why examining both global totals and individual country trajectories is important.

---

## Data

- 10,668 observations
- 254 entities
- 1980–2021
- eight childhood vaccination measures

Vaccines available include:

- DTP3
- Polio third dose
- MCV1
- HepB3
- Hib3
- Rubella-containing vaccine
- Rotavirus vaccine
- BCG

The dataset reports the number of one-year-olds vaccinated, rather than a directly comparable vaccination coverage percentage.

Regional, income-group and global aggregate observations were excluded from the main country-level analysis.

---

## Missing-data assessment

Data availability differed substantially between vaccines.

Historical missingness was particularly high for vaccines introduced or reported later in the study period such as rotavirus, Hib3, rubella and HepB3.

For the primary 2019–2021 analysis:

- DTP3 had 195 observations in each year.
- Polio third dose had 195 observations in each year.
- MCV1 had 195 observations in each year.

These three vaccines were  selected as the primary measures for the pandemic-period analysis.

Missing vaccination values were not imputed.

---

## Analytical workflow

### 01 — Data audit

`01_data_audit.R`

Assesses:

- dataset structure
- time coverage
- duplicate observations
- missingness
- aggregate entities
- vaccine-specific completeness during 2019–2021

### 02 — Data cleaning

`02_data_cleaning.R`

- standardises variable names
- removes aggregate entities
- creates country-level datasets
- reshapes vaccination data from wide to long format

### 03 — Vaccination trends

`03_vaccination_trends.R`

Examines:

- long-term vaccination counts
- changes in reporting availability
- DTP3, MCV1 and polio trends from 2015–2021
- vaccination indices using 2019 as the baseline

### 04 — Country-level disruption

`04_country_level_disruption.R`

Quantifies:

- proportion of entities experiencing declines
- median percentage changes
- distribution of country-level effects
- paired Wilcoxon signed-rank tests

### 05 — Recovery analysis

`05_recovery_analysis.R`

Classifies entities as:

- recovered to 2019 level
- partially recovered
- continued decline

A recovery index was calculated:

**Recovery index = (2021 − 2020) / (2019 − 2020)**

- 1 = full recovery of the 2020 decline
- 0–1 = partial recovery
- <0 = continued decline
- >1 = recovery beyond the 2019 level

### 06 — Country patterns

`06_country_patterns.R`

Examines:

- largest absolute declines
- largest proportional declines
- strongest recoveries
- contrasting country trajectories

---

## Epidemiological considerations

Several limitations are important when interpreting these results.

First, the dataset contains numbers vaccinated rather than vaccination coverage rates. Changes in the number vaccinated may therefore reflect changes in both vaccination programme performance and the size of the underlying birth cohort.

Second, missingness differs substantially between vaccines and years. Analyses were therefore restricted to observed values, with complete paired observations required for longitudinal comparisons.

Third, these are ecological country-level data. The results cannot be interpreted as individual-level associations.

Finally, changes occurring during 2020 and 2021 should not automatically be attributed causally to the COVID-19 pandemic. Other demographic, health-system, political and surveillance changes may also have contributed.

---

## Tools

Analysis conducted in R using:

- `tidyverse`
- `ggplot2`
- `dplyr`
- `tidyr`
- `readr`
- `here`
- `scales`

---

## Reproducibility

The raw source dataset is retained unchanged in:

```text
data/raw/
```

Cleaned datasets are generated programmatically and written to:

```text
data/processed/
```

All tables and figures can be reproduced by running the R scripts sequentially from `01` through `06`.

