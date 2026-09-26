# 04_country_level_disruption.R
#
# Project: Vaccine Disruption and Recovery
#
# Purpose: Quantify country-level changes in DTP3, polio third dose and MCV1 vaccination counts between 2019 and 2020.


# 1. Load packages 

library(tidyverse)
library(here)
library(scales)


# 2. Import cleaned long-format data 

vaccination_long <- read_csv(
  here(
    "data",
    "processed",
    "vaccination_long.csv"
  ),
  show_col_types = FALSE
)


# 3. Select core vaccines and pandemic years 

core_country_data <- vaccination_long |>
  filter(
    vaccine %in% c(
      "DTP3",
      "Polio 3",
      "MCV1"
    ),
    year %in% c(
      2019,
      2020,
      2021
    )
  ) |>
  select(
    entity,
    code,
    year,
    vaccine,
    vaccinated
  )


# 4. Convert years into separate columns 

country_wide <- core_country_data |>
  pivot_wider(
    names_from = year,
    values_from = vaccinated,
    names_prefix = "year_"
  )


glimpse(country_wide)


# 5. Require complete observations across all three years

country_complete <- country_wide |>
  filter(
    !is.na(year_2019),
    !is.na(year_2020),
    !is.na(year_2021)
  )


# Check number of paired entities

country_complete |>
  count(vaccine)


# 6. Calculate country-level changes 

country_change <- country_complete |>
  mutate(
    
    absolute_change_2019_2020 =
      year_2020 - year_2019,
    
    percent_change_2019_2020 =
      if_else(
        year_2019 > 0,
        100 *
          (year_2020 - year_2019) /
          year_2019,
        NA_real_
      ),
    
    absolute_change_2020_2021 =
      year_2021 - year_2020,
    
    percent_change_2020_2021 =
      if_else(
        year_2020 > 0,
        100 *
          (year_2021 - year_2020) /
          year_2020,
        NA_real_
      ),
    
    declined_2020 =
      year_2020 < year_2019
  )


# 7. Summarise how many countries declined 

decline_summary <- country_change |>
  group_by(
    vaccine
  ) |>
  summarise(
    
    n_countries = n(),
    
    n_declined =
      sum(
        declined_2020,
        na.rm = TRUE
      ),
    
    percent_declined =
      100 *
      mean(
        declined_2020,
        na.rm = TRUE
      ),
    
    median_percent_change =
      median(
        percent_change_2019_2020,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )


print(
  decline_summary
)


# 8. Save summary

write_csv(
  decline_summary,
  here(
    "tables",
    "country_decline_summary_2019_2020.csv"
  )
)


write_csv(
  country_change,
  here(
    "tables",
    "country_level_changes.csv"
  )
)

# ANALYSIS 1: Percentage of countries experiencing a decline



# 9. Plot proportion declining 

plot_decline <- decline_summary |>
  ggplot(
    aes(
      x = reorder(
        vaccine,
        percent_declined
      ),
      y = percent_declined
    )
  ) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(
    limits = c(
      0,
      100
    ),
    labels = label_percent(
      scale = 1
    )
  ) +
  labs(
    title = "Most countries recorded lower vaccination counts in 2020",
    subtitle = "Paired country comparison with 2019",
    x = NULL,
    y = "Countries recording a decline"
  ) +
  theme_minimal(
    base_size = 12
  )


plot_decline


ggsave(
  filename = here(
    "plots",
    "04_countries_with_2020_decline.png"
  ),
  plot = plot_decline,
  width = 8,
  height = 5,
  dpi = 300
)


# ANALYSIS 2: Distribution of country-level percentage changes



# 10. Plot distributions 

plot_distribution <- country_change |>
  filter(
    !is.na(
      percent_change_2019_2020
    ),
    is.finite(
      percent_change_2019_2020
    )
  ) |>
  ggplot(
    aes(
      x = percent_change_2019_2020
    )
  ) +
  geom_histogram(
    bins = 35
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  facet_wrap(
    ~ vaccine,
    scales = "free_y"
  ) +
  labs(
    title = "Country-level changes in vaccination during 2020",
    subtitle = "Percentage change from 2019 to 2020",
    x = "Change in vaccinated one-year-olds (%)",
    y = "Countries"
  ) +
  theme_minimal(
    base_size = 12
  )


plot_distribution


ggsave(
  filename = here(
    "plots",
    "05_country_change_distribution.png"
  ),
  plot = plot_distribution,
  width = 10,
  height = 6,
  dpi = 300
)


# ANALYSIS 3: 2019 vs 2020 comparison



# 11. Scatterplot 

plot_scatter <- country_change |>
  filter(
    year_2019 > 0,
    year_2020 > 0
  ) |>
  ggplot(
    aes(
      x = year_2019,
      y = year_2020
    )
  ) +
  geom_point(
    alpha = 0.6
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  scale_x_log10(
    labels = label_number(
      scale_cut = cut_short_scale()
    )
  ) +
  scale_y_log10(
    labels = label_number(
      scale_cut = cut_short_scale()
    )
  ) +
  facet_wrap(
    ~ vaccine
  ) +
  labs(
    title = "Vaccination counts in 2019 compared with 2020",
    subtitle = "Points below the diagonal represent lower vaccination counts in 2020",
    x = "Vaccinated in 2019",
    y = "Vaccinated in 2020"
  ) +
  theme_minimal(
    base_size = 12
  )


plot_scatter


ggsave(
  filename = here(
    "plots",
    "06_2019_vs_2020_country_comparison.png"
  ),
  plot = plot_scatter,
  width = 10,
  height = 6,
  dpi = 300
)

# ANALYSIS 4: Paired statistical comparison



# 12. Wilcoxon signed-rank test 


wilcoxon_results <- country_complete |>
  group_by(
    vaccine
  ) |>
  summarise(
    
    n_pairs = n(),
    
    p_value =
      wilcox.test(
        year_2020,
        year_2019,
        paired = TRUE,
        exact = FALSE
      )$p.value,
    
    .groups = "drop"
  ) |>
  mutate(
    p_value_fdr =
      p.adjust(
        p_value,
        method = "BH"
      )
  )


print(
  wilcoxon_results
)


write_csv(
  wilcoxon_results,
  here(
    "tables",
    "wilcoxon_2019_2020.csv"
  )
)


message(
  "Country-level disruption analysis complete."
)