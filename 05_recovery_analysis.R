# 05_recovery_analysis.R
#
# Project: Vaccine Disruption and Recovery
#
# Countries classification:
# - Recovered to 2019 level
# - Partial recovery
# - Continued decline
#
# Recovery index:
#
# (2021 - 2020) / (2019 - 2020)
#
# Interpretation:
# 1   = full recovery to the 2019 level
# 0-1 = partial recovery
# <0  = continued decline
# >1  = recovery beyond the 2019 level



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


# 4. Convert to wide format 

country_wide <- core_country_data |>
  pivot_wider(
    names_from = year,
    values_from = vaccinated,
    names_prefix = "year_"
  )


# 5. Restrict to complete three-year observations 

country_complete <- country_wide |>
  filter(
    !is.na(year_2019),
    !is.na(year_2020),
    !is.na(year_2021)
  )


# 6. Identify countries that declined in 2020

recovery_data <- country_complete |>
  mutate(
    
    disrupted_2020 =
      year_2020 < year_2019,
    
    recovery_index =
      if_else(
        disrupted_2020 &
          (year_2019 - year_2020) > 0,
        
        (year_2021 - year_2020) /
          (year_2019 - year_2020),
        
        NA_real_
      ),
    
    recovery_status =
      case_when(
        
        !disrupted_2020 ~
          "No 2020 decline",
        
        year_2021 >= year_2019 ~
          "Recovered to 2019 level",
        
        year_2021 > year_2020 &
          year_2021 < year_2019 ~
          "Partial recovery",
        
        year_2021 <= year_2020 ~
          "Continued decline",
        
        TRUE ~ NA_character_
      )
  )


# 7. Inspect recovery data 

glimpse(
  recovery_data
)


# 8. Summarise recovery among countries that declined in 2020 

recovery_summary <- recovery_data |>
  filter(
    disrupted_2020
  ) |>
  count(
    vaccine,
    recovery_status,
    name = "n_countries"
  ) |>
  group_by(
    vaccine
  ) |>
  mutate(
    percent =
      100 *
      n_countries /
      sum(n_countries)
  ) |>
  ungroup()


print(
  recovery_summary,
  n = Inf
)


# 9. Save recovery summary 

write_csv(
  recovery_summary,
  here(
    "tables",
    "recovery_summary.csv"
  )
)


write_csv(
  recovery_data,
  here(
    "tables",
    "country_recovery_data.csv"
  )
)



# ANALYSIS 1: Recovery status by vaccine



# 10. Order recovery categories 

recovery_summary <- recovery_summary |>
  mutate(
    recovery_status = factor(
      recovery_status,
      levels = c(
        "Continued decline",
        "Partial recovery",
        "Recovered to 2019 level"
      )
    )
  )


# 11. Plot recovery status

plot_recovery <- recovery_summary |>
  ggplot(
    aes(
      x = vaccine,
      y = percent,
      fill = recovery_status
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
    title = "Recovery by 2021 among countries with a 2020 decline",
    subtitle = "Countries classified relative to their own 2019 vaccination count",
    x = NULL,
    y = "Countries",
    fill = "2021 status"
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    legend.position = "bottom"
  )


plot_recovery


ggsave(
  filename = here(
    "plots",
    "07_recovery_status_2021.png"
  ),
  plot = plot_recovery,
  width = 9,
  height = 6,
  dpi = 300
)


# ANALYSIS 2: Recovery index



# 12. Summarise recovery index 

recovery_index_summary <- recovery_data |>
  filter(
    disrupted_2020,
    !is.na(recovery_index),
    is.finite(recovery_index)
  ) |>
  group_by(
    vaccine
  ) |>
  summarise(
    
    n_countries = n(),
    
    median_recovery_index =
      median(
        recovery_index,
        na.rm = TRUE
      ),
    
    q1 =
      quantile(
        recovery_index,
        0.25,
        na.rm = TRUE
      ),
    
    q3 =
      quantile(
        recovery_index,
        0.75,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )


print(
  recovery_index_summary
)


write_csv(
  recovery_index_summary,
  here(
    "tables",
    "recovery_index_summary.csv"
  )
)


# 13. Plot recovery-index distribution 

plot_recovery_index <- recovery_data |>
  filter(
    disrupted_2020,
    !is.na(recovery_index),
    is.finite(recovery_index)
  ) |>
  ggplot(
    aes(
      x = recovery_index
    )
  ) +
  geom_histogram(
    bins = 35
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed"
  ) +
  facet_wrap(
    ~ vaccine,
    scales = "free_y"
  ) +
  coord_cartesian(
    xlim = c(
      -2,
      3
    )
  ) +
  labs(
    title = "Recovery index after the 2020 vaccination decline",
    subtitle = "0 = no recovery; 1 = return to the 2019 vaccination count",
    x = "Recovery index",
    y = "Countries"
  ) +
  theme_minimal(
    base_size = 12
  )


plot_recovery_index


ggsave(
  filename = here(
    "plots",
    "08_recovery_index_distribution.png"
  ),
  plot = plot_recovery_index,
  width = 10,
  height = 6,
  dpi = 300
)


# ANALYSIS 3: Countries with continued decline


# 14. Identify countries continuing to decline 

continued_decline <- recovery_data |>
  filter(
    recovery_status == "Continued decline"
  ) |>
  mutate(
    
    percent_change_2019_2021 =
      if_else(
        year_2019 > 0,
        
        100 *
          (year_2021 - year_2019) /
          year_2019,
        
        NA_real_
      )
  ) |>
  arrange(
    vaccine,
    percent_change_2019_2021
  )


print(
  continued_decline,
  n = 30
)


write_csv(
  continued_decline,
  here(
    "tables",
    "countries_with_continued_decline.csv"
  )
)


# ANALYSIS 4: Countries recovering beyond 2019 levels



recovered_beyond_2019 <- recovery_data |>
  filter(
    disrupted_2020,
    year_2021 > year_2019
  ) |>
  mutate(
    
    percent_above_2019 =
      if_else(
        year_2019 > 0,
        
        100 *
          (year_2021 - year_2019) /
          year_2019,
        
        NA_real_
      )
  ) |>
  arrange(
    vaccine,
    desc(
      percent_above_2019
    )
  )


print(
  recovered_beyond_2019,
  n = 30
)


write_csv(
  recovered_beyond_2019,
  here(
    "tables",
    "countries_recovered_beyond_2019.csv"
  )
)



message(
  "Recovery analysis complete."
)