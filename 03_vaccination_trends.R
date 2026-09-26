# 03_vaccination_trends.R
#
# Project: Vaccine Disruption and Recovery
#
# The source data contain numbers of vaccinated one-year-olds rather than vaccination coverage percentages.



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


# 3. Inspect processed data 

glimpse(vaccination_long)


# 4. Calculate annual totals and reporting availability 

annual_vaccine_summary <- vaccination_long |>
  group_by(
    year,
    vaccine
  ) |>
  summarise(
    n_reporting = sum(
      !is.na(vaccinated)
    ),
    
    total_vaccinated = if (
      all(is.na(vaccinated))
    ) {
      NA_real_
    } else {
      sum(
        vaccinated,
        na.rm = TRUE
      )
    },
    
    .groups = "drop"
  )


print(
  annual_vaccine_summary,
  n = 20
)


# 5. Save annual summary

write_csv(
  annual_vaccine_summary,
  here(
    "tables",
    "annual_vaccine_summary.csv"
  )
)


# ANALYSIS 1: Long-term vaccination trends



# 6. Plot long-term vaccination counts

plot_long_term <- annual_vaccine_summary |>
  filter(
    !is.na(total_vaccinated)
  ) |>
  ggplot(
    aes(
      x = year,
      y = total_vaccinated,
      colour = vaccine
    )
  ) +
  geom_line(
    linewidth = 0.9
  ) +
  scale_y_continuous(
    labels = label_number(
      scale = 1e-6,
      suffix = "M"
    )
  ) +
  labs(
    title = "Global childhood vaccination counts, 1980–2021",
    subtitle = "Annual totals across country and territory records with available data",
    x = "Year",
    y = "One-year-olds vaccinated",
    colour = "Vaccine",
    caption = "Counts represent vaccinated one-year-olds, not vaccination coverage."
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    legend.position = "bottom"
  )


plot_long_term


# 7. Save long-term trend plot 

ggsave(
  filename = here(
    "plots",
    "01_long_term_vaccination_trends.png"
  ),
  plot = plot_long_term,
  width = 11,
  height = 7,
  dpi = 300
)


# ANALYSIS 2: Reporting availability



# 8. Plot number of entities reporting each vaccine 

plot_reporting <- annual_vaccine_summary |>
  ggplot(
    aes(
      x = year,
      y = n_reporting,
      colour = vaccine
    )
  ) +
  geom_line(
    linewidth = 0.9
  ) +
  labs(
    title = "Availability of vaccination observations over time",
    subtitle = "Number of country and territory entities with observed vaccination counts",
    x = "Year",
    y = "Entities with data",
    colour = "Vaccine"
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    legend.position = "bottom"
  )


plot_reporting


# 9. Save reporting plot

ggsave(
  filename = here(
    "plots",
    "02_vaccine_reporting_over_time.png"
  ),
  plot = plot_reporting,
  width = 11,
  height = 7,
  dpi = 300
)


# ANALYSIS 3: Core vaccine trends around the COVID-19 pandemic



# 10. Select the three vaccines with complete 2019-2021 country data 

core_vaccines <- annual_vaccine_summary |>
  filter(
    vaccine %in% c(
      "DTP3",
      "Polio 3",
      "MCV1"
    ),
    year >= 2015,
    year <= 2021
  )


print(
  core_vaccines,
  n = Inf
)


# 11. Create an index where 2019 = 100 

core_vaccine_index <- core_vaccines |>
  group_by(
    vaccine
  ) |>
  mutate(
    baseline_2019 =
      total_vaccinated[
        year == 2019
      ],
    
    index_2019 =
      100 *
      total_vaccinated /
      baseline_2019
  ) |>
  ungroup()


print(
  core_vaccine_index,
  n = Inf
)


# 12. Save indexed data 

write_csv(
  core_vaccine_index,
  here(
    "tables",
    "core_vaccine_index_2015_2021.csv"
  )
)


# 13. Plot pandemic-era indexed trends 

plot_core_index <- core_vaccine_index |>
  ggplot(
    aes(
      x = year,
      y = index_2019,
      colour = vaccine
    )
  ) +
  geom_hline(
    yintercept = 100,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = 2020,
    linetype = "dotted"
  ) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 2
  ) +
  scale_x_continuous(
    breaks = 2015:2021
  ) +
  labs(
    title = "Childhood vaccination counts declined during 2020–2021",
    subtitle = "DTP3, polio third dose and MCV1 indexed to 2019 = 100",
    x = "Year",
    y = "Vaccination index (2019 = 100)",
    colour = "Vaccine",
    caption = "Index based on summed vaccination counts among records with available data."
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    legend.position = "bottom"
  )


plot_core_index


# 14. Save indexed pandemic plot 

ggsave(
  filename = here(
    "plots",
    "03_core_vaccines_index_2015_2021.png"
  ),
  plot = plot_core_index,
  width = 10,
  height = 6,
  dpi = 300
)



# ANALYSIS 4: Direct comparison of 2019, 2020 and 2021

# 15. Calculate percentage change 

pandemic_change_summary <- core_vaccines |>
  filter(
    year %in% c(
      2019,
      2020,
      2021
    )
  ) |>
  select(
    vaccine,
    year,
    total_vaccinated
  ) |>
  pivot_wider(
    names_from = year,
    values_from = total_vaccinated,
    names_prefix = "year_"
  ) |>
  mutate(
    change_2019_2020 =
      100 *
      (year_2020 - year_2019) /
      year_2019,
    
    change_2020_2021 =
      100 *
      (year_2021 - year_2020) /
      year_2020,
    
    change_2019_2021 =
      100 *
      (year_2021 - year_2019) /
      year_2019
  )


print(
  pandemic_change_summary
)


# 16. Save pandemic change summary 
write_csv(
  pandemic_change_summary,
  here(
    "tables",
    "pandemic_change_summary.csv"
  )
)




message(
  "Vaccination trend analysis complete."
)