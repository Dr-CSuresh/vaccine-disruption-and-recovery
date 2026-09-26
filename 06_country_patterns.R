# 06_country_patterns.R
#
# Project: Vaccine Disruption and Recovery



# 1. Load packages 

library(tidyverse)
library(here)
library(scales)


# 2. Import recovery dataset 

recovery_data <- read_csv(
  here(
    "tables",
    "country_recovery_data.csv"
  ),
  show_col_types = FALSE
)


# 3. Calculate overall 2019-2021 change 

country_patterns <- recovery_data |>
  mutate(
    
    absolute_change_2019_2021 =
      year_2021 - year_2019,
    
    percent_change_2019_2021 =
      if_else(
        year_2019 > 0,
        100 *
          (year_2021 - year_2019) /
          year_2019,
        NA_real_
      )
  )


# ANALYSIS 1: Largest absolute DTP3 declines



largest_absolute_declines <- country_patterns |>
  filter(
    vaccine == "DTP3"
  ) |>
  arrange(
    absolute_change_2019_2021
  ) |>
  slice_head(
    n = 15
  )


print(
  largest_absolute_declines,
  n = Inf
)


write_csv(
  largest_absolute_declines,
  here(
    "tables",
    "dtp3_largest_absolute_declines.csv"
  )
)


# 4. Plot largest absolute declines 

plot_absolute_declines <- largest_absolute_declines |>
  mutate(
    entity = fct_reorder(
      entity,
      absolute_change_2019_2021
    )
  ) |>
  ggplot(
    aes(
      x = entity,
      y = absolute_change_2019_2021
    )
  ) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(
    labels = label_number(
      scale_cut = cut_short_scale()
    )
  ) +
  labs(
    title = "Largest absolute DTP3 declines, 2019–2021",
    subtitle = "Change in the number of one-year-olds vaccinated",
    x = NULL,
    y = "Change in vaccinated one-year-olds"
  ) +
  theme_minimal(
    base_size = 12
  )


plot_absolute_declines


ggsave(
  here(
    "plots",
    "09_dtp3_largest_absolute_declines.png"
  ),
  plot_absolute_declines,
  width = 9,
  height = 7,
  dpi = 300
)


# ANALYSIS 2: Largest proportional declines



largest_proportional_declines <- country_patterns |>
  filter(
    vaccine == "DTP3",
    year_2019 >= 10000,
    !is.na(percent_change_2019_2021)
  ) |>
  arrange(
    percent_change_2019_2021
  ) |>
  slice_head(
    n = 15
  )


print(
  largest_proportional_declines,
  n = Inf
)


write_csv(
  largest_proportional_declines,
  here(
    "tables",
    "dtp3_largest_proportional_declines.csv"
  )
)


# 5. Plot proportional declines 

plot_proportional_declines <- largest_proportional_declines |>
  mutate(
    entity = fct_reorder(
      entity,
      percent_change_2019_2021
    )
  ) |>
  ggplot(
    aes(
      x = entity,
      y = percent_change_2019_2021
    )
  ) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(
    labels = label_percent(
      scale = 1
    )
  ) +
  labs(
    title = "Largest proportional DTP3 declines, 2019–2021",
    subtitle = "Restricted to countries with at least 10,000 vaccinated one-year-olds in 2019",
    x = NULL,
    y = "Change from 2019"
  ) +
  theme_minimal(
    base_size = 12
  )


plot_proportional_declines


ggsave(
  here(
    "plots",
    "10_dtp3_largest_proportional_declines.png"
  ),
  plot_proportional_declines,
  width = 9,
  height = 7,
  dpi = 300
)


# ANALYSIS 3: Strongest recoveries



strongest_recoveries <- country_patterns |>
  filter(
    disrupted_2020,
    recovery_status == "Recovered to 2019 level",
    vaccine == "DTP3"
  ) |>
  arrange(
    desc(
      percent_change_2019_2021
    )
  )


print(
  strongest_recoveries,
  n = Inf
)


write_csv(
  strongest_recoveries,
  here(
    "tables",
    "dtp3_recovered_countries.csv"
  )
)

# ANALYSIS 4: Example trajectories



trajectory_countries <- bind_rows(
  
  largest_proportional_declines |>
    slice_head(
      n = 5
    ),
  
  strongest_recoveries |>
    slice_head(
      n = 5
    )
  
) |>
  distinct(
    entity
  )


trajectory_data <- country_patterns |>
  filter(
    vaccine == "DTP3",
    entity %in% trajectory_countries$entity
  ) |>
  select(
    entity,
    year_2019,
    year_2020,
    year_2021
  ) |>
  pivot_longer(
    cols = starts_with("year_"),
    names_to = "year",
    values_to = "vaccinated"
  ) |>
  mutate(
    year = as.numeric(
      str_remove(
        year,
        "year_"
      )
    )
  )


plot_trajectories <- trajectory_data |>
  ggplot(
    aes(
      x = year,
      y = vaccinated,
      group = entity,
      colour = entity
    )
  ) +
  geom_line(
    linewidth = 0.9
  ) +
  geom_point(
    size = 2
  ) +
  scale_x_continuous(
    breaks = 2019:2021
  ) +
  scale_y_continuous(
    labels = label_number(
      scale_cut = cut_short_scale()
    )
  ) +
  labs(
    title = "Contrasting DTP3 vaccination trajectories",
    subtitle = "Selected countries with large declines or recovery by 2021",
    x = "Year",
    y = "One-year-olds vaccinated",
    colour = "Country"
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    legend.position = "bottom"
  )


plot_trajectories


ggsave(
  here(
    "plots",
    "11_dtp3_country_trajectories.png"
  ),
  plot_trajectories,
  width = 10,
  height = 7,
  dpi = 300
)



message(
  "Country pattern analysis complete."
)