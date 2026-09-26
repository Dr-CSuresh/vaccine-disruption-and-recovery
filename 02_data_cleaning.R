# 02_data_cleaning.R
#
# Project: Vaccine Disruption and Recovery
#
# Purpose:
# Clean variable names, remove aggregate entities, and create, analysis-ready country-level vaccination datasets.



# 1. Load packages 

library(tidyverse)
library(here)


# 2. Import raw data 

vaccination_raw <- read_csv(
  here(
    "data",
    "raw",
    "2- the-worlds-number-of-vaccinated-one-year-olds.csv"
  ),
  show_col_types = FALSE
)


# 3. Rename variables 

vaccination <- vaccination_raw |>
  transmute(
    entity = Entity,
    code = Code,
    year = Year,
    
    hepb3 =
      `Number of one-year-olds vaccinated with HepB3`,
    
    dtp3 =
      `Number of one-year-olds vaccinated with DTP containing vaccine, 3rd dose`,
    
    polio3 =
      `Number of one-year-olds vaccinated with polio, 3rd dose`,
    
    population_age0 =
      `Population - Sex: all - Age: 0 - Variant: estimates`,
    
    mcv1 =
      `Number of one-year-olds vaccinated with measles-containing vaccine, 1st dose`,
    
    hib3 =
      `Number of one-year-olds vaccinated with Hib3`,
    
    rubella1 =
      `Number of one-year-olds vaccinated with rubella-containing vaccine, 1st dose`,
    
    rotavirus =
      `Number of one-year-olds vaccinated with rotavirus, last dose`,
    
    bcg =
      `Number of one-year-olds vaccinated with BCG`
  )


# 4. Restrict to country-level observations 

vaccination_countries <- vaccination |>
  filter(
    !is.na(code),
    code != "OWID_WRL"
  )


# 5. Check cleaned country-level dataset

vaccination_countries |>
  summarise(
    n_rows = n(),
    n_entities = n_distinct(entity),
    first_year = min(year),
    last_year = max(year)
  )


# 6. Convert vaccine columns to long format 

vaccination_long <- vaccination_countries |>
  pivot_longer(
    cols = c(
      hepb3,
      dtp3,
      polio3,
      mcv1,
      hib3,
      rubella1,
      rotavirus,
      bcg
    ),
    names_to = "vaccine",
    values_to = "vaccinated"
  )


# 7. Give vaccines readable names 

vaccination_long <- vaccination_long |>
  mutate(
    vaccine = recode(
      vaccine,
      hepb3 = "HepB3",
      dtp3 = "DTP3",
      polio3 = "Polio 3",
      mcv1 = "MCV1",
      hib3 = "Hib3",
      rubella1 = "Rubella 1",
      rotavirus = "Rotavirus",
      bcg = "BCG"
    )
  )


# 8. Inspect long-format dataset 

glimpse(vaccination_long)


# 9. Create processed-data directory if needed 

if (!dir.exists(
  here("data", "processed")
)) {
  dir.create(
    here("data", "processed"),
    recursive = TRUE
  )
}


# 10. Save processed datasets 

write_csv(
  vaccination_countries,
  here(
    "data",
    "processed",
    "vaccination_countries.csv"
  )
)


write_csv(
  vaccination_long,
  here(
    "data",
    "processed",
    "vaccination_long.csv"
  )
)


# 11. Confirmation 

message("Data cleaning complete.")