# 01_data_audit.R
#
# Project: Vaccine Disruption and Recovery
#
# Purpose:
# Audit the structure, completeness, time coverage, duplicate records, aggregate entities, and missing-data patterns in the childhood vaccination dataset before analysis.



# 1. Load packages 

library(tidyverse)
library(here)


# 2. Confirm project location 

here()


# 3. Check raw data file 

list.files(
  here("data", "raw")
)


# 4. Import vaccination data 

vaccination_raw <- read_csv(
  here(
    "data",
    "raw",
    "2- the-worlds-number-of-vaccinated-one-year-olds.csv"
  ),
  show_col_types = FALSE
)


# 5. Inspect dataset structure 

glimpse(vaccination_raw)


# 6. Check dataset dimensions 

dim(vaccination_raw)


# 7. Check variable names 

names(vaccination_raw)


# 8. Summarise dataset coverage 

dataset_summary <- vaccination_raw |>
  summarise(
    n_rows = n(),
    n_columns = ncol(vaccination_raw),
    first_year = min(Year, na.rm = TRUE),
    last_year = max(Year, na.rm = TRUE),
    n_entities = n_distinct(Entity),
    n_codes = n_distinct(Code, na.rm = TRUE)
  )

dataset_summary


# 9. Check for duplicate entity-year observations 

duplicates <- vaccination_raw |>
  count(
    Entity,
    Year,
    name = "n"
  ) |>
  filter(n > 1)

duplicates


# 10. Assess overall missingness 

missingness <- vaccination_raw |>
  summarise(
    across(
      everything(),
      ~ sum(is.na(.x))
    )
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "n_missing"
  ) |>
  mutate(
    percent_missing = round(
      100 * n_missing / nrow(vaccination_raw),
      2
    )
  ) |>
  arrange(desc(percent_missing))

print(
  missingness,
  n = Inf
)


# 11. Identify aggregate and non-country entities 

aggregate_entities <- vaccination_raw |>
  filter(
    is.na(Code) |
      Code == "OWID_WRL"
  ) |>
  distinct(
    Entity,
    Code
  ) |>
  arrange(Entity)

print(
  aggregate_entities,
  n = Inf
)


# 12. Assess missingness by year 

missing_by_year <- vaccination_raw |>
  group_by(Year) |>
  summarise(
    across(
      everything(),
      ~ sum(is.na(.x))
    ),
    .groups = "drop"
  )

print(
  missing_by_year,
  n = Inf
)


# 13. Check vaccine completeness during 2019-2021 

pandemic_missingness <- vaccination_raw |>
  filter(
    Year %in% c(
      2019,
      2020,
      2021
    )
  ) |>
  group_by(Year) |>
  summarise(
    dtp3_missing =
      sum(
        is.na(
          `Number of one-year-olds vaccinated with DTP containing vaccine, 3rd dose`
        )
      ),
    
    polio3_missing =
      sum(
        is.na(
          `Number of one-year-olds vaccinated with polio, 3rd dose`
        )
      ),
    
    mcv1_missing =
      sum(
        is.na(
          `Number of one-year-olds vaccinated with measles-containing vaccine, 1st dose`
        )
      ),
    
    hepb3_missing =
      sum(
        is.na(
          `Number of one-year-olds vaccinated with HepB3`
        )
      ),
    
    hib3_missing =
      sum(
        is.na(
          `Number of one-year-olds vaccinated with Hib3`
        )
      ),
    
    rubella_missing =
      sum(
        is.na(
          `Number of one-year-olds vaccinated with rubella-containing vaccine, 1st dose`
        )
      ),
    
    rotavirus_missing =
      sum(
        is.na(
          `Number of one-year-olds vaccinated with rotavirus, last dose`
        )
      ),
    
    bcg_missing =
      sum(
        is.na(
          `Number of one-year-olds vaccinated with BCG`
        )
      ),
    
    .groups = "drop"
  )

pandemic_missingness


# 14. Count records by year 

records_by_year <- vaccination_raw |>
  count(
    Year,
    name = "n_records"
  ) |>
  arrange(Year)

print(
  records_by_year,
  n = Inf
)


# 15. Count years contributed by each entity 

records_by_entity <- vaccination_raw |>
  count(
    Entity,
    name = "n_years"
  ) |>
  arrange(desc(n_years))

print(
  records_by_entity,
  n = Inf
)


# 16. Save audit outputs

if (!dir.exists(
  here("tables")
)) {
  dir.create(
    here("tables"),
    recursive = TRUE
  )
}


write_csv(
  dataset_summary,
  here(
    "tables",
    "dataset_summary.csv"
  )
)


write_csv(
  duplicates,
  here(
    "tables",
    "duplicate_entity_years.csv"
  )
)


write_csv(
  missingness,
  here(
    "tables",
    "missingness_summary.csv"
  )
)


write_csv(
  aggregate_entities,
  here(
    "tables",
    "aggregate_entities.csv"
  )
)


write_csv(
  pandemic_missingness,
  here(
    "tables",
    "pandemic_missingness_2019_2021.csv"
  )
)


write_csv(
  records_by_year,
  here(
    "tables",
    "records_by_year.csv"
  )
)


write_csv(
  records_by_entity,
  here(
    "tables",
    "records_by_entity.csv"
  )
)


# 17. Confirmation

message("Data audit complete.")

# 18. Assess pandemic-period completeness among countries 

country_level_raw <- vaccination_raw |>
  filter(
    !is.na(Code),
    Code != "OWID_WRL"
  )


country_pandemic_completeness <- country_level_raw |>
  filter(
    Year %in% c(2019, 2020, 2021)
  ) |>
  group_by(Year) |>
  summarise(
    across(
      starts_with("Number of one-year-olds vaccinated"),
      ~ sum(!is.na(.x))
    ),
    .groups = "drop"
  )


print(
  country_pandemic_completeness,
  width = Inf
)


write_csv(
  country_pandemic_completeness,
  here(
    "tables",
    "country_pandemic_completeness_2019_2021.csv"
  )
)