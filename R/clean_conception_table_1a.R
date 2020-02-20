library("readxl")
library("tidyverse")

# Read in sheet 1a of the conception data
# Conceptions (numbers and rates) and outcome: age of woman at conception, 1990 to 2017
df_raw = read_excel("data/raw/conceptionstatisticstables2017.xls",
                    sheet = "Table 1a", range = "A7:AB36")

# Remove entirely blank rows
df =
  df_raw %>% 
  janitor::remove_empty("rows")

age_groups = c("all", "under 16", "under 18", "under 20", "20 - 24", "25 - 29", "30 - 34", "35 - 39", "40 and over")
stats_names = c("number_of_conceptions", "conception_rate_per_1000_in_age_group", "percent_conceptions_leading_to_abortion")

# I've used purrr:cross_df() here but you could also do this with a for loop
col_names = 
  list(stats = stats_names,
       age_groups = age_groups) %>% 
  cross_df() %>% 
  unite("stat_age", sep = "_age_") %>%
  pull()

names(df) = c("year", col_names)

# Convert the wide data to long data
# names_pattern will use the information in the column headers to become new variables
df = 
  df %>% pivot_longer(
    cols = -year,
    names_to = c("stat", "age_group"), 
    names_pattern = "(.*)_age_(.*)",
    values_to = "value"
  )

write_csv(x = df, path = "data/processed/conception_table_1a.csv")