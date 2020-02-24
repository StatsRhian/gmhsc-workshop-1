library("readxl")
library("tidyverse")

# Read in sheet 6 of the conception data
# Under 18 conceptions (numbers and rates) and outcome, 1998 to 2017
df_raw = read_excel("data/raw/conceptionstatisticstables2017.xls",
                    sheet = "Table 6", range = "A7:FZ459")

# Remove entirely blank rows
df =
  df_raw %>% 
  janitor::remove_empty("rows")

# Rename the first two columns to have sensible names
df = 
  df %>%
  rename("la_code" = "...1") %>%
  rename("la_Name" = "...2") 

# There are some columns which only contain NA or "u".
# Here "u" stands for "unreliable data"
# However for our purposes we're going to ignore these columns
# 
# 1. Create a pattern to identify them (starts_with("..."))
# 2. Check that the pattern successfully only extracts the unwanted columns
# 3. Remove the unwanted columns

# Create a summary table of all unwanted columns, and the unique values in that column

df %>% 
  select(starts_with("...")) %>%
  unlist() %>%
  unique()

df = 
  df %>%
  select(-starts_with("..."))

# Create new names to represent the statistics
stats_names =  c("number_conceptions", "conceptions_per_1000", "maternity_per_1000", "abortion_per_1000", "percent_conceptions_abortion")

# Create a vector of new columns names using the statistics and the years
# I looked up the years in advance and saw this data set spans 2017 - 1998 with exactly 5 statistics for each year
# I've used purrr:cross_df() here but you could also do this with a for loop
col_names = 
list(stats = stats_names,
     years = seq(2017, 1998)) %>% 
  cross_df() %>% 
  unite("year_stat", sep = "_year_") %>%
  pull()
  
# Rename the data set
names(df) = c("la_code", "la_name", col_names)

# Keep only GM data
la_codes = c("E08000001", "E08000002", "E08000003", "E08000004", "E08000005",
               "E08000006", "E08000007", "E08000008", "E08000009", "E08000010",
               "E11000001", "E12000002", "E92000001")
df = 
  df %>%
  filter(la_code %in% LA_codes)

# Convert the wide data to long data
# names_pattern will use the information in the column headers to become new variables
df = 
df %>% pivot_longer(
  cols = `number_conceptions_year_2017`:`percent_conceptions_abortion_year_1998`,
  names_to = c("stat", "year"), 
  names_pattern = "(.*)_year_(.*)",
  values_to = "value"
)

write_csv(x = df, path = "data/processed/conception_table_6.csv")
