library(readxl)
library("tidyverse")

# Read in teen pregnancy data
df_raw = read_excel("data/conceptionstatisticstables2017.xls", sheet = "Table 6", range = "A7:FZ459")


# Remove entirely blank rows
df = 
  df_raw %>% 
  janitor::remove_empty()

df = df[,-which(is.na(df[1,]))]

stats_names =  c("number_conceptions", "conceptions_per_100", "maternity_per_100", "abortion_per_100", "percent_conceptions_abortion")

col_names = 
list(stats = stats_names,
     years = seq(2017, 1998)) %>% 
  cross_df() %>%
  unite("year_stat", sep = "_year_") %>%
  pull()
  

names(df) = c("LSOA", "Area", col_names)

# Keep only GM data
lsoa_codes = c("E08000001", "E08000002", "E08000003", "E08000004", "E08000005",
               "E08000006", "E08000007", "E08000008", "E08000009", "E08000010",
               "E11000001", "E12000002", "E92000001")
df = 
  df %>%
  filter(LSOA %in% lsoa_codes)



df = 
df %>% pivot_longer(
  cols = `number_conceptions_year_2017`:`percent_conceptions_abortion_year_1998`,
  names_to = c("stat", "year"), 
  names_pattern = "(.*)_year_(.*)",
  values_to = "value"
)



df %>%
  filter(stat != "number_conceptions") %>%
  ggplot(aes(x = year, y = value)) + 
  geom_point(aes(col = Area)) + 
  geom_line(aes(group = Area)) + 
  facet_wrap(~stat)



df %>%
  filter(stat == "conceptions_per_100") %>%
  filter(Area != "NORTH WEST") %>%
  filter(Area != "ENGLAND") %>%
  filter(year == 2017) %>%
  ggplot(aes(x =  reorder(Area, value), y = value)) + 
  geom_col() + 
  coord_flip() + 
  ggtitle("Teenage conceptions per 100 in the North West, split by Area") + 
  theme_minimal() + 
  ylab("") +
  xlab("")


geo = st_read("data/Local_Authority_Districts_December_2019_Boundaries_UK_BFC/Local_Authority_Districts_December_2019_Boundaries_UK_BFC.shp")

geo_gm = 
  geo %>%
  filter(lad19cd %in% lsoa_codes)

teen_17 = 
  df %>%
  filter(year == 2017) %>%
  filter(stat == "number_conceptions")

teen_17_geo = 
  left_join(geo_gm, teen_17, by = c("lad19cd" = "LSOA")) %>%
  st_transform(27700)

library(leaflet)
leaflet(teen_17_geo) %>%
  addTiles() %>%
  addPolygons()
