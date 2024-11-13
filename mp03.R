library(tidyverse)
#Data Ingestion
#Task 1
get_district_files <- function(x, y){
  base_url <- "https://cdmaps.polisci.ucla.edu/shp/"
  for(num in x:y) {
    if (num < 10) {
      ext <- paste0("districts00", num, ".zip")
    }
    else if (num < 100) {
      ext <- paste0("districts0", num, ".zip")
    }
    else {
      ext <- paste0("districts", num, ".zip")
    }
    if (!file.exists(ext)){
      url <- paste0(base_url, ext)
      download.file(url, 
                    destfile = ext)
    }
  }
}
get_district_files(95, 114)

#Task 2

get_tiger_files <- function(x, y){
  base_url <- "https://www2.census.gov/geo/tiger/"
  for(year in x:y) {
    if (year %in% 2014:2015) {
      ext <- paste0("TIGER", year, "/CD/", "tl_", year, "_us_cd114.zip")
    }
    else if (year %in% 2016:2017) {
      ext <- paste0("TIGER", year, "/CD/", "tl_", year, "_us_cd115.zip")
    }
    else if (year %in% 2018:2022) {
      ext <- paste0("TIGER", year, "/CD/", "tl_", year, "_us_cd116.zip")
    }
    if (!file.exists(paste0(year, "congressDistrict.zip"))){
      url <- paste0(base_url, ext)
      download.file(url, 
                    destfile = paste0(year, "congressDistrict.zip"))
    }
  }
}

get_tiger_files(2014, 2022)

#Task 3
house <- read.csv("1976-2022-house.csv")
state_districts <- house |> group_by(year, state) |> 
  summarise(num_districts = max(district))
state_districts <- state_districts |> 
  mutate(num_districts = ifelse(num_districts == 0,1,num_districts))
state_districts <- state_districts |> 
  filter(year == 1976 | year == 2022) |> 
  group_by(state) |> 
  mutate(diff = diff(num_districts))
library(forcats)
state_districts |> 
  filter(year == 2022) |> 
  mutate(Color = ifelse(diff < 0, "red", "green")) |> 
  ggplot(aes(diff, fct_reorder(state, diff), fill = Color)) +
  geom_col() +
  labs(x = "Change in Congress Seats from 1976-2022", y = "State") +
  theme_minimal() +
  scale_fill_identity(guide = FALSE)
#Texas - New York
house_max <- house |> 
  group_by(candidate, GEOID, year) |> 
  slice(which.max(candidatevotes)) |> 
  group_by(GEOID, year) |> 
  slice(which.max(candidatevotes)) |> 
  select(candidate, GEOID, year, candidatevotes)
house_winner <- house |> 
  group_by(candidate, GEOID, year) |> 
  summarise(candidatevotes = sum(candidatevotes)) |> 
  group_by(GEOID, year) |> 
  slice(which.max(candidatevotes))
#Discrepancies
unmatched <- anti_join(house_max, house_winner, by = c("candidate", "GEOID", "year"))
#36 is NY
president <- read.csv("1976-2020-president.csv")
house_party_votes <- house |> 
  group_by(state, year, party) |> 
  summarise(housevotes = sum(candidatevotes)) |> 
  rename(party_detailed = party)
merged <- merge(president, house_party_votes, by = c("state", "year", "party_detailed"))
merged <- merged |> 
  mutate(diff = candidatevotes - housevotes) 
merged |> 
  arrange(desc(diff))
merged |> 
  group_by(year) |> 
  summarise(diff = sum(diff)) |> 
  ggplot(aes(year, diff)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "red") +
  theme_minimal()
#Task 4