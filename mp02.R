library(tidyverse)
library(ggplot2)
#Data
get_imdb_file <- function(fname){
  BASE_URL <- "https://datasets.imdbws.com/"
  fname_ext <- paste0(fname, ".tsv.gz")
  if(!file.exists(fname_ext)){
    FILE_URL <- paste0(BASE_URL, fname_ext)
    download.file(FILE_URL, 
                  destfile = fname_ext)
  }
  as.data.frame(readr::read_tsv(fname_ext, lazy=FALSE))
}

NAME_BASICS      <- get_imdb_file("name.basics")
TITLE_BASICS     <- get_imdb_file("title.basics")
TITLE_EPISODES   <- get_imdb_file("title.episode")
TITLE_RATINGS    <- get_imdb_file("title.ratings")
TITLE_CREW       <- get_imdb_file("title.crew")
TITLE_PRINCIPALS <- get_imdb_file("title.principals")

#Data Sub-Sampling
NAME_BASICS <- NAME_BASICS |> 
  filter(str_count(knownForTitles, ",") > 1)

TITLE_RATINGS |>
  ggplot(aes(x=numVotes)) + 
  geom_histogram(bins=30) +
  xlab("Number of IMDB Ratings") + 
  ylab("Number of Titles") + 
  ggtitle("Majority of IMDB Titles Have Less than 100 Ratings") + 
  theme_bw() + 
  scale_x_log10(label=scales::comma) + 
  scale_y_continuous(label=scales::comma)
TITLE_RATINGS |>
  pull(numVotes) |>
  quantile()
TITLE_RATINGS <- TITLE_RATINGS |>
  filter(numVotes >= 100)

TITLE_BASICS <- TITLE_BASICS |>
  semi_join(TITLE_RATINGS, 
            join_by(tconst == tconst))

TITLE_CREW <- TITLE_CREW |>
  semi_join(TITLE_RATINGS, 
            join_by(tconst == tconst))

TITLE_EPISODES_1 <- TITLE_EPISODES |>
  semi_join(TITLE_RATINGS, 
            join_by(tconst == tconst))
TITLE_EPISODES_2 <- TITLE_EPISODES |>
  semi_join(TITLE_RATINGS, 
            join_by(parentTconst == tconst))

TITLE_EPISODES <- bind_rows(TITLE_EPISODES_1,
                            TITLE_EPISODES_2) |>
  distinct()

TITLE_PRINCIPALS <- TITLE_PRINCIPALS |>
  semi_join(TITLE_RATINGS, join_by(tconst == tconst))


rm(TITLE_EPISODES_1)
rm(TITLE_EPISODES_2)

#Initial Exploration
glimpse(NAME_BASICS)
glimpse(TITLE_BASICS)
glimpse(TITLE_CREW)
glimpse(TITLE_EPISODES)
glimpse(TITLE_PRINCIPALS)
glimpse(TITLE_RATINGS)

NAME_BASICS <- NAME_BASICS |>
  mutate(birthYear = as.numeric(birthYear),
         deathYear = as.numeric(deathYear))

#Task1
TITLE_BASICS <- TITLE_BASICS |>
  mutate(isAdult = as.logical(isAdult),
         startYear = as.numeric(startYear),
         endYear = as.numeric(endYear),
         runtimeMinutes = as.numeric(runtimeMinutes))
TITLE_CREW <- TITLE_CREW |>
  mutate(writers = ifelse(str_equal(writers, "\\N"), NA, writers),
         directors = ifelse(str_equal(writers, "\\N"), NA, directors))
TITLE_EPISODES <- TITLE_EPISODES |> 
  mutate(seasonNumber = as.numeric(seasonNumber),
         episodeNumber = as.numeric(episodeNumber))
TITLE_PRINCIPALS <- TITLE_PRINCIPALS |>
  mutate(category = ifelse(str_equal(category, "\\N"), NA, category),
         job = ifelse(str_equal(job, "\\N"), NA, job),
         characters = ifelse(str_equal(characters, "\\N"), NA, characters))
#Task2
TITLE_BASICS |> 
  group_by(titleType) |> 
  summarise(n = n())
#132369 movies, 30043 TV series, 156967 TV episodes
NAME_BASICS |> 
  filter(is.na(deathYear), birthYear > 1917) |> 
  arrange(birthYear) |> 
  head()
#Brenda Miller
TITLE_RATINGS |> 
  filter(numVotes > 200000, averageRating == 10)
TITLE_BASICS |> 
  filter(tconst == "tt2301451")
#Ozymandias Breaking Bad
NAME_BASICS |> 
  filter(primaryName == "Mark Hamill")
TITLE_BASICS |> 
  filter(tconst %in% c("tt0076759","tt2527336","tt0080684","tt0086190"))
#Star Wars IV-V-VI-VIII
TvSeries <- TITLE_EPISODES |> 
  group_by(parentTconst) |> 
  summarise(count = n()) |> 
  filter(count > 12)
TITLE_RATINGS |> 
  filter(tconst %in% TvSeries$parentTconst) |> 
  group_by(tconst) |> 
  summarise(avgRating = mean(averageRating)) |> 
  arrange(desc(avgRating)) |> 
  head()
TITLE_BASICS |> 
  filter(tconst == "tt23028046")
#Jogandofodoacci
TITLE_BASICS |> 
  filter(primaryTitle == "Happy Days")
#tt0070992
Happy_Days <- TITLE_EPISODES |> 
  filter(parentTconst == "tt0070992") |> 
  arrange(seasonNumber, episodeNumber) |> 
  mutate(totalEpisodeNum = row_number())
Happy_Days <- inner_join(Happy_Days, TITLE_RATINGS, by = "tconst")
Happy_Days |> 
  ggplot(aes(totalEpisodeNum, averageRating)) +
  geom_point() +
  geom_smooth() +
  theme_minimal()
#Kind of True
#Logarithmic operator
#Range (3-15.8)
#Task 3
TITLE_RATINGS <- TITLE_RATINGS |> 
  mutate(success = averageRating * 1+log10(numVotes))
TITLE_RATINGS |> 
  arrange(desc(success)) |> 
  head()
TITLE_BASICS |> 
  filter(tconst %in% c("tt0903747","tt0111161","tt0944947","tt0068646", "tt0468569", "tt2301451"))
TITLE_RATINGS |> 
  filter(numVotes > 50000, averageRating < 5) |> 
  arrange(success)
#Christopher Nolan
NAME_BASICS |> 
  filter(primaryName == "Christopher Nolan")
TITLE_RATINGS |> 
  filter(tconst %in% c("tt6723592","tt0816692","tt1375666","tt0482571"))
TITLE_BASICS |> 
  filter(primaryTitle == "The Room")
TITLE_RATINGS |> 
  filter(tconst == "tt0368226")
TITLE_RATINGS |> 
  ggplot(aes(success)) + 
  stat_function(
    fun = dnorm,
    args = with(TITLE_RATINGS, c(mean = mean(success), sd = sd(success)))
  ) + theme_minimal()
#Threshold 10
#Task 4
titles <- inner_join(TITLE_RATINGS, separate_longer_delim(TITLE_BASICS, genres, ","), by = "tconst")
titles <- filter(titles, titleType == "movie")
titles |> 
  mutate(decade = signif(startYear, 3)) |> 
  filter(success >= 10) |> 
  group_by(decade, genres) |> 
  summarise(successes = n()) |> 
  arrange(desc(successes)) |> 
  distinct(decade, .keep_all = TRUE)
titles |> 
  mutate(decade = signif(startYear, 3)) |> 
  filter(success >= 10) |> 
  group_by(decade, genres) |> 
  summarise(successes = n()) |> 
  arrange(genres) |> 
  filter(genres %in% c("Action", "Drama", "Romance", "Documentary", "Comedy", "Western", "Biography", "History")) |> 
  ggplot(aes(decade, successes)) +
  geom_line(aes(color = genres))
titles <- titles |> 
  mutate(decade = signif(startYear, 3)) |> 
  mutate(threshold = ifelse(success >= 10, 1, 0))
titles |> 
  filter(startYear >= 2010) |> 
  group_by(genres) |> 
  summarise(successes = sum(threshold),
            ratio = sum(threshold) / n()) |> 
  arrange(desc(successes)) |> 
  ggplot() +
  geom_col(aes(successes, genres)) + 
  theme_minimal()
titles |> 
  filter(startYear >= 2010) |> 
  group_by(genres) |> 
  summarise(successes = sum(threshold),
            ratio = sum(threshold) / n()) |> 
  arrange(desc(successes)) |> 
  ggplot() +
  geom_col(aes(ratio, genres)) +
  theme_minimal()
#Documentaries and biographies have become more popular
#Select Biographies
#Task 5
#Christopher Nolan
titles |> 
  filter(tconst %in% c("tt6723592","tt0816692","tt1375666","tt0482571")) 
#Actors
NAME_BASICS |> 
  filter(primaryName == "Christian Bale")
titles |> 
  filter(tconst %in% c("tt0468569","tt0372784","tt1345836","tt0144084")) 
titles |> 
  filter(genres == "Biography") |> 
  arrange(desc(success))
NAME_BASICS |> 
  filter(primaryName == "Leonardo DiCaprio")
titles |> 
  filter(tconst %in% c("tt1375666","tt0407887","tt0120338","tt0993846", "tt0468569","tt0372784","tt1345836","tt0144084")) |> 
  distinct(tconst, .keep_all = TRUE)
#Task 6
titles |> 
  arrange(desc(success)) |> 
  distinct(tconst, .keep_all = TRUE)
#Schindler's List