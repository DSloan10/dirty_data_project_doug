library(tidyverse)
library(readr)
library(here)

dirty_decathlon_data <- read_rds(here::here("raw_data/decathlon.rds"))

# Verifying data

stopifnot(
  names(dirty_decathlon_data) == 
    c("100m", "Long.jump", "Shot.put", "High.jump", "400m",
      "110m.hurdle", "Discus", "Pole.vault", "Javeline",
      "1500m", "Rank", "Points", "Competition")
    )

# Cleaning data

cleaned_decathlon_data <-
  rownames_to_column(
    dirty_decathlon_data, var = "competitor"
    )  

cleaned_decathlon_data <-
cleaned_decathlon_data %>%
  mutate(
    competitor = str_to_title(competitor)
    )  

cleaned_decathlon_data <-
  cleaned_decathlon_data %>%
  rename(
    "100m_(sec)" = "100m",
    "long_jump_(m)" = "Long.jump",
    "shot_putt_(m)" = "Shot.put",
    "high_jump_(m)" = "High.jump",
    "400m_(sec)" = "400m",
    "100m_hurdle_(sec)" = "110m.hurdle",
    "discus_(m)" = "Discus",
    "pole_vault_(m)" = "Pole.vault",
    "javeline_(m)" = "Javeline",
    "1500m_(sec)" = "1500m",
    rank = Rank,
    points = Points,
    competition = Competition
    ) 

cleaned_decathlon_data <-
  cleaned_decathlon_data %>%
  arrange(
    desc(competition), rank)

cleaned_decathlon_data <-
  cleaned_decathlon_data %>%
  select(
    competitor, competition, rank, points, everything()
    ) 

write_csv(cleaned_decathlon_data,"clean_data/clean_decathlon_data")
