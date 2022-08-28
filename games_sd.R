# get sd of games played for each league
# it may be better to check these as needed in sim-league

library(tidyverse)
library(lubridate)
library(conflicted)
library(rvest)
library(tidybayes)
library(progress)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
source("functions.R")


action2 <- read_rds("../ratings/action2.rds")
action2
games <- read_rds("../scoresway/rds/games.rds")
league_ids <- read_rds("../ratings/league_ids.rds")
rating_ranks <- read_rds("../ratings/rating_ranks.rds") # this is the only place to get team names from, also uses posteriors that may not exist
league_ids %>% 
  mutate(r = row_number()) %>% 
  unnest(id) %>% 
  left_join(action2, by = "the_country") %>% 
  arrange(desc(to_rate), next_rate, get_games) -> league_ids
league_ids %>% select(the_country, next_rate, day_of, to_rate, get_games)


options(width = 132)

league_ids %>% 
  # filter(to_rate == "rateable") %>%
  mutate(r = row_number()) %>% 
  rowwise() %>% 
  mutate(check = list(check_league(r, league_ids, 1))) %>% 
  unnest_wider(check) %>% 
  unnest_wider(summary) %>% 
  select(r, the_name, m, s, next_rate, get_games, rem_games) %>% 
  arrange(s) %>% 
  print(n = Inf)


