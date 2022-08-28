## ------------------------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(conflicted)
library(rvest)
library(tidybayes)
library(progress)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
source("functions.R")


## ------------------------------------------------------------------------------------------------
action2 <- read_rds("../ratings/action2.rds")
action2
games <- read_rds("../scoresway/rds/games.rds")
league_ids <- read_rds("../ratings/league_ids.rds")
rating_ranks <- read_rds("../ratings/rating_ranks.rds") # now I can get these from same folder as games
league_ids %>% 
  mutate(r = row_number()) %>% 
  unnest(id) %>% 
  left_join(action2, by = "the_country") %>% 
  arrange(desc(to_rate), next_rate, get_games) -> league_ids
league_ids %>% select(the_country, last_rate, next_rate, day_of, to_rate, get_games) 


## ------------------------------------------------------------------------------------------------
check_games <- function(row_number, league_ids, games) {
  league_ids %>% filter(r == row_number) %>% pull(id) -> idd
    games %>% filter(comp == idd) %>% 
      arrange(match) -> games0 
  games0 %>% 
    pivot_longer(matches("[12]_name")) %>%
    select(country, match, name, value) %>% 
    count(country, value) %>% arrange(desc(n)) -> d
  d %>%  summarize(country = min(country), m = mean(n), s = sd(n)) -> summary
  list(games0, d, summary)
}


## ------------------------------------------------------------------------------------------------
league_ids %>%
  filter(to_rate == "rateable") %>%
  filter(get_games == 0) %>% 
  # slice(81:90) %>%
  # mutate(r = row_number()) %>%
  # rowwise() %>%
  # mutate(check = list(check_games(r, league_ids, games))) %>%
  # unnest_wider(check) %>% 
  mutate(rrr = row_number()) %>% 
  select(rrr, the_country, last_rate, next_rate, get_games, rem_games) -> lids
write_rds(lids, "lids.rds")
lids


## ------------------------------------------------------------------------------------------------
library(RSelenium)
remDr <- remoteDriver(port = 4445L)
remDr$open(silent = TRUE)


## ------------------------------------------------------------------------------------------------
stop("Stopping here.")


## ------------------------------------------------------------------------------------------------
remDr$close()

