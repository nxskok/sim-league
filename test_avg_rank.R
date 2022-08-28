library(tidyverse)
library(conflicted)
library(rvest)
library(tidybayes)
library(progress)
library(logr)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
source("functions.R")

action2 <- read_rds("../ratings/action2.rds")
action2
games <- read_rds("../scoresway/rds/games.rds")
league_ids <- read_rds("../ratings/league_ids.rds")
league_ids %>% 
  mutate(r = row_number()) %>% 
  unnest(id) %>% 
  left_join(action2, by = "the_country") %>% 
  arrange(desc(to_rate), get_games) -> league_ids
# league_ids %>% arrange(desc(last_rate)) -> league_ids
league_ids

row_of_this <- 1
comp_no <- 1
check_league(row_of_this, league_ids, comp_no)

name <- read_rds("country_name.rds")
name
v <- list.files(pattern = str_c(name, ";", comp_no, ";2*"))
v
old <- read_rds(tail(sort(v), 1))
old %>% select(name, matches("^[0-9]")) -> old
old

n_sim <- 1000
league_id <- read_rds("league_id_singular.rds")
league_id

# now function "run"

print(glue::glue("n_sim is {n_sim}."))
country_list <- get_country(league_id, league_ids, 2021) 
comp <- country_list$comps[comp_no]
comp
league_table <- get_league_table(comp) 
league_table
print(glue::glue("Got league table."))
draws <- get_draws(country_list)
draws
print(glue::glue("Got draws."))
lookup <- get_lookup(country_list)
lookup
print(glue::glue("Got lookup."))
basis <- get_basis(league_table, lookup)
basis
cat("Done basis\n")
games_left <- get_games_left(comp, games, c())
games_left
cat("done games left\n")
simulated <- sim_games(games_left, lookup, country_list)
simulated %>% unnest(data)
cat("done sim_games\n")
made_ranks <- make_rank(simulated, n_sim, basis)
cat("done make_rank\n")
# work out mean rank
mean_ranks <- make_mean_rank(made_ranks, n_sim)
mean_ranks
rk <- get_ranks(league_table)
cat("done get_ranks\n ")
display_ranks(made_ranks, rk, mean_ranks, lookup, league_table)

# this is display_ranks
tibble(rk = rk) %>% 
  rowwise() %>% 
  mutate(disp = list(display_rank(made_ranks, rk, lookup))) %>% 
  unnest(disp) %>% 
  mutate(cum_n = cum_n * 1000 / max(cum_n)) %>% 
  select(-rank) %>% 
  pivot_wider(names_from = rk, values_from = cum_n) %>% 
  left_join(league_table, by = "team_id") %>% 
  select(team_id:P) %>% 
  select(-name) %>% 
  left_join(mean_ranks) %>% 
  arrange(mean_rank)


ddd
