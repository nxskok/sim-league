## ------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(conflicted)
library(rvest)
library(tidybayes)
library(progress)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
source("functions.R")
cat("intro")

## ------------------------------------------------------------------------
action2 <- read_rds("../ratings/action2.rds")
action2
games <- read_rds("../scoresway/rds/games.rds")
league_ids <- read_rds("../ratings/league_ids.rds")
rating_ranks <- read_rds("../ratings/rating_ranks.rds") # this is the only place to get team names from
league_ids %>% 
  mutate(r = row_number()) %>% 
  unnest(id) %>% 
  left_join(action2, by = "the_country") %>% 
  arrange(desc(to_rate), next_rate, get_games) -> league_ids
league_ids %>% select(the_country, last_rate, next_rate, day_of, to_rate, get_games) 
cat("league ids")

## ------------------------------------------------------------------------
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
cat("define check_games")

## ------------------------------------------------------------------------
league_ids %>%
  filter(to_rate == "rateable") %>%
  # slice(81:90) %>%
  # mutate(r = row_number()) %>%
  # rowwise() %>%
  # mutate(check = list(check_games(r, league_ids, games))) %>%
  # unnest_wider(check) %>% 
  mutate(rrr = row_number()) %>% 
  select(rrr, the_country, last_rate, next_rate, get_games, rem_games)

cl <- check_league(row_of_this, league_ids, remDr, comp_no)
cl_tab <- cl$table
cl_tab
# cl %>% .[[2]] %>% View(as.character(row_of_this))
# walk(1:3, ~check_league(., league_ids, comp_no))
cat("check league")

## ------------------------------------------------------------------------
name <- read_rds("country_name.rds")
print(glue::glue("reading {name}"))
v <- list.files(pattern = str_c(name, ";", comp_no, ";2*")) # there may be none, in which case it chokes
v
if (length(v) == 0) {
  cl_tab %>% 
    mutate(`1` = round(1000/nrow(cl_tab)),
           `99` = 1000)
} else {
  last_one <- rev(v)[1] # 1, normally
  last_one
  old <- read_rds(last_one)
  old
  old %>% select(team_id, matches("^[0-9]")) -> old
  # old %>% select(Team, matches("^[0-9]")) %>% rename("name" = Team) -> old
}

## ------------------------------------------------------------------------
# old %>% left_join(rating_ranks) %>%
# select(team_id = sw_id, name:P) -> old


## ---- warning=FALSE------------------------------------------------------
nsim <- 1000 # normally 1000
league_id <- read_rds("league_id_singular.rds")
ddd <- run(league_id, league_ids, games, remDr, comp_no, nsim, omits = 0, table_number = tab_number, prior_year = league_ids$prior_needed[row_of_this])
ddd
if (length(v)>0) {
  ddd %>% left_join(old, by = "team_id") -> thing
} else {
  thing <- ddd
}
thing
fname <- str_c(name, ";", comp_no, ";", Sys.time(),".rds")
write_rds(ddd, fname)
glue::glue("Done at {Sys.time()}")
# beepr::beep(1)
# thing
ann <- glue::glue("sim for {name}\n")
cat(ann)

# stopifnot(length(v)>0)

## ------------------------------------------------------------------------
if (length(v)==0) {
  is_results <- FALSE
  results <- thing
  beepr::beep(2)
  stop("length of v is zero")
} else {
  is_results <- TRUE
  diff1 <- function(d) {
    d %>% mutate(across(everything(), ~coalesce(., 0))) -> d
    d[,1] - d[,2]
  }
  col_diff <- function(d, col) {
    # input the x col
    # make the y col name
    yname <- str_replace(col, ".x", ".y")
    d %>% select({{col}}, {{yname}}) %>% 
      mutate(dif = diff1(.)) %>% 
      pull(dif)
  }
  # or get the col names I want
  thing
  thing %>% select(ends_with(".x")) %>% names() -> nms
  map(nms, ~col_diff(thing, .)) %>% bind_cols() -> z
  title <- str_c(name, "_", comp_no, "_", tab_number)
  title
  thing %>% bind_cols(z) -> thing2
  league_ids %>% slice(row_of_this) %>% pull(id) -> the_comp
  the_comp
  last_sim_date <- str_split(last_one, "[;.]")[[1]][3]
  last_sim_date
  games %>% filter(comp == the_comp, stat == "FT", retrieved > last_sim_date) %>% 
    select(t1, t2, score) %>% 
    extract(score, into = c("s1", "s2"), "([0-9]+) - ([0-9]+)") -> new_games
  new_games
  # new_games
  thing2 %>% 
    left_join(new_games, by = c("team_id"="t1")) %>% 
    left_join(new_games, by = c("team_id"="t2")) %>% 
    mutate(opponent = coalesce(t1, t2)) %>% 
    mutate(v = case_when(
      !is.na(s1.x) ~ "h",
      !is.na(s1.y) ~ "a",
      TRUE         ~ NA_character_
    )) %>% 
    mutate(score = case_when(
      !is.na(s1.x) ~ str_c(s1.x, " - ", s2.x),
      !is.na(s1.y) ~ str_c(s2.y, " - ", s1.y),
      TRUE         ~ NA_character_
    ))  %>% 
    # select(-team_id) %>% 
    select(-(`1.y`:`99.y`)) %>% 
    select(-(t2:s2.y)) -> results # but still has team numbers in
    # View(title)
  # results
}
cat("construct table")

## ------------------------------------------------------------------------
# rating_ranks %>% select(name, sw_id) -> lookup
# results %>% left_join(lookup, by = c("team_id"="sw_id")) %>% 
#   select(team = name, everything()) %>% 
#   left_join(lookup, by = c("opponent"="sw_id")) %>% 
#   select(team:opponent, opp_name = name, v, score) %>% 
#   distinct() %>% 
  # View(title)


## ------------------------------------------------------------------------
# thing %>% select(ends_with(".x")) %>% names() -> nms
#   map(nms, ~col_diff(thing, .)) %>% bind_cols() -> z
#   title <- str_c(name, "_", comp_no)
#   thing %>% select(name:P) %>% bind_cols(z) -> thing2
# thing2
# the_comp <- comp # comp number
# v %>% enframe(name = NULL) %>% 
#   extract(value, "dt", ";(2.*).rds") %>% 
#   slice_max(dt, n = 1) %>% pull(dt) -> last_sim_date
# last_sim_date


## ------------------------------------------------------------------------
# games %>% filter(comp == the_comp, stat == "FT", retrieved > last_sim_date) %>% 
#   select(t1_name, t2_name, score) %>% 
#     extract(score, into = c("s1", "s2"), "([0-9]) - ([0-9])") -> new_games



## ------------------------------------------------------------------------
# new_games %>% 
#   extract(score, into = c("s1", "s2"), "([0-9]) - ([0-9])") -> new_games
# thing2 %>% 
#   left_join(new_games, by = c("name"="t1_name")) %>% 
#   left_join(new_games, by = c("name"="t2_name")) %>% 
#   mutate(opponent = coalesce(t1_name, t2_name)) %>% 
#   mutate(v = case_when(
#     !is.na(s1.x) ~ "h",
#     !is.na(s1.y) ~ "a",
#     TRUE         ~ NA_character_
#   )) %>% 
#   mutate(score = case_when(
#     !is.na(s1.x) ~ str_c(s1.x, " - ", s2.x),
#     !is.na(s1.y) ~ str_c(s2.y, " - ", s1.y),
#     TRUE         ~ NA_character_
#   )) %>% 
#   select(-(t2_name:s2.y)) %>% 
#   # View()
# 
# 
# ## ------------------------------------------------------------------------
# diff1 <- function(d) {
#   d %>% mutate(across(everything(), ~coalesce(., 0))) -> d
#   d[,1] - d[,2]
# }
# x <- tibble(a = c(4,2,1,NA,NA), b = c(2,1,NA,NA,NA))
# diff1(x)
# 
# 
# 
# ## ------------------------------------------------------------------------
# name
# comp_no
# comp
# the_comp <- comp
# v <- list.files(pattern = str_c(name, ";", comp_no, "*"))
# sort(v)
# old <- read_rds(v[1])
# old %>% select(name, matches("[0-9]"))
# 
# 
# ## ------------------------------------------------------------------------
# # View(lt)
# summary <- season_summary(comp, games, lt)
# if (summary$summary$s > 0) stop("teams have different #games")
# summary$table
# season_games1 <- summary$summary$m
# # season_games1
# 
# 
# ## ------------------------------------------------------------------------
# comp
# left <- get_games_left(comp, games)
# season_games(lt, left)
# 
# cat("Done")
