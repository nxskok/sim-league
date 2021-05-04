# functions

# get country info

get_country <- function(this_league, league_ids, prior_year = 2021) {
  league_ids %>% 
    mutate(row = row_number()) %>% 
    filter(row == this_league) -> d
  country_name <- d$the_country
  country_lower <- tolower(country_name)
  country_comps <- d$id[[1]]
  rating_dir <- "../ratings/"
  priorname <- str_c(rating_dir, 
                     country_lower, 
                     prior_year,
                     "_prior.csv")
  postname <- str_c(rating_dir, country_lower,"_post.rds")
  prior <- read_csv(priorname)
  read_rds(postname) -> rat
  
  list(name = country_name, comps = country_comps, 
       prior = prior, rat = rat)
}

# league table from soccerway

get_league_table <- function(league_id) {
  base <- "https://uk.soccerway.com/national/andorra/1a-divisio/20202021/championship-round/r"
  url <- str_c(base, league_id, "/")
  html <- read_html(url)
  html %>% html_nodes("table") %>% .[2] -> tab
  tab %>% html_nodes("tr") %>% html_attr("class") %>% 
    str_detect("team_rank") -> rows
  tab %>% html_table() %>% .[[1]] %>% 
    select("Team", "MP", "F", "A", "P") %>% 
    slice(which(rows)-1) -> league_table
  tab %>% html_nodes("td") %>% html_attr("class") %>% 
    str_subset("rank") %>% str_replace("^rank ", "") -> team_colours
  tab %>% html_nodes("td") %>% html_nodes("a") %>% 
    html_attr("href") %>% 
    str_subset("^/teams") %>% 
    str_extract("/[0-9]+/$") %>% 
    parse_number() -> team_ids
  league_table %>% 
    mutate(colour = team_colours, team_id = team_ids)
}

get_games_left <- function(league_id, games) {
  games %>% filter(comp == league_id, stat != "FT") %>% 
    select(t1, t2) 
}

season_games <- function(league_table, games_left) {
  games_left %>% 
    pivot_longer(everything(), values_to = "team_id") %>% 
    count(team_id) -> count_left
  count_left
  league_table %>% 
    mutate(mp = parse_number(MP)) %>% 
    select(team_id, mp) %>% 
    left_join(count_left) %>% 
    mutate(total = mp + n) %>% 
    select(team_id, games = total) %>% 
    summarise(m = mean(games),
              s = sd(games))
}

season_summary <- function(league_id, games, league_table) {
  games_left <- get_games_left(league_id, games)
  season_games(league_table, games_left)
}

get_draws <- function(country_list) {
  country_list$rat %>% 
    spread_draws(o[i], d[i], h) %>%
    select(i, o, d, h, .draw)
}

get_lookup <- function(country_list) {
  country_list$prior %>% 
    select(stan_id = id, sw_id, name)
}

get_basis <- function(league_table, lookup) {
  league_table %>%
    left_join(lookup, by=c("team_id"="sw_id")) %>% 
    select(name, MP, P, team_id, stan_id) %>% 
    left_join(post_mean, by = c("stan_id"="i"))
}

sim_games <- function(games_left, lookup, country_list) {
  games_left %>% 
    left_join(lookup, by = c("t1"="sw_id")) %>% 
    left_join(lookup, by = c("t2"="sw_id")) %>% 
    select(id1 = stan_id.x, id2 = stan_id.y) -> left_stan
  get_draws(country_list) %>% 
    ungroup() %>% 
    nest_by(i) -> nested_draws
  left_stan %>% 
    left_join(nested_draws, by = c("id1" = "i")) %>% 
    left_join(nested_draws, by = c("id2" = "i")) %>% 
    unnest(data.x, data.y) %>% 
    mutate(mean1 = exp(o - d1 + h), 
           mean2 = exp(o1 - d)) %>% 
    rowwise() %>% 
    mutate(sc1 = rpois(1, mean1),
           sc2 = rpois(1, mean2)) %>% 
    select(id1, id2, sc1, sc2, .draw) %>% 
    nest_by(.draw)
}

pt <- function(s1, s2) {
  case_when(
    s1>s2 ~ 3,
    s1==s2 ~ 1,
    TRUE   ~ 0
    )
}

# league table of just simulated games

extra_table <- function(d) {
  d %>% 
    rename(id_1 = id1, id_2 = id2,
           sc_1 = sc1, sc_2 = sc2) %>% 
    mutate(pt_1 = pt(sc_1, sc_2),
           pt_2 = pt(sc_2, sc_1),
           m_1 = 1,
           m_2 = 1,
           f_1 = sc_1,
           f_2 = sc_2, 
           a_1 = sc_2,
           a_2 = sc_1) %>% 
    pivot_longer(everything(),
                 names_to = c("set", ".value"),
                 names_pattern = "(.+)_(.+)") %>% 
    pivot_longer(!set) %>% 
    pivot_wider(names_from = set, values_from = value) %>% 
    unnest() %>% 
    group_by(id) %>% 
    summarize(across(c(m, pt, f, a), ~sum(.)))
}
