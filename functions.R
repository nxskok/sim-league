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
    select(name, MP, P, F, A, team_id, stan_id) 
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

# league table of just simulated games using stan id

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
    pivot_wider(names_from = set, values_from = value)  %>%
    unnest_legacy() %>%
    group_by(id) %>%
    summarize(across(c(m, pt, f, a), ~sum(.)))
}

simulated_table <- function(d, basis) {
  e <- extra_table(d)
  basis %>% left_join(e, by = c("stan_id" = "id")) %>% 
    mutate(m = as.numeric(MP) + m,
           pt = as.numeric(P) + pt,
           f = as.numeric(F) + f,
           a = as.numeric(A) + a) %>% 
    select(team_id, name, m, f, a, pt) %>% 
    arrange(desc(pt), desc(f-a), desc(f))
}

simulated_rank <- function(tab) {
  tab %>% mutate(rank = row_number()) %>% 
    select(team_id, rank)
}

make_rank <- function(sim, n_sim=1000) {
  sim %>% 
    ungroup() %>% 
    slice_sample(n = n_sim) %>% 
    rowwise() %>% 
    mutate(simtab = list(simulated_table(data, basis))) %>% 
    mutate(rk = list(simulated_rank(simtab))) -> d
  d %>% unnest(rk) %>% 
    group_by(team_id) %>% 
    count(rank) %>% 
    mutate(cum_n = cumsum(n))
}

get_ranks <- function(league_table) {
  league_table %>% 
    mutate(colour = 
             ifelse(colour == "", "rank-grey", colour)) %>% 
    select(colour) %>% 
    mutate(lead_col = lead(colour)) %>% 
    mutate(x = (colour != lead_col)) %>% 
    pull(x) %>% which() -> y
  y <- c(y, 99)
  if (y[1]==1) return(y) else return(c(1, y))
}

display_rank <- function(d, rk) {
  d %>% 
    filter(rank<=rk) %>% 
    group_by(team_id) %>% 
    filter(rank==max(rank)) %>% 
    arrange(desc(cum_n), rank)%>% 
    left_join(lookup, by = c("team_id" = "sw_id")) %>% 
    select(name, rank, cum_n)
}

display_ranks <- function(dd, rk) {
  tibble(rk = rk) %>% 
    rowwise() %>% 
    mutate(disp = list(display_rank(dd, rk))) %>% 
    unnest(disp) %>% 
    select(-team_id, -rank) %>% 
    pivot_wider(names_from = rk, values_from = cum_n)
}

run <- function(league_id, league_ids, comp_no = 1, games) {
  country_list <- get_country(league_id, league_ids) 
  comp <- country_list$comps[comp_no]
  league_table <- get_league_table(comp) # have to get comp
  draws <- get_draws(country_list)
  lookup <- get_lookup(country_list)
  basis <- get_basis(league_table, lookup)
  games_left <- get_games_left(comp, games)
  simulated <- sim_games(games_left, lookup, country_list)
  dd <- make_rank(simulated, n_sim = 100)
  rk <- get_ranks(league_table)
  display_ranks(dd, rk)
}