# functions

# get country info

get_country <- function(this_league, league_ids, prior_year = 2021) {
  league_ids %>% 
    # mutate(row = row_number()) %>% 
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
  
  list(name = country_name, comps = country_comps, priorname = priorname,
       prior = prior, rat = rat)
}

# league table from soccerway

get_league_table <- function(league_id, remDr, table_number = 2) {
  base <- "https://uk.soccerway.com/national/sweden/allsvenskan/2022/regular-season/r"
  url <- str_c(base, league_id, "/")
  # print(glue::glue("in get_league_table with league_id = ", {{league_id}}))
  # print(glue::glue("getting url = ", {{url}}))
  # html <- read_html(url)
  # rselenium
  remDr$navigate(url)
  html_source <- remDr$getPageSource()[[1]][1]
  html <- read_html(html_source)
  # end of rselenium
  html %>% html_nodes("table") %>% .[table_number] -> tab
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
  team_ids <- unique(team_ids)
  league_table %>% 
    mutate(colour = team_colours, team_id = team_ids)
}

get_games_left <- function(league_id, games, omits) {
  games %>% filter(comp == league_id, stat != "FT") %>% 
    select(t1, t2) %>% 
    filter(!(t1 %in% omits)) %>% 
    filter(!(t2 %in% omits))
}

season_games <- function(league_table, games_left) {
  games_left %>% 
    pivot_longer(everything(), values_to = "team_id") %>% 
    count(team_id) -> count_left
  # count_left
  league_table %>% 
    mutate(mp = parse_number(MP)) %>% 
    select(team_id, mp) %>% 
    left_join(count_left) %>% 
    mutate(n = ifelse(is.na(n), 0, n)) %>% 
    mutate(total = mp + n) %>% 
    select(team_id, games = total) -> d
  d %>% 
    summarise(m = mean(games),
              s = sd(games)) -> summ
  list(table = d, summary = summ)
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
  # pb$tick()
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
    # rowwise() %>% 
    mutate(sc1 = rpois(nrow(.), mean1),
           sc2 = rpois(nrow(.), mean2)) %>% 
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
  # pb$tick()
  basis %>% left_join(e, by = c("stan_id" = "id")) %>% 
    replace_na(list(m = 0, pt = 0, f = 0, a = 0)) -> b2
  b2 %>% 
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

make_rank <- function(sim, n_sim=1000, basis) {
  # print(glue::glue("n_sim in make_rank is {n_sim}."))
  # if there are no games left, sim will be empty
  if (nrow(sim) == 0) {
    basis %>% 
      mutate(rank = row_number()) %>% 
      mutate(n = n_sim, cum_n = n_sim) %>% 
      select(team_id, rank, n, cum_n) -> dd
    return(dd)
  }
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

make_mean_rank <- function(made_ranks, n_sim) {
  made_ranks %>% group_by(team_id) %>% 
    summarize(mean_rank = sum(rank*n) / n_sim) %>% 
    arrange(mean_rank)
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

display_rank <- function(d, rk, lookup) {
  d %>% 
    filter(rank<=rk) %>% 
    group_by(team_id) %>% 
    filter(rank==max(rank)) %>% 
    arrange(desc(cum_n), rank)%>% 
    left_join(lookup, by = c("team_id" = "sw_id")) %>% 
    select(name, rank, cum_n)
}

display_ranks <- function(made_ranks, rk, mean_ranks, lookup, league_table) {
  tibble(rk = rk) %>% 
    rowwise() %>% 
    mutate(disp = list(display_rank(made_ranks, rk, lookup))) %>% 
    unnest(disp) %>% 
    mutate(cum_n = cum_n * 1000 / max(cum_n)) %>% 
    select(-rank) %>% 
    pivot_wider(names_from = rk, values_from = cum_n) %>% 
    left_join(league_table, by = "team_id") %>% 
    select(team_id:P) %>% 
    select(-Team) %>% 
    left_join(mean_ranks) %>% 
    arrange(mean_rank)
}

run <- function(league_id, league_ids, games, remDr, 
                comp_no = 1, n_sim = 1000, omits = 0, prior_year = 2021, table_number = 2) {
  print(glue::glue("n_sim is {n_sim}."))
  country_list <- get_country(league_id, league_ids, prior_year) 
  comp <- country_list$comps[comp_no]
  league_table <- get_league_table(comp, remDr, table_number) 
  print(glue::glue("Got league table."))
  draws <- get_draws(country_list)
  print(glue::glue("Got draws."))
  lookup <- get_lookup(country_list)
  print(glue::glue("Got lookup."))
  basis <- get_basis(league_table, lookup)
  cat("Done basis\n")
  games_left <- get_games_left(comp, games, omits)
  cat("done games left\n")
  simulated <- sim_games(games_left, lookup, country_list)
  cat("done sim_games\n")
  made_ranks <- make_rank(simulated, n_sim, basis)
  mean_ranks <- make_mean_rank(made_ranks, n_sim)
  cat("done make_rank\n")
  rk <- get_ranks(league_table)
  cat("done get_ranks\n ")
  display_ranks(made_ranks, rk, mean_ranks, lookup, league_table) %>% select(-name)
}


check_league <- function(row_of_this, league_ids, remDr, comp_no = 1) {
  league_id <- league_ids$row[row_of_this]
  write_rds(league_id, "league_id_singular.rds")
  country <- get_country(league_id, league_ids,   league_ids$prior_needed[row_of_this])
  comp <- country$comps[comp_no]
  name <- country$name
  write_rds(name, "country_name.rds")
  left <- get_games_left(comp, games, omits = 0)
  # left %>% filter(t1 == 1870 | t2 == 1870)
  print(glue::glue("Calling get_league_table with comp = {comp}"))
  lt <- get_league_table(comp, remDr)
  a <- season_games(lt, left) 
  a$summary %>% mutate(the_name = name) -> summary
  list(table = a$table, summary = summary)
}
