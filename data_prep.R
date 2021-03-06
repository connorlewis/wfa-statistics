# This file preps the scraped data: 
# Prepping will entail:
# 1. Prepping Passing, Rushing, and Receiving data frames
# 2. Combining all defensive satistics into one table
# 3. Make special teams tables

# git push in terminal: git push -u origin master 

pacman::p_load(tidyverse, rvest)

# load in the supporting functions script:
source("functions.R")

# Use this to get the table names, I have saved them as table_column_names.rda (tbl.names)
# example_page <- read_html("http://www.hostedstatistics.com/football/team_stats.asp?league=WFA&season=2018&tier=WFA+I&selected_team=Titans&selected_week=1")
# 
# # use this to name the tables: 
# tbl.names <-  example_page %>% 
#   html_nodes(".indStatHead") %>%
#   html_text() %>%
#   trimws() 
# 
# save(tbl.names, file = "data/table_column_names.rda")
load("data/table_column_names.rda")
# Read in the statistics for all division 
dta.list <- readRDS("data/div_all_stats_2019.rds")

# name the tables using tab
names(dta.list) <- tbl.names

# group names into Offense, Defense and special teams: 
off.tbl.names <- c("Passing", "Rushing", "Receiving")
def.tbl.names <- c("Tackles", "Sacks", "Forced Fumbles"
                   , "Interceptions (Top 25)", "Fumble Recoveries"
                   , "Pass Deflections")
sp.tbl.names <- c("Blocked Kicks & Punts", "Kickoff Returns"
                  , "Punt Returns", "Punting", "Touchdowns"
                  ,"Field Goals", "PATs", "2-Point PATs", "Safeties")



# Creating dataframes from the list of df -----------------------------

# Create offensive data sets for each of the main categories: Passing, Rushing, Receiving. 
pass.dta <- dta.list[["Passing"]] %>%
  data.frame %>%
  select(-contains("Rnk"))

rush.dta <- dta.list[["Rushing"]] %>%
  data.frame %>%
  select(-contains("Rnk"))

rec.dta <- dta.list[["Receiving"]] %>%
  data.frame %>%
  select(-contains("Rnk"))

# Create data sets for all of the offensive, defensive, and special teams, sepearatly. 
off.dta <- dta.list[off.tbl.names] %>% 
  reduce(full_join, by = c("Name", "No.", "team", "week", "season")) %>%
  select(-contains("Rnk"))

def.dta <- dta.list[def.tbl.names] %>% 
  reduce(full_join, by = c("Name", "No.", "team", "week", "season"), suffix) %>%
  select(-contains("Rnk"))
  

sp.dta <- dta.list[sp.tbl.names] %>% 
  reduce(full_join, by = c("Name", "No.", "team", "week", "season")) %>%
  select(-contains("Rnk"))

# Need to add Division level to teams for each season
# Need the week by week games: Oppenetns, scores, etc. 
# Positions for each player.. QB, RB, LB, etc. 


# Passing Data Prep -------------------------------------------------------
# Create the necessary stats of interest:
# repeate, some are double numbers: michele walsh, angela mason, ashley bush, maggie Hudkins, rachel gore, samantha valentino, 
pass.cum.dta <- pass.dta %>% 
  create.name.vars %>%
  mutate_at(.funs = funs(as.numeric(.)), .vars = vars(yards:week, season)) %>%
  group_by(player, team, season) %>%
  arrange(week) %>%
  mutate_at(vars(yards:att, td:int), funs(cum = cumsum)) %>%
  rename(pass_yards_cum = yards_cum, pass_td_cum = td_cum
        , pass_yards = yards, pass_td = td) %>%
  mutate(game = as.numeric(1:length(player))
         , pass_td_rate_cum = pass_td_cum/att_cum
         , comp_cum_rate = round(comp_cum/att_cum*100)
         , int_rate_cum = int_cum/att_cum
         , pass_yards_att_cum = pass_yards_cum/att_cum
         , pass_avg_cum = round(pass_yards_cum/comp_cum, 1)
         , pass_yards_cum_game = round(pass_yards_cum/game, 1)
         , pass_td_int_cum = case_when(
                          int_rate_cum == 0 ~ pass_td_rate_cum
                          , pass_td_rate_cum == 0 ~ 0
                          , TRUE              ~ pass_td_rate_cum/int_rate_cum)
         # Now create the NFL passer rating:
         , qb.part1 = case_when(
              (comp_cum/att_cum*100-30)*0.05 < 0     ~ 0
              , (comp_cum/att_cum*100-30)*0.05 > 2.375 ~2.375
              , TRUE ~ (comp_cum/att_cum*100-30)*0.05
         )
         , qb.part2 = case_when(
           (pass_yards_att_cum-3)*.25 < 0 ~ 0
           , (pass_yards_att_cum-3)*.25 > 2.375 ~ 2.375
           , TRUE ~ (pass_yards_att_cum-3)*.25
         )
         , qb.part3 = case_when(
           (pass_td_rate_cum*100*.2) < 0 ~ 0
           , (pass_td_rate_cum*100*.2) > 2.375 ~ 2.375
           , TRUE ~ (pass_td_rate_cum*100*.2)
         )
         , qb.part4 = case_when(
           2.375-(int_rate_cum*100*.25) < 0 ~ 0
           , 2.375-(int_rate_cum*100*.25) > 2.375 ~ 2.375
           , TRUE ~ 2.375-(int_rate_cum*100*.25)
         )
         , qb_rate_cum = round((qb.part1 + qb.part2 
                             + qb.part3 + qb.part4)/6*100, 1)) #%>%
  # join with the WL data for team calculations. 
  #mutate(team_for_merge)
  #left_join(wl.dta.2018.prepped, )



# Rushing Data Prep -------------------------------------------------------
# create cumulative data that can be merged with other datasets.
rush.cum.dta <- rush.dta %>% 
  create.name.vars() %>%
  mutate_at(.funs = funs(as.numeric(.)), .vars = vars(yards:week, season)) %>%
  group_by(player, team, season) %>%
  arrange(week) %>%
  mutate_at(vars(yards:carries, td), funs(cum = cumsum)) %>%
  rename(rush_td_cum = td_cum, rush_yards = yards
         , rush_yards_cum = yards_cum, rush_td = td) %>%
  mutate(game = as.numeric(1:length(player))
         , rush_td_rate_cum = rush_td_cum/carries_cum
         , rush_avg_cum = round(rush_yards_cum/carries_cum, 1)
         , rush_yards_cum_game = round(rush_yards_cum/game, 1)
         , rush_yards_car = round(rush_yards/carries, 1))




# Receiving data prep -------------------------------------------------------
rec.cum.dta <- rec.dta %>% 
  create.name.vars() %>%
  mutate_at(.funs = funs(as.numeric(.)), .vars = vars(yards:week, season)) %>%
  group_by(player, team, season) %>%
  mutate_at(vars(yards, rec, td), funs(cum = cumsum)) %>%
  rename(rec_td = td, rec_td_cum = td_cum
         , rec_yards = yards, rec_yards_cum = yards_cum) %>%
  mutate(game = as.numeric(1:length(player))
         , rec_td_rate_cum = rec_td_cum/rec_cum
         , rec_avg_cum = round(rec_yards_cum/rec_cum, 1)
         , rec_yards_cum_game = round(rec_yards_cum/game, 1)
         , rec_yards_rec = round(rec_yards/rec, 1))


  

# Subsets for dashboard ---------------------------------------------------

qb.min <- 5
rb.min <- 10
wr.min <- 4

qb.dash.dta <- pass.cum.dta %>%
        filter(last(att_cum) > qb.min & season == 2019) %>%
        left_join(rush.cum.dta, by = c("player", "plyr.lbl", "team", "season", "game")
                  , suffix = c("", ".rush")) %>%
        ungroup %>%
        arrange(name)

rb.dash.dta <- rush.cum.dta %>%
        filter(last(carries_cum) > rb.min & carries > 1
               & season == 2019) %>%
        left_join(rec.cum.dta, by = c("player", "plyr.lbl", "team", "season", "game")
                  , suffix = c(".rush", ".rec")) %>%
        ungroup %>%
        arrange(name.rush)

wr.dash.dta <- rec.cum.dta %>%
        filter(last(rec_cum) > wr.min & season == 2019) %>%
        left_join(rush.cum.dta, by = c("player", "plyr.lbl", "team", "season", "game")
                  , suffix = c(".rec", ".rush")) %>%
        ungroup %>%
        arrange(name.rec)
        
# other values necessary for the dashboard: 
pass.yrd.max <- max(qb.dash.dta$pass_yards_cum)
pass.yrd.min <- min(qb.dash.dta$pass_yards_cum)
pass.td.max <- max(qb.dash.dta$pass_td_cum)
pass.att.max <- max(qb.dash.dta$att_cum)

# for offline working
write_csv(qb.dash.dta, "qb_2019.csv")
write_csv(rb.dash.dta, "rb_2019.csv")
write_csv(wr.dash.dta, "wr_2019.csv")
# 
# 
<<<<<<< HEAD

        
        
        
        
        
        
        
        
=======
# 

# Read in Team Data -------------------------------------------------------
team.raw.dta <- readRDS("data/game_score_historical.rds")
View(team.raw.dta)

w.l.2018.dta <- team.raw.dta %>%
  mutate_at(vars(visitor_score, home_team_score)
            , funs(str_remove(str_remove(., "[)]"), "[(]"))) %>% 
  separate(week, c("wk.part1", "wk.part2", "wk.part3", "wk.part4", "wk.part5"), sep = " ", remove = TRUE) %>%
  mutate(postseason = case_when(
          wk.part1 == "Postseason" ~ 1
          , TRUE  ~ 0)
         , week = case_when(
            postseason == 1     ~ wk.part3
            , TRUE              ~ wk.part2)) %>%
  select(-c(wk.part1:wk.part5)) %>%
  filter(year == 2018)
  

# Rankings using: https://cran.r-project.org/web/packages/fbRanks/vignettes/Basic_team_ranking.pdf

# pacman::p_load(fbRanks, glmnet)      
# rank.team.dta  <- w.l.2018.dta %>%
#   filter(postseason == 0) %>%
#   select(home.team = home_team, home.score = home_team_score, 
#          away.score = visitor_score, away.team = visitor
#          , week, year) %>%
#   mutate(date = as.Date(paste0(year, "-03-31")) + as.numeric(week)*7
#          , home.score = as.numeric(home.score)
#          ,away.score = as.numeric(away.score)) %>%
#   select(-week, -year)
# 
# 
# ranks = rank.teams(scores = rank.team.dta)
# print(ranks)
# ranks %>% View()
# str(rank.team.dta)

table(rb.dash.dta$team)
table(w.l.2018.dta$home_team)
        
        
        

wl.dta.2018.prepped <- w.l.2018.dta %>% 
  separate(home_team, c("home1", "home2", "home3", "home4"), remove = FALSE) %>%
  separate(visitor, c("away1", "away2", "away3", "away4"), remove = FALSE) %>%
  mutate(home_temp = case_when(
        !is.na(home4) ~ home4
        , !is.na(home3) ~ home3
        , TRUE          ~ home2)
        , home_for_merge = case_when(
                 home_temp == "Blaze" & home1 == "Madison"  ~ "MD Blaze"
                 , home_temp == "Blaze" & home1 == "Mile"   ~ "MH Blaze"
                 , home_temp == "Reign" & home1 == "Everett" ~ "EV Reign"
                 , home_temp == "Reign" & home1 == "Toledo" ~ "TO Reign"
                 , home_temp == "Phoenix" & home1 == "Atlanta" ~ "AT Phoenix"
                 , home_temp == "Phoenix" & home1 == "Carolina" ~ "CA Phoenix"                   
                 , TRUE                                         ~ home_temp)
        , away_temp = case_when(
                  !is.na(away4) ~ away4
                  , !is.na(away3) ~ away3
                  , TRUE          ~ away2)
        
        ## When we do 2017, be careful of war angels and dark angels
        , away_for_merge = case_when(
                  away_temp == "Blaze" & away1 == "Madison"  ~ "MD Blaze"
                , away_temp == "Blaze" & away1 == "Mile"   ~ "MH Blaze"
                , away_temp == "Reign" & away1 == "Everett" ~ "EV Reign"
                , away_temp == "Reign" & away1 == "Toledo" ~ "TO Reign"
                , away_temp == "Phoenix" & away1 == "Atlanta" ~ "AT Phoenix"
                , away_temp == "Phoenix" & away1 == "Carolina" ~ "CA Phoenix"                   
                , TRUE                                         ~ away_temp)
        , date = as.Date(paste0(year, "-03-31")) + as.numeric(week)*7) %>%
  select(-c(home1:home4, home_temp, away1:away4, away_temp)) 
  
w.l.2018.dta %>% 
  gather("location", "team", c(1,3)) %>% View()
      


>>>>>>> 9cf3c5df7e3e87a2b7c8abae8aaf657c518ec6d3
