# This file preps the scraped data: 
# Prepping will entail:
# 1. Prepping Passing, Rushing, and Receiving data frames
# 2. Combining all defensive satistics into one table
# 3. Make special teams tables

# git push in terminal: git push -u origin master 

library(tidyverse)
library(rvest)

# load in the supporting functions script:
source("functions.R")
example_page <- read_html("http://www.hostedstatistics.com/football/team_stats.asp?league=WFA&season=2018&tier=WFA+I&selected_team=Titans&selected_week=1")

# use this to name the tables: 
tbl.names <-  example_page %>% 
  html_nodes(".indStatHead") %>%
  html_text() %>%
  trimws() 

# Read in the statistics for all division 
dta.list <- readRDS("data/div_all_stats.rds")

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
# 1. 
pass.sum.dta <- pass.dta %>% 
  create.name.vars %>%
  group_by(Player, team, season) %>%
  mutate(game = 1:length(Player)
         , Yds.cum = cumsum(Yards)
         , Att.cum = cumsum(Att) 
         , TD.cum = cumsum(TD) 
         , comp.cum = cumsum(Comp)
         , TD.rate.cum = TD.cum/Att.cum
         , comp.cum.rate = round(comp.cum/Att.cum*100)
         , Int.rate.cum = cumsum(Int)/Att.cum
         , Yds.Att.cum = Yds.cum/Att.cum
         , avg.cum = round(Yds.cum/comp.cum, 1)
         , TD.Int.cum = case_when(
                          Int.rate.cum == 0 ~ TD.rate.cum
                          , TD.rate.cum == 0 ~ 0
                          , TRUE              ~ TD.rate.cum/Int.rate.cum)
  
         , qb.part1 = case_when(
              (comp.cum/Att.cum*100-30)*0.05 < 0     ~ 0
              , (comp.cum/Att.cum*100-30)*0.05 > 2.375 ~2.375
              , TRUE ~ (comp.cum/Att.cum*100-30)*0.05
         )
         , qb.part2 = case_when(
           (Yds.Att.cum-3)*.25 < 0 ~ 0
           , (Yds.Att.cum-3)*.25 > 2.375 ~ 2.375
           , TRUE ~ (Yds.Att.cum-3)*.25
         )
         , qb.part3 = case_when(
           (TD.rate.cum*100*.2) < 0 ~ 0
           , (TD.rate.cum*100*.2) > 2.375 ~ 2.375
           , TRUE ~ (TD.rate.cum*100*.2)
         )
         , qb.part4 = case_when(
           2.375-(Int.rate.cum*100*.25) < 0 ~ 0
           , 2.375-(Int.rate.cum*100*.25) > 2.375 ~ 2.375
           , TRUE ~ 2.375-(Int.rate.cum*100*.25)
         )
         , qb.rate.cum = round((qb.part1 + qb.part2 
                             + qb.part3 + qb.part4)/6*100, 1)) %>%
  filter(last(Att.cum) > 50 & season == 2018) %>%
  ungroup %>%
  arrange(Name)
  
# important values to save: 
pass.yrd.max <- max(pass.sum.dta$Yds.cum)
pass.yrd.min <- min(pass.sum.dta$Yds.cum)
pass.td.max <- max(pass.sum.dta$TD.cum)
pass.att.max <- max(pass.sum.dta$Att.cum)



# Rushing Data Prep -------------------------------------------------------
# 1. 
rush.sum.dta <- rush.dta %>% 
  create.name.vars() %>%
  group_by(Player, team, season) %>%
  mutate(game = 1:length(Player)
         , yds.cum = cumsum(as.numeric(Yards))
         , car.cum = cumsum(as.numeric(Carries))
         , TD.cum = cumsum(as.numeric(TD))
         , TD.rate.cum = TD.cum/car.cum
         , avg.cum = round(yds.cum/car.cum, 1)
         , yds.cum.game = round(yds.cum/game, 1)
         , yds.car = round(as.numeric(Yards)/as.numeric(Carries), 1)) %>%
  filter(last(car.cum) > 50 & season == 2018) %>%
  ungroup %>%
  arrange(Name)


# Receiving data prep -------------------------------------------------------
rec.sum.dta <- rec.dta %>% 
  create.name.vars() %>%
  group_by(Player, team, season) %>%
  mutate(game = 1:length(Player)
         , yds.cum = cumsum(as.numeric(Yards))
         , rec.cum = cumsum(as.numeric(Rec))
         , TD.cum = cumsum(as.numeric(TD))
         , TD.rate.cum = TD.cum/rec.cum
         , avg.cum = round(yds.cum/rec.cum, 1)
         , yds.cum.game = round(yds.cum/game, 1)
         , yds.rec = round(as.numeric(Yards)/as.numeric(Rec), 1)) %>%
  filter(last(rec.cum) > 10 & season == 2018) %>%
  ungroup %>%
  arrange(Name)
