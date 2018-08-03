# This file preps the scraped data: 
# Prepping will entail:
# 1. Combining all offensive statistics into one table
# 2. Combining all defensive satistics into one table
# 3. Make special teams tables

# git push in terminal: git push -u origin master 

#if (!require("pacman")) install.packages("pacman")
#pacman::p_load(tidyverse, rvest)
library(tidyverse)
library(rvest)

example_page <- read_html("http://www.hostedstatistics.com/football/team_stats.asp?league=WFA&season=2018&tier=WFA+I&selected_team=Titans&selected_week=1")

# use this to name the tables: 
tbl.names <-  example_page %>% 
  html_nodes(".indStatHead") %>%
  html_text() %>%
  trimws() 

# Read in the statistics for all division 
dta.list <- readRDS("Data/div_all_stats.rds")

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



# Creating dataframes -----------------------------------------------------

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
  unique %>%
  unite("Player", No., Name, sep = " ", remove = FALSE) %>%
  group_by(Player, team, season) %>%
  mutate(game = 1:length(Player)
         , Yds.cum = cumsum(Yards)
         , Att.cum = cumsum(Att) 
         , TD.cum = cumsum(TD) 
         , TD.rate.cum = TD.cum/Att.cum
         , Int.rate.cum = cumsum(Int)/Att.cum
         , Yds.Att.cum = Yds.cum/Att.cum
         , TD.Int.cum = case_when(
                          Int.rate.cum == 0 ~ TD.rate.cum
                          , TD.rate.cum == 0 ~ 0
                          , TRUE              ~ TD.rate.cum/Int.rate.cum)
  ) %>%
  filter(Att.cum > 10 & season == 2018) %>%
  ungroup %>%
  arrange(Name)
  
# important values to save: 
pass.yrd.max <- max(pass.sum.dta$Yds.cum)
pass.td.max <- max(pass.sum.dta$TD.cum)
pass.att.max <- max(pass.sum.dta$Att.cum)

