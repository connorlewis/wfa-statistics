# This file preps the scraped data: 
# Prepping will entail:
# 1. Combining all offensive statistics into one table
# 2. Combining all defensive satistics into one table
# 3. Make special teams tables

# git push in terminal: git push -u origin master 

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rvest)

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



# Bind the listed datasets for offense, defense, and special teams, separately. 
off.dta <- dta.list[off.tbl.names] %>% 
  reduce(full_join, by = c("Name", "No.", "team", "week", "season")) %>%
  select(-contains("Rnk"))

def.dta <- dta.list[def.tbl.names] %>% 
  reduce(full_join, by = c("Name", "No.", "team", "week", "season"), suffix) %>%
  select(-contains("Rnk"))
  

sp.dta <- dta.list[sp.tbl.names] %>% 
  reduce(full_join, by = c("Name", "No.", "team", "week", "season")) %>%
  select(-contains("Rnk"))



