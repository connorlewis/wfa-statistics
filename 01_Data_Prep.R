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
tab.names <-  example_page %>% 
  html_nodes(".indStatHead") %>%
  html_text()

# Read in the division 2 statistics
d2.dta.list <- readRDS("div_2_stats.rds")

# name the tables using tab
names(d2.dta.list) <- tab.names

# combine data
full.dta <- d2.dta %>% 
  reduce(full_join, by = c("Name", "No.", "team", "week")) %>%
  select(-contains("Rnk"))

View(full.dta)
