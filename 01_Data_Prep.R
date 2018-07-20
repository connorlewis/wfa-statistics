# This file preps the scraped data: 
# Prepping will entail:
# 1. Combining all offensive statistics into one table
# 2. Combining all defensive satistics into one table
# 3. Make special teams tables

# git push in terminal: git push -u origin master 

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rvest)

check <- read_html("http://www.hostedstatistics.com/football/team_stats.asp?league=WFA&season=2018&tier=WFA+I&selected_team=Titans&selected_week=1")

# use this to name the tables: 
tab.names <- check %>% 
  html_nodes(".indStatHead") %>%
  html_text()

d2.dta <- readRDS("div_2_stats.rds")

names(d2.dta) <- tabgrab.names


rush.dta <- d2.dta$Rushing
names(rush.dta)

ggplot(rush.dta %>% filter(Name %in% c("Grace Cooper", "Jeanette Nelson"))
       , aes(x = as.numeric(week), y = as.numeric(Yards)/as.numeric(Carries), color = Name)) +
  geom_line(size = 2) +
  geom_point() +
  labs(x = "Week", y = "Yards per Carry"
       , title = "2018 Regular Season Yards per Carry")





