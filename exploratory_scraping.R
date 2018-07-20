library(rvest)
library(tidyverse)


check <- read_html("http://www.hostedstatistics.com/football/team_stats.asp?league=WFA&season=2018&tier=WFA+I&selected_team=Titans&selected_week=1")


  
tabgrab <- check %>% 
   html_nodes("table") %>%
   html_table(fill = TRUE, header = TRUE) %>%
   keep(~(c("Name", "Rnk") %in% names(.)))

tabgrab <- check %>% 
  html_nodes(".indStatHead") %>%
  html_text()



tabgrab[[8]]

length(tabgrab)
View(tabgrab)


tabgrab[[]]


tabgrab
