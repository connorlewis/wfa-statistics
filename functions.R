# These are the supporting functions for the wfa data prep and dashboard.

# This function creates the columns with various combinations of 
# plyr name, number, and initials for labelling and selection purposed
# used in the dashboard.

create.name.vars <- function(dta) {
  dta %>% 
    unique %>%
    # first creat just the first initial and last name 
    # of player for plotting labeling: 
    separate(Name, c("First", "Last"), sep = " ", remove = FALSE) %>%
    mutate(First = substr(First, 1,1) 
           , Last = substr(Last, 1,1)) %>%
    unite("plyr.temp", First, Last, sep = "", remove = TRUE) %>%
    unite("plyr.lbl", plyr.temp, No., sep = " ", remove = FALSE) %>%
    unite("Player", No., Name, sep = " ", remove = FALSE) %>%
    select(-plyr.temp)
}

