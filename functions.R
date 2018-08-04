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

# This function creates plots over the season by game and highlights the 
# selected players' paths
# Input: 
#    react.dta the reactive data,
#    dta: underlying data (all)
#    y.var: the variable to be plotted on the Y axis,
#    y.axis.lbl: the title of the y.axis
#    y.lims: the limits for the y-axis (e.g. c(0,200))
#    
var.by.game.plot <- function(react.dta, dta, y.var, y.axis.lbl, y.lims){
  
  # using tidyeval language
  quo_var <- enquo(y.var)
  
  react.dta %>%
    ggplot(aes(x = game, y = !!quo_var
               , group = Player, color = Player)) +
    
    geom_line(data = dta 
              , mapping = aes(x = game, y = !!quo_var
                              , group = factor(Player))
              , color = "grey") +
    # geom_smooth(data = dta 
    #             , mapping = aes(x = game, y = !!quo_var)
    #             , method = "loess", se = FALSE, color = "red") +
    
    geom_point(size = 3) +
    geom_line(size = 1.5) +
    labs(x = "Game"
         , y = y.axis.lbl
         , caption = "Grey lines represent all non-selected players.") +
    coord_cartesian(xlim = c(1, 9)
                    , ylim = y.lims) +
    scale_x_continuous(breaks = seq(1,8,1)) +
    scale_color_discrete(guide = FALSE) +
    geom_text(data = (react.dta %>%
                group_by(Player) %>%
                top_n(1, as.numeric(game))),
              mapping = aes(x = game + 0.6, y = !!quo_var
                            , label = plyr.lbl, color = Player)
              , size = 4.5)
  
}