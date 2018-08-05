# These are the supporting functions for the wfa data prep and dashboard.

# This function creates the columns with various combinations of 
# plyr name, number, and initials for labelling and selection purposed
# used in the dashboard.

create.name.vars <- function(df) {
  
  df %>% 
    rename_all(tolower) %>%
    unique %>%
    # first creat just the first initial and last name 
    # of player for plotting labeling: 
    separate(name, c("First", "Last"), sep = " ", remove = FALSE) %>%
    mutate(First = substr(First, 1,1) 
           , Last = substr(Last, 1,1)) %>%
    unite("plyr.temp", First, Last, sep = "", remove = TRUE) %>%
    unite("plyr.lbl", plyr.temp, no., sep = " ", remove = FALSE) %>%
    unite("player", no., name, sep = " ", remove = FALSE) %>%
    select(-plyr.temp) %>%
    mutate(team = str_replace_all(team, c('[+]' =' '
                                          , '_' = ' '
                                          , '[-]' = ' ')))
    #mutate(team = gsub("+", " ", team, fixed = TRUE))
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
               , group = player, color = player)) +
    
    geom_line(data = dta 
              , mapping = aes(x = game, y = !!quo_var
                              , group = factor(player))
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
                group_by(player) %>%
                top_n(1, as.numeric(game))),
              mapping = aes(x = game + 0.6, y = !!quo_var
                            , label = plyr.lbl, color = player)
              , size = 4.5)
  
}


# This function creates the player stats and comparison tables, given the temp.dta
create.stats.tbl <- function(df) {
  temp.df <- df %>%
          group_by(plyr.lbl) %>%
          top_n(1, Games) %>%
          t
        
  
  plyr.names <- temp.df[1, ]
  
  temp.df <- temp.df %>%
    data.frame()
  
  names(temp.df) <- plyr.names
  
  temp.df %>%
    rownames_to_column("Stat") %>%
    filter(Stat != "plyr.lbl")
}  
  

# This function takes a temp data set and gets the latest value for the given 
# val.var. Used for the value boxes and reactive input (e.g. input$player)
get.value <- function(temp.df, inp, val.var) {
  q.var <- enquo(val.var)
  
  temp.df %>% 
      filter(player == inp) %>%
      top_n(1, game) %>%
      select(!!q.var)
}

