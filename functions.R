# These are the supporting functions for the wfa data prep and dashboard.

# set the theme for all ggplots: ------------------------------------- 
theme_set(theme_classic() +
            theme(
              plot.title = element_text(hjust = 0.5)
              , axis.text = element_text(size = 10)
              , axis.title = element_text(size=14
                                          , face="bold")
              , plot.caption = element_text(hjust = 0.5)
              , legend.position = c(0.5, 1)
              , legend.direction = "horizontal"
              , legend.text = element_text(size = 11)
            ))



# Data prep functions -----------------------------------------------------
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
    group_by(name) %>%
    mutate(no. = first(no.)) %>%
    ungroup %>%
    unite("plyr.lbl", plyr.temp, no., sep = " ", remove = FALSE) %>%
    unite("player", no., name, sep = " ", remove = FALSE) %>%
    select(-plyr.temp) %>%
    mutate(team = str_replace_all(team, c('[+]' =' '
                                          , '_' = ' '
                                          , '[-]' = ' ')))
    #mutate(team = gsub("+", " ", team, fixed = TRUE))
}




# Plotting functions ------------------------------------------------------

# This function creates plots over the season by game and highlights the 
# selected players' paths
# Input: 
#    react.dta the reactive data,
#    dta: underlying data (all)
#    var: the variable to be plotted on the Y axis,
#    y.axis.lbl: the title of the y.axis
 
plot.var.by.game <- function(react.dta, dta, var, y.axis.lbl){

  # using tidyeval language
  quo_var <- enquo(var)
  
  # removing NAs, replacing with zeros for the variable of interest
  dta <- dta %>%
    mutate(y.var = replace_na(!!quo_var, 0))
  
  react.dta <- react.dta %>%
    mutate(y.var = replace_na(!!quo_var, 0))
  
  ylim.max <- dta %>% 
                select(y.var) %>%
                max
  
  react.dta %>%
    filter(game != 9) %>%
    ggplot(aes(x = game, y = y.var
               , group = plyr.lbl, color = plyr.lbl)) +
    
    # This section puts all players information on the plot
    # and adds the league for the season
    geom_line(data = dta %>%
                filter(game != 9) 
              , mapping = aes(x = game, y = y.var
                              , group = factor(plyr.lbl))
              , color = "grey") +
    geom_line(data = dta %>%
                filter(game != 9) 
              , mapping = aes(y = mean(y.var), group = NULL
                              , color = NULL)
              , color = "black", size = 1.05) +
    # stat_summary(data = dta %>% 
    #                filter(game != 9)
    #              , mapping = aes(x = game, y = y.var,
    #                              color = NULL, group = NULL)
    #              , fun.y = "mean", color = "black"
    #          , size = 1, geom = "line") +
    geom_point(size = 1) +
    geom_line(size = 1.2) +
    labs(x = "Game"
         , y = y.axis.lbl
         , caption = paste0("Grey lines represent all non-selected players.\n"
                             , "The black line represents the league average per game.")
         , color = "") +
    coord_cartesian(xlim = c(1, 8)
                    , ylim = c(0, ylim.max)) +
    scale_x_continuous(breaks = seq(1,8,1)) +
    # geom_text(data = (react.dta %>%
    #             group_by(player) %>%
    #             top_n(-1, game)),
    #           mapping = aes(x = game - 1, y = y.var
    #                         , label = plyr.lbl, color = plyr.lbl)
    #           , size = 4, fontface = "bold", angle = 30) +
    scale_color_manual(values = c("blue", "darkgreen"))
  
}

# This function plots the season to date values:
# Input: 
#    react.df the reactive data,
#    dash.df: underlying data (all)
#    x.var: the variable to be plotted on the x axis,
#    y.var: the variable to be plotted on the y axis
#    x.axis.lbl: the title of the x.axis
#    y.axis.lbl: the title of the y.axis

plot.std <- function(react.df, dash.df, x.var, y.var
                     , x.axis.lbl, y.axis.lbl) {
  
  quo_x <- enquo(x.var)
  quo_y <- enquo(y.var)
  # To get the x,y axis limits:  
  x.min <- c(dash.df %>% 
                group_by(plyr.lbl) %>%
                top_n(1, game) %>%
                ungroup %>%
                select(!!quo_x) %>%
                min)
  
  x.max <- c(dash.df %>% 
               select(!!quo_x) %>%
               max)
  
  y.min <- c(dash.df %>% 
               group_by(plyr.lbl) %>%
               top_n(1, game) %>%
               ungroup %>%
               select(!!quo_y) %>%
               min)
  
  y.max <- c(dash.df %>% 
               select(!!quo_y) %>%
               max)
  
   p <- react.df %>%       
    group_by(player) %>%      
    top_n(1, game) %>% 
    ggplot(aes(x = !!quo_x, y = !!quo_y
               , color = factor(plyr.lbl))) +
    
    # plots all of the players in grey with a red line for the lm
    geom_point(data = dash.df %>%
                         group_by(plyr.lbl) %>%
                         top_n(1, game)
               , mapping = aes(x = !!quo_x, y = !!quo_y
                               , color = NULL)
               , color = "grey") +
    geom_smooth(data = dash.df %>%
                          group_by(player) %>%
                          top_n(1, game)
                , mapping = aes(x = !!quo_x, y = !!quo_y
                                , color = NULL)
                , method = "lm", se = FALSE, color = "black") +
    
    # plot the selected players
    geom_point(size = 3) +
    labs(x = x.axis.lbl, y = y.axis.lbl
         , caption = paste0("Grey dots represent all non-selected players.\n"
                            , "The black line represents the mean trend.")
         , color = "") +
    coord_cartesian(xlim = c(x.min, x.max)
                    , ylim = c(y.min, y.max)) +
    scale_color_manual(values = c("blue", "darkgreen"))
 p
  
}


# Table Functions ---------------------------------------------------------
# This function creates the player stats and comparison tables, given the temp.dta
create.stats.tbl <- function(df) {
  temp.df <- df %>%
          group_by(plyr.lbl) %>%
          filter(row_number() == n()) %>%
          #top_n(1, Games) %>%
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

