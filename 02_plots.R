library(dplyr) # pipes
library(ggplot2) # plots
library(gridExtra) # arrange ggplots
library(purrr) # map plotting function
library(ggthemes) # plot themes

survey_clean <- readRDS("data/survey.rds")

# function to construct bar plot given the survey data
# input: column name as string/character
# output: ggplot object
plot_by_name <- function(col_name){
    ggplot(survey_clean, aes_string(col_name)) +
        geom_bar()
}

# construct and arrange bar plots of columns 6:17 (self_employed to anonymity)
survey_bar1 <- names(survey_clean)[c(6:17)] %>% 
    map(plot_by_name) %>%
    grid.arrange(grobs = ., ncol = 4)
# save to png file
ggsave("survey_bar1.png", plot = survey_bar1, device = "png",
           width = 50, height = 20, units = "cm")

# construct and arrange bar plots of columns 18-26 (leave to obs_consequence)
survey_bar2 <- names(survey_clean)[18:26] %>%
    map(plot_by_name) %>% 
    grid.arrange(grobs = ., ncol = 3) # 3x3 grid

ggsave("survey_bar2.png", plot = survey_bar2, device = "png",
       width = 40, height = 20, units = "cm")

# construct and arrange bar plots of columns 3 and 6-26 
# (gender, self_employed to obs_consequence)
survey_bar_all <- names(survey_clean)[c(3, 6:26)] %>%
    map(plot_by_name) %>% 
    grid.arrange(grobs = ., ncol = 4)

ggsave("survey_bar_all.png", plot = survey_bar_all, device = "png",
       width = 80, height = 40, units = "cm")



# Others ------------------------------------------------------------------

# plot Age
ggplot(survey_clean, aes(x = Age)) +
    geom_bar() +
    scale_x_continuous(breaks = seq(20, 70, 10))

# country (not very useful, a table does it better)
ggplot(survey_clean, aes(Country)) +
    geom_bar() +
    coord_flip()

# or i could lump the minor countries into 'Other'
library(forcats)
survey_clean %>% 
    mutate(country = fct_lump(Country, n = 9)) %>%  # lump uncommon responses
    group_by(country) %>% 
    summarise(total = n()) %>% 
    arrange(desc(total))

# plot country again
survey_clean %>% 
    mutate(country = fct_lump(Country, n = 9)) %>% 
    group_by(country) %>% 
    summarise(total = n()) %>% 
    # reordering the country variable so that the countries appear in order of
    # most frequent to least frequent
    mutate(country = fct_reorder(country, total)) %>%
    ggplot(aes(x = country, y = total)) +
        geom_bar(stat = "identity") +
        coord_flip()

# plotting for state
survey_clean %>% 
    filter(!is.na(state)) %>% 
    mutate(us_state = fct_lump(state, n = 10)) %>% 
    group_by(us_state) %>% 
    summarise(total = n()) %>%
    # reordering the country variable so that the countries appear in order of
    # most frequent to least frequent
    mutate(us_state = fct_reorder(us_state, total)) %>%
    ggplot(aes(x = us_state, y = total)) +
    geom_bar(stat = "identity") +
    coord_flip()
