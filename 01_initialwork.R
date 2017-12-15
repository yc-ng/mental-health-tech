# data is obtained from 
# https://www.kaggle.com/osmi/mental-health-in-tech-survey


# Setup -------------------------------------------------------------------

setwd("~/R/mental-health-tech")
library(readr)       # read data
library(dplyr)       # data manipulation
library(stringr)     # string manipulation
library(forcats)     # factor manipulation
options(scipen = 15) # display large numbers as fixed

survey <- read_csv("survey.csv")


# looking at raw data -----------------------------------------------------

dim(survey) # 1259 x 27
glimpse(survey)
summary(survey)
View(survey)


# Timestamp ---------------------------------------------------------------

# 69 submissions were made after 2014
sum(survey$Timestamp >= "2015-01-01")

# The `Age` variable ---------------------------------------------------------

# $Age column has some very weird outliers
summary(survey$Age)
boxplot(survey$Age)
head(sort(survey$Age, decreasing = T), 12) # highest outliers
head(sort(survey$Age, decreasing = F), 12) # lowest/negative outliers

# replace invalid ages (outside 18-99) with NA
survey_clean <- survey
survey_clean$Age[!(survey_clean$Age %in% 18:99)] <- NA

# view the anomalous observations directly - are there joke submissions?
survey %>%
    filter(!Age %in% 18:99) %>% 
    View()

# visualizing valid ages
summary(survey_clean$Age)
hist(survey_clean$Age)
MASS::truehist(survey_clean$Age)


# State and Country -------------------------------------------------------

table(survey$Country)
summary(factor(survey$state))
sum(!is.na(survey$state))
# $state only valid for those in US
# 751 US respondents from $Country, 
# 744 respondents with state from $state

temp <- survey %>% 
    mutate(is_US = (Country == "United States"))
table(temp$is_US, temp$state)
# some non-US respondents put in state: maybe they live there but indicated 
# their birth country? or live in another country but indicate their home state?
# no way to find out (I think)
rm(temp)

# Gender ------------------------------------------------------------------

table(survey$Gender)
# $Gender column has many inputs, some with typos, this will need to be cleaned
# (taking into account other genders)

# convert to lowercase and tabulate
gender <- str_to_lower(survey$Gender)
table(gender)

# convert man/woman to male/female
woman <- "\\bwoman\\b"
# str_view(gender, woman, match = TRUE)
man <- "\\bman\\b"
# str_view(gender, man, match = TRUE)

# cis genders
cis1 <- "^cis[- ](female|male)"
# str_view(gender, cis1, match = TRUE)

cis2 <- "(female|male)[- ]\\(cis\\)"
# str_view(gender, cis2, match = TRUE)

# trans genders
trans1 <- "(female|male).*trans.*"
# str_view(gender, trans1, match = TRUE)

trans2 <- "^trans-(female|male)"
# str_view(gender, trans2, match = TRUE)

# inputs with forward slash
slash <- "(\\b\\w+\\b)/.*"
# str_view(gender, slash, match = TRUE)

# non-trans male-like (as described)
# syntax would actually capture "trans male" (but none found in this dataset)
malelike <- ".*\\b(?:male|guy)\\b.+"
# str_view(gender, malelike, match = TRUE)

# queer
queer <- ".*(queer)$"
# str_view(gender, queer, match = TRUE)

# males with or without typo, or shortened
males <- "^m\\w*"

# females with or without typo, or shortened
females <- "^fe?\\w*\\b"

# all replacements in one pipe
gender_clean <- gender %>%
    str_replace(woman, "female") %>% 
    str_replace(man, "male") %>% 
    str_replace(cis1, "\\1") %>% 
    str_replace(cis2, "\\1") %>% 
    str_replace(trans1, "trans \\1") %>% 
    str_replace(trans2, "trans \\1") %>% 
    str_replace(slash, "\\1") %>% 
    str_replace(queer, "\\1") %>% 
    str_replace(malelike, "male") %>%
    str_replace(males, "male") %>% 
    str_replace(females, "female")
table(gender_clean)
rm(woman, man, cis1, cis2, trans1, trans2, slash, 
   queer, malelike, males, females)

# Gender(2) ---------------------------------------------------------------

# gather remaining responses
others <- c("queer", "trans female","agender", "androgyne", "enby", "non-binary")
gender_clean[gender_clean %in% others] <- "other"
genders <- c("male", "female", "other")
gender_clean[!(gender_clean %in% genders)] <- NA
table(gender_clean)

# assign cleaned vector to dataset
survey_clean$Gender <- str_to_title(gender_clean)

# Factor variables -----------------------------------------------------------

# quick glance of character columns
lapply(survey_clean[3:5], function(x) summary(factor(x)))

lapply(survey_clean[6:10], function(x) summary(factor(x))) 

lapply(survey_clean[11:15], function(x) summary(factor(x)))

lapply(survey_clean[16:20], function(x) summary(factor(x)))

lapply(survey_clean[21:26], function(x) summary(factor(x))) 

# convert appropriate columns to factor
survey_clean[3:26] <- lapply(survey_clean[3:26], as.factor)

# column indexes based on how many factor levels 
col_2lvls <- c(7:8, 11:12, 26)
col_3lvls <- c(6, 13:17, 19:25)
col_many <- c(9, 10, 18)

# reorder 2-level factor columns as "Yes", "No"
survey_clean[col_2lvls] <- survey_clean[col_2lvls] %>% 
    lapply(fct_relevel, "Yes", "No")

# reorder 3-level factor columns as "Yes", (middle), "No"
survey_clean[col_3lvls] <- survey_clean[col_3lvls] %>% 
    lapply(fct_relevel, "Yes") %>% 
    lapply(fct_relevel, "No", after = 2)

# manually reorder many-level columns
survey_clean$work_interfere <- survey_clean$work_interfere %>% 
    fct_relevel("Often", "Sometimes", "Rarely", "Never")
survey_clean$no_employees <- survey_clean$no_employees %>% 
    fct_relevel("1-5",     "6-25",     "26-100", 
                "100-500", "500-1000", "More than 1000")
survey_clean$leave <- survey_clean$leave %>% 
    fct_relevel("Very easy", "Somewhat easy", "Don't know", 
                "Somewhat difficult", "Very difficult")

# Saving the cleaned data -------------------------------------------------

# save the cleaned dataset for later use
saveRDS(survey_clean, file = "data/survey.rds")

# reset options
options(scipen = 0)

# Other ideas -------------------------------------------------------------

# represent multiple bar charts for multiple variables at once
# factor level ordering is lost in the process
library(tidyr)
library(ggplot2)
survey_clean %>% 
    select(Timestamp:Age, self_employed:anonymity) %>% 
    gather(category, response, self_employed:anonymity) %>% 
    ggplot(aes(response)) +
    geom_bar() +
    facet_wrap(~category, scales = "free_x")

survey_clean %>% 
    select(Timestamp:Age, Gender, leave:obs_consequence) %>% 
    gather(category, response, Gender, leave:obs_consequence) %>% 
    ggplot(aes(response)) +
    geom_bar() +
    facet_wrap(~category, scales = "free_x")

# explore comments (163 that are not NA)
# potential for text analytics
survey$comments %>%
    `[`(!is.na(.)) %>%
    sample(5)

