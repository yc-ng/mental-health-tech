Pre-processing the Survey Data
================
YC Huang

-   [Introduction](#introduction)
-   [Setting up the Workspace](#setting-up-the-workspace)
-   [Looking at the Data](#looking-at-the-data)
-   [The Age Variable](#the-age-variable)
-   [The Gender Variable](#the-gender-variable)
-   [Handling Factor Variables](#handling-factor-variables)
-   [Conclusion](#conclusion)

Introduction
------------

The data is obtained from [OSMI's Survey on Mental Health in the Tech Workplace in 2014](https://www.kaggle.com/osmi/mental-health-in-tech-survey) and is available as a csv file.

The [data dictionary](data_dictionary.txt) for this dataset is also included for ease of reference.

Setting up the Workspace
------------------------

We will be using the following packages. The `scipen` option is set so that certain large numbers can be viewed as fixed format instead of scientific format.

``` r
library(readr)       # read data
library(dplyr)       # process the data
library(stringr)     # work with strings
library(forcats)     # handle factor variables
options(scipen = 15) # display large numbers as fixed
```

Looking at the Data
-------------------

The `survey` dataset has 1259 observations and 27 variables.

``` r
survey <- read_csv("data/survey.csv")
glimpse(survey)
```

    ## Observations: 1,259
    ## Variables: 27
    ## $ Timestamp                 <dttm> 2014-08-27 11:29:31, 2014-08-27 11:...
    ## $ Age                       <dbl> 37, 44, 32, 31, 31, 33, 35, 39, 42, ...
    ## $ Gender                    <chr> "Female", "M", "Male", "Male", "Male...
    ## $ Country                   <chr> "United States", "United States", "C...
    ## $ state                     <chr> "IL", "IN", NA, NA, "TX", "TN", "MI"...
    ## $ self_employed             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ family_history            <chr> "No", "No", "No", "Yes", "No", "Yes"...
    ## $ treatment                 <chr> "Yes", "No", "No", "Yes", "No", "No"...
    ## $ work_interfere            <chr> "Often", "Rarely", "Rarely", "Often"...
    ## $ no_employees              <chr> "6-25", "More than 1000", "6-25", "2...
    ## $ remote_work               <chr> "No", "No", "No", "No", "Yes", "No",...
    ## $ tech_company              <chr> "Yes", "No", "Yes", "Yes", "Yes", "Y...
    ## $ benefits                  <chr> "Yes", "Don't know", "No", "No", "Ye...
    ## $ care_options              <chr> "Not sure", "No", "No", "Yes", "No",...
    ## $ wellness_program          <chr> "No", "Don't know", "No", "No", "Don...
    ## $ seek_help                 <chr> "Yes", "Don't know", "No", "No", "Do...
    ## $ anonymity                 <chr> "Yes", "Don't know", "Don't know", "...
    ## $ leave                     <chr> "Somewhat easy", "Don't know", "Some...
    ## $ mental_health_consequence <chr> "No", "Maybe", "No", "Yes", "No", "N...
    ## $ phys_health_consequence   <chr> "No", "No", "No", "Yes", "No", "No",...
    ## $ coworkers                 <chr> "Some of them", "No", "Yes", "Some o...
    ## $ supervisor                <chr> "Yes", "No", "Yes", "No", "Yes", "Ye...
    ## $ mental_health_interview   <chr> "No", "No", "Yes", "Maybe", "Yes", "...
    ## $ phys_health_interview     <chr> "Maybe", "No", "Yes", "Maybe", "Yes"...
    ## $ mental_vs_physical        <chr> "Yes", "Don't know", "No", "No", "Do...
    ## $ obs_consequence           <chr> "No", "No", "No", "Yes", "No", "No",...
    ## $ comments                  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, ...

Many of the character type variables (`Gender` to `obs_consequence`) can be represented as categorical variables.

Note that not all the responses were submitted in 2014:

``` r
tail(survey$Timestamp)
```

    ## [1] "2015-08-25 19:59:38 UTC" "2015-09-12 11:17:21 UTC"
    ## [3] "2015-09-26 01:07:35 UTC" "2015-11-07 12:36:58 UTC"
    ## [5] "2015-11-30 21:25:06 UTC" "2016-02-01 23:04:31 UTC"

``` r
sum(survey$Timestamp >= "2015-01-01")
```

    ## [1] 69

There are certain variables that require cleaning if they are to be used for further analysis.

The Age Variable
----------------

The `summary()` quickly reveals some outliers:

``` r
summary(survey$Age)
```

    ##        Min.     1st Qu.      Median        Mean     3rd Qu.        Max. 
    ##       -1726          27          31    79428148          36 99999999999

Besides the most extreme values, there are some other abnormal values in this column:

``` r
head(sort(survey$Age, decreasing = T), 8) # highest outliers
```

    ## [1] 99999999999         329          72          65          62          61
    ## [7]          60          60

``` r
head(sort(survey$Age, decreasing = F), 8) # lowest/negative outliers
```

    ## [1] -1726   -29    -1     5     8    11    18    18

The 2 highest values & the negative ages are clearly invalid, and we should also ignore those ages 11 and below. Instead of throwing the entire observations away, we instead convert these values to `NA`:

``` r
survey_clean <- survey    # create a copy of the dataset for cleaning

survey_clean$Age[!(survey_clean$Age %in% 18:99)] <- NA  # only keep ages 18 to 99
```

The Gender Variable
-------------------

The Gender variable is a free-text field on the survey form, so it contains a variety of submissions.

``` r
head(table(survey$Gender), 10) # display only the first 10 responses
```

    ## 
    ## A little about you            Agender                All 
    ##                  1                  1                  1 
    ##          Androgyne   cis-female/femme         Cis Female 
    ##                  1                  1                  1 
    ##           cis male           Cis Male            Cis Man 
    ##                  1                  2                  1 
    ##               Enby 
    ##                  1

Some responses are misspelt, some refer to other genders, while some other responses have no ostensible meaning. Our approach to cleaning up this variable is as follows:

-   Convert all strings to lower case
-   Replace instances of synonyms (like woman to female, guy to male)
-   Capture key descriptors like 'trans', 'queer', 'non-binary'
    -   Due to small sample size, these genders would be grouped into a common category for further analysis.

We use the `stringr` package to construct a pipe of string replacements based on the above approach:

``` r
gender_clean <- survey$Gender %>%
    str_to_lower() %>%                          # convert to lowercase
    # 'woman' to 'female', 'man' to 'male'
    str_replace("\\bwoman\\b", "female") %>%    
    str_replace("\\bman\\b", "male") %>%
    # convert cis-genders to their respective genders
    str_replace("^cis[- ](female|male)", "\\1") %>%     
    str_replace("(female|male)[- ]\\(cis\\)", "\\1") %>% 
    # reword trans genders into a consistent format
    str_replace("(female|male).*trans.*", "trans \\1") %>% 
    str_replace("^trans-(female|male)", "trans \\1") %>% 
    # extract the first option from multiple answers separated by /
    str_replace("(\\b\\w+\\b)/.*", "\\1") %>% 
    # extract instances of 'queer'
    str_replace(".*(queer)$", "\\1") %>%
    # reword guys as males
    str_replace(".*\\b(?:male|guy)\\b.+", "male") %>% 
    # fix typos of male and female
    str_replace("^m\\w*", "male") %>% 
    str_replace("^fe?\\w*\\b", "female")
```

Due to small sample size, we will group responses that are not male/female into a single *other* category. Other meaningless/nonsensical responses are converted to *NA*:

``` r
others <- c("queer", "trans female","agender", "androgyne", "enby", "non-binary")
gender_clean[gender_clean %in% others] <- "other"
genders <- c("male", "female", "other")
gender_clean[!(gender_clean %in% genders)] <- NA
summary(factor(gender_clean))
```

    ## female   male  other   NA's 
    ##    248    995     11      5

``` r
# assign cleaned vector to dataset
survey_clean$Gender <- gender_clean
```

Handling Factor Variables
-------------------------

As `read_csv` imports all character columns as type `character`, we will need to identify the appropriate categorical variables and convert them into type `factor`. For this dataset, the variables occupy columns 3-26 (`Gender`, `Country`, `state`... `obs_consequence`).

``` r
# convert character variables into factor variables
survey_clean[3:26] <- lapply(survey_clean[3:26], as.factor)
```

Many of the factor variables consist of ordered responses: there is usually a "Yes" response, a "No" response, and there may be a middle response such as "Don't know" or "Maybe". We can thus reorder the factor levels for each of these variables. First, we assign the column indices according to the number of responses:

``` r
# column indexes based on how many factor levels 
col_2lvls <- c(7:8, 11:12, 26)
col_3lvls <- c(6, 13:17, 19:25)
col_many <- c(9, 10, 18)
```

We use `fct_relevel()` from the `forcats` package to reorder the levels for each of these variables:

``` r
# reorder 2-level factor columns as "Yes", "No"
survey_clean[col_2lvls] <- survey_clean[col_2lvls] %>% 
    lapply(fct_relevel, "Yes", "No")

# reorder 3-level factor columns as "Yes", (middle), "No"
survey_clean[col_3lvls] <- survey_clean[col_3lvls] %>% 
    lapply(fct_relevel, "Yes") %>% 
    lapply(fct_relevel, "No", after = 2)
```

For the few factor variables with other types of values, we would have to reorder them manually:

``` r
# manually reorder many-level columns
survey_clean$work_interfere <- survey_clean$work_interfere %>% 
    fct_relevel("Often", "Sometimes", "Rarely", "Never")
survey_clean$no_employees <- survey_clean$no_employees %>% 
    fct_relevel("1-5",     "6-25",     "26-100", 
                "100-500", "500-1000", "More than 1000")
survey_clean$leave <- survey_clean$leave %>% 
    fct_relevel("Very easy", "Somewhat easy", "Don't know", 
                "Somewhat difficult", "Very difficult")
```

Conclusion
----------

Now that we have cleaned up the dataset, we can use it for future analysis.

Since `write.csv()` and `write_csv()` coerces factor variables to character, we use `saveRDS()` to save our dataset as an RDS object (with specified column types) which we can load during subsequent sessions:

``` r
saveRDS(survey_clean, file = "data/survey.rds")
```

To summarise our data processing:

-   We handled outlier values in the `Age` variable
-   We cleaned up and standardized inputs in the `Gender` variable
-   We converted the necessary variables to factors
