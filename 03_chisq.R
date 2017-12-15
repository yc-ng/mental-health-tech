library(dplyr)

survey_clean <- readRDS("survey.rds")

attach(survey_clean)
table(work_interfere, no_employees, 
      deparse.level = 2) %>% 
    mosaicplot()

table(phys_health_consequence, mental_health_consequence,
      deparse.level = 2) %>% 
    chisq.test()

table(remote_work, leave, deparse.level = 2)

table(seek_help, no_employees, deparse.level = 2)

select(survey_clean, self_employed:family_history) %>% table()
select(survey_clean, self_employed:treatment) %>% table()
select(survey_clean, self_employed:work_interfere) %>% table()
select(survey_clean, self_employed:work_interfere) %>% ftable()
select(survey_clean, self_employed:treatment) %>% table() %>% prop.table()

