#Libraries

library(data.table)
library(tidyverse)
library(purrr)
library(lubridate)

#Variable definition

#RODCIS2 = unique personal identifier
#DATPRI = date of admission
#DATUKO = date of discharge
#VEK = age
#POHL = sex
#NBYDL = region of residence
#ZDG = primary diagnosis
#DAUMR = date of death

#Import all hospitalizations from 1994 to 2017

hospitalizations_1994_2015 <- list.files(path = "path/UZIS_data_raw", 
                                         full.names = TRUE) %>%
  .[str_detect(., "1[6-7].csv$", negate = TRUE)] %>%
  map_dfr(~ fread(.,
                  select = c("RODCIS2" = "character", 
                             "DATPRI" = "integer", 
                             "DATUKO" = "integer", 
                             "VEK"= "integer", 
                             "POHL" = "integer",
                             "NBYDL"= "character",
                             "ZDG" = "character",
                             "DDG2" = "character",
                             "DDG3" = "character",
                             "DDG4" = "character",
                             "DDG5" = "character"),
                  header = TRUE,
                  sep = ",",
                  dec = ".",
                  fill = TRUE,
                  encoding = "Latin-1",
                  nThread = 8))

hospitalizations_2016_2017 <- list.files(path = "path/UZIS_data_raw", 
                                         full.names = TRUE) %>%
  .[str_detect(., "1[6-7].csv$", negate = FALSE)] %>%
  map_dfr(~ fread(.,
                  select = c("RODCIS" = "character", 
                             "DATPRI" = "integer", 
                             "DATUKO" = "integer", 
                             "VEK"= "integer", 
                             "POHL" = "integer",
                             "NBYDL"= "character",
                             "ZDG" = "character",
                             "DDG2" = "character"),
                  header = TRUE,
                  sep = ";",
                  dec = ".",
                  fill = TRUE,
                  encoding = "Latin-1",
                  nThread = 8)) %>%
  rename(RODCIS2 = RODCIS)

#Combine all data 

hospitalizations_1994_2017 <- hospitalizations_1994_2015 %>%
  bind_rows(hospitalizations_2016_2017)

#Remove partial data

rm(hospitalizations_1994_2015)
rm(hospitalizations_2016_2017)

#Import data on mortality

deaths_1994_2013 <- fread(file = "path/UZIS_mortality_raw/zem_1994_2013.csv")
deaths_2014 <- fread(file = "path/UZIS_mortality_raw/zem_2014.csv")
deaths_2015 <- fread(file = "path/UZIS_mortality_raw/zem_2015.csv")
deaths_2016 <- fread(file = "path/UZIS_mortality_raw/zem_2016.csv")
deaths_2017 <- fread(file = "path/UZIS_mortality_raw/zem_2017.csv")

#Unifying the format of mortality data

deaths_1994_2017 <- deaths_1994_2013 %>%
  transmute(RODCIS2 = RC,
            DAUMR = dmy(DAUMR),
            cause_of_death = trimws(DGP),
            external_cause_of_death = trimws(DGE)) %>%
  bind_rows(deaths_2014 %>%
              transmute(RODCIS2 = RC,
                        DAUMR = dmy(DAUMR),
                        cause_of_death = trimws(DGP),
                        external_cause_of_death = trimws(DGE)),
            deaths_2015 %>%
              transmute(RODCIS2 = RODCIS2,
                        DAUMR = ymd(DAUMR),
                        cause_of_death = trimws(DGP),
                        external_cause_of_death = trimws(DGE)),
            deaths_2016 %>%
              transmute(RODCIS2 = RCZEMAN2,
                        DAUMR = ymd(paste0(UMROK, str_pad(UMRMM, 2, pad = "0"), UMRDD)),
                        cause_of_death = trimws(DGUMR),
                        external_cause_of_death = trimws(DGUMR2)),
            deaths_2017 %>%
              transmute(RODCIS2 = RCZEMAN2,
                        DAUMR = ymd(paste0(UMROK, str_pad(UMRMM, 2, pad = "0"), UMRDD)),
                        cause_of_death = trimws(DGUMR),
                        external_cause_of_death = trimws(DGUMR2)))

#Remove partial data

rm(deaths_1994_2013)
rm(deaths_2014)
rm(deaths_2015)
rm(deaths_2016)
rm(deaths_2017)

#Excluding records with missing values on key variables
#Excluding records with invalid dates

hospitalizations_1994_2017 <- hospitalizations_1994_2017 %>%
  filter(rowSums(is.na(across(c(RODCIS2, DATPRI, DATUKO, VEK, POHL, NBYDL, ZDG)))) == 0) %>%
  filter(!is.na(ymd(DATPRI)) & !is.na(ymd(DATUKO))) 

#Excluding individuals with more than one date of death
#Excluding individuals with hospitalizations after death

hospitalizations_1994_2017 <- hospitalizations_1994_2017 %>%
  anti_join(hospitalizations_1994_2017 %>%
              inner_join(deaths_1994_2017, 
                         by = c("RODCIS2" = "RODCIS2")) %>%
              mutate(DATUKO = ymd(DATUKO)) %>%
              group_by(RODCIS2) %>%
              filter(DAUMR < max(DATUKO) | n_distinct(DAUMR) > 1) %>%
              ungroup(),
            by = c("RODCIS2" = "RODCIS2"))

#Excluding records with invalid date overlaps (discharge date after the admission date of another record)

hospitalizations_1994_2017 <- hospitalizations_1994_2017 %>%
  anti_join(map_dfr(.x = hospitalizations_1994_2017 %>%
                      group_split(split_ID = frank(RODCIS2, ties.method = "dense") %/% 10000),
                    ~ .x %>% 
                      select(RODCIS2,
                             DATPRI_index = DATPRI,
                             DATUKO_index = DATUKO) %>%
                      inner_join(.x %>%
                                   select(RODCIS2,
                                          DATPRI_non_index = DATPRI,
                                          DATUKO_non_index = DATUKO), 
                                 by = c("RODCIS2" = "RODCIS2")) %>%
                      filter((DATPRI_non_index < DATPRI_index & DATUKO_non_index > DATPRI_index) | (DATPRI_non_index == DATPRI_index & DATUKO_non_index > DATUKO_index)) %>%
                      pivot_longer(-RODCIS2,
                                   names_to = c(".value", "type"), 
                                   names_pattern = "([^_]+)_(.*)")),
            by = c("RODCIS2" = "RODCIS2",
                   "DATPRI" = "DATPRI",
                   "DATUKO" = "DATUKO"))

#Add row identifier and remove whitespace

hospitalizations_1994_2017 <- hospitalizations_1994_2017 %>%
  mutate(rowid = row_number()) %>%
  mutate(ZDG = trimws(ZDG),
         DDG2 = trimws(DDG2),
         DDG3 = trimws(DDG3),
         DDG4 = trimws(DDG4),
         DDG5 = trimws(DDG5)) 

#Vector or rheumatic diagnoses

rheuma_codes_vec <- set_names(c("^M05|^M06[0, 2-4, 8-9]|^M45|^M468|^M469", 
                                "^M05|^M06[0, 2-4, 8-9]",
                                "^M45|^M468|^M469"),
                              c("Rheumatoid arthritis or ankylosing spondylitis",
                                "Rheumatoid arthritis",
                                "Ankylosing spondylitis"))

#Vector of mental disorders codes 

psychiatric_codes_vec <- set_names(c("^F0|^G20|^G30", 
                                     "^F1", 
                                     "^F2", 
                                     "^F3", 
                                     "^F4",
                                     "^F5",
                                     "^F00|^G30",
                                     "^F10", 
                                     "^F11|^F12|^F13|^F14|^F15|^F16|^F17|^F18|^F19", 
                                     "^F20",
                                     "^F31", 
                                     "^F32|^F33", 
                                     "F32[1-3]|F33[1-3]",
                                     "^F41", 
                                     "^F43", 
                                     "^F45",
                                     "^F48"),
                                   c("Organic disorders",
                                     "Substance use disorders",
                                     "Psychotic disorders",
                                     "Mood disorders",
                                     "Anxiety disorders",
                                     "Behavioural syndromes",
                                     "Alzheimer’s disease",
                                     "Alcohol use disorders",
                                     "Drug use disorders",
                                     "Schizophrenia",
                                     "Bipolar disorder",
                                     "Depression",
                                     "Moderate or severe depression",
                                     "Other anxiety disorders",
                                     "Reaction to severe stress",
                                     "Somatoform disorders",
                                     "Other neurotic disorders"))

# Selecting rheumatic hospitalizations

hospitalizations_1999_2012_rheuma <- map(rheuma_codes_vec,
                                         function(rheuma_codes) {
                                           
                                           hospitalizations_1994_2017 %>%
                                                   filter(year(ymd(DATPRI)) >= 1999 & year(ymd(DATUKO)) <= 2012) %>%
                                                   filter(grepl(rheuma_codes, ZDG) | 
                                                            grepl(rheuma_codes, DDG2) | 
                                                            grepl(rheuma_codes, DDG3) | 
                                                            grepl(rheuma_codes, DDG4) | 
                                                            grepl(rheuma_codes, DDG5)) %>%
                                                   filter(VEK >= 18)
                                         })

#Selecting the first hospitalization in the examined time period
#Excluding individuals with residence outside of Czechia

set.seed(123)
first_hospitalizations_rheuma_1999_2012 <- map(hospitalizations_1999_2012_rheuma,
                                               function(data_rheuma_dg) {
                                                
                                                       data_rheuma_dg %>%
                                                         group_by(RODCIS2, DATPRI, DATUKO) %>%
                                                         filter(row_number() == sample(1:n(), 1)) %>%
                                                         group_by(RODCIS2) %>%
                                                         filter(DATPRI == min(DATPRI)) %>%
                                                         filter(DATUKO == min(DATUKO)) %>%
                                                         ungroup() %>%
                                                         filter(!grepl("^99", NBYDL))
                                               })

rm(hospitalizations_1999_2012_rheuma)

#Establishing the unexposed cohort 
#Selecting hospitalizations between 1999 and 2012, but not for the specific rheumatic and psychiatric disorder
#Selecting individuals aged 18+
#Randomly selecting one record when multiple records with the same admission and discharge date are present
#Excluding individuals with history of hospitalization for specific rheumatic disroder five or more years before
#Exact matching on age, sex, month and year at discharge
#Excluding individuals with residence outside of Czechia
#Randomly select five unexposed counterparts

set.seed(123)
all_matched_sampled_pairs <- map2(first_hospitalizations_rheuma_1999_2012,
                                  rheuma_codes_vec,
                                  function(data_rheuma_dg, rheuma_codes) {
                                    
                                    hospitalizations_unexp_1999_2012 <- hospitalizations_1994_2017  %>%
                                      filter(year(ymd(DATPRI)) >= 1999 & year(ymd(DATUKO)) <= 2012) %>%
                                      filter(!grepl(rheuma_codes, ZDG) & 
                                               !grepl(rheuma_codes, DDG2) & 
                                               !grepl(rheuma_codes, DDG3) & 
                                               !grepl(rheuma_codes, DDG4) & 
                                               !grepl(rheuma_codes, DDG5)) %>%
                                      filter(VEK >= 18) %>%
                                      mutate(cond = DATUKO < data_rheuma_dg$DATPRI[match(RODCIS2, data_rheuma_dg$RODCIS2)]) %>%
                                      filter(is.na(cond) | cond == TRUE) %>%
                                      select(-cond) 
                                    
                                    hospitalizations_unexp_without_past_rheuma_1999_2012 <- hospitalizations_unexp_1999_2012 %>%
                                      anti_join(hospitalizations_unexp_1999_2012 %>%
                                                  transmute(RODCIS2,
                                                            rowid,
                                                            washout_start = ymd(19940101),
                                                            washout_end = ymd(DATPRI)) %>%
                                                  inner_join(hospitalizations_1994_2017 %>%
                                                               filter(grepl(rheuma_codes, ZDG) | 
                                                                        grepl(rheuma_codes, DDG2) | 
                                                                        grepl(rheuma_codes, DDG3) | 
                                                                        grepl(rheuma_codes, DDG4) | 
                                                                        grepl(rheuma_codes, DDG5)) %>%
                                                               transmute(RODCIS2,
                                                                         historic_hospitalization_start = ymd(DATPRI),
                                                                         historic_hospitalization_end = ymd(DATUKO)),
                                                             by = c("RODCIS2" = "RODCIS2")) %>%
                                                  mutate(past_rheuma_hospitalizations = int_overlaps(interval(washout_start, washout_end),
                                                                                                     interval(historic_hospitalization_start, historic_hospitalization_end))) %>%
                                                  group_by(RODCIS2, rowid) %>%
                                                  filter(any(past_rheuma_hospitalizations == TRUE)) %>%
                                                  ungroup(),
                                                by = c("RODCIS2" = "RODCIS2", 
                                                       "rowid" = "rowid" ))
                                    
                                    matched_pairs <- data_rheuma_dg %>%
                                      transmute(RODCIS2_exposed = RODCIS2,
                                                VEK,
                                                POHL,
                                                month_discharge = month(ymd(DATUKO)),
                                                year_discharge = year(ymd(DATUKO))) %>%
                                      inner_join(hospitalizations_unexp_without_past_rheuma_1999_2012 %>%
                                                   mutate(month_discharge = month(ymd(DATUKO)),
                                                          year_discharge = year(ymd(DATUKO))),
                                                 by = c("VEK" = "VEK",
                                                        "POHL" = "POHL",
                                                        "month_discharge" = "month_discharge",
                                                        "year_discharge" = "year_discharge")) %>%
                                      filter(RODCIS2_exposed != RODCIS2) %>%
                                      filter(!grepl("^99", NBYDL))
                                    
                                    all_matched_sampled_pairs <- matched_pairs %>%
                                      group_by(RODCIS2) %>%
                                      filter(row_number() %in% sample(1:n(), 1)) %>%
                                      ungroup() %>%
                                      group_by(RODCIS2_exposed) %>%
                                      filter(row_number() %in% sample(1:n(), 5)) %>%
                                      ungroup()
                                    
                                    return(all_matched_sampled_pairs)
                                    
                                  })

#Sanity check
#Are all exposed individuals present?

map2(.x = map(first_hospitalizations_rheuma_1999_2012,
              function(data_rheuma_dg) {
                
                nrow(data_rheuma_dg)
              }),
     .y = map(all_matched_sampled_pairs,
              function(data_rheuma_dg) {
                
                 n_distinct(data_rheuma_dg$RODCIS2_exposed)
              }),
     ~ .x == .y)

#Combine data of people of rheumatic diseases and their matched counterparts

cohorts_baseline <- map2(first_hospitalizations_rheuma_1999_2012,
                         all_matched_sampled_pairs,
                         function(data_rheuma_dg, data_rheuma_dg_matched_counterparts) {
                           
                           data_rheuma_dg %>%
                                    mutate(RODCIS2_exposed = RODCIS2,
                                           exposure = "exposed",
                                           month_discharge = month(ymd(DATUKO)),
                                           year_discharge = year(ymd(DATUKO))) %>%
                                    bind_rows(data_rheuma_dg_matched_counterparts %>%
                                                mutate(exposure = "unexposed"))
                         })

#Establish the occurrence of the outcome

cohorts_baseline_with_outcome <- map(cohorts_baseline,
                                     function(data_rheuma_dg) {
                                       
                                       map(psychiatric_codes_vec,
                                           function(psychiatric_codes) {
                                              
                                             data_rheuma_dg %>%
                                                mutate(followup_start = ymd(DATUKO),
                                                       followup_end = ymd(20171231)) %>%
                                                inner_join(hospitalizations_1994_2017 %>%
                                                             transmute(RODCIS2,
                                                                       historic_hospitalization_start_numeric = DATPRI,
                                                                       historic_hospitalization_end_numeric = DATUKO,
                                                                       historic_hospitalization_start = ymd(DATPRI),
                                                                       historic_hospitalization_end = ymd(DATUKO),
                                                                       ZDG_follow_up = ZDG,
                                                                       DDG2_follow_up = DDG2,
                                                                       DDG3_follow_up = DDG3,
                                                                       DDG4_follow_up = DDG4,
                                                                       DDG5_follow_up = DDG5),
                                                           by = c("RODCIS2" = "RODCIS2")) %>%
                                                mutate(overlap = int_overlaps(interval(historic_hospitalization_start -1, historic_hospitalization_end -1),
                                                                              interval(followup_start, followup_end)),
                                                       outcome = overlap == TRUE & (grepl(psychiatric_codes, ZDG_follow_up) | 
                                                                                      grepl(psychiatric_codes, DDG2_follow_up) |
                                                                                      grepl(psychiatric_codes, DDG3_follow_up) |
                                                                                      grepl(psychiatric_codes, DDG4_follow_up) |
                                                                                      grepl(psychiatric_codes, DDG5_follow_up))) %>%
                                                group_by(across(RODCIS2:year_discharge)) %>%
                                                summarise(hospitalization_start_outcome = ifelse(any(outcome), min(historic_hospitalization_start_numeric[outcome == TRUE]), NA_integer_),
                                                          hospitalization_end_outcome = ifelse(any(outcome), min(historic_hospitalization_end_numeric[outcome == TRUE]), NA_integer_),
                                                          outcome = as.numeric(any(outcome))) %>%
                                                ungroup()
                                            })
                                     })

#Sanity check
#Are all matched individuals present?

map2(.x = map(cohorts_baseline,
              function(data_rheuma_dg) {
              
                          n_distinct(data_rheuma_dg$RODCIS2_exposed)
                }),
     .y = map(cohorts_baseline_with_outcome,
              function(data_rheuma_dg) {
                
                map_int(data_rheuma_dg,
                        function(data_rheuma_and_psychiatric_dg) {
                          
                          n_distinct(data_rheuma_and_psychiatric_dg$RODCIS2_exposed)
                        })
              }),
     ~ all(.x == .y))

#Establish mortality

data_main_analysis <- map(cohorts_baseline_with_outcome,
                          function(data_rheuma_dg) {
                            
                            map(data_rheuma_dg,
                                function(data_rheuma_and_psychiatric_dg) {
                                  
                                  data_rheuma_and_psychiatric_dg %>%
                                    left_join(deaths_1994_2017,
                                              by = c("RODCIS2" = "RODCIS2")) %>%
                                    mutate(years_diff_outcome = as.duration(ymd(DATUKO) %--% ymd(hospitalization_start_outcome))/dyears(1),
                                           years_diff_mortality = as.duration(ymd(DATUKO) %--% ymd(DAUMR))/dyears(1),
                                           years_diff_censoring = as.duration(ymd(DATUKO) %--% ymd("2017-12-31"))/dyears(1),
                                           years_until_outcome_or_censoring = ifelse(outcome == 1, 
                                                                                     years_diff_outcome,
                                                                                     pmin(years_diff_mortality, years_diff_censoring, na.rm = TRUE))) %>%
                                    mutate(exposure = factor(exposure, 
                                                             levels = c("unexposed", "exposed")))
                                })
                          })

#Export data

save(data_main_analysis, 
     file = "path/AZV_rheuma_risk_of_psych_hosp/Data/data_main_analysis.RData")
