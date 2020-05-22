### Wrapping functions to support MN senate analysis.
### May 21 2020
###

library(tidyverse)
library(tidycensus)
library(httr)
library(magrittr)
library(stringr)
library(modelr)
library(sf)

MN_senate_election_district_pull <- function(url, year){
  ## Pulls data from MN elections website following the MN senate format
  ## year value is provided and added to dataset
  senate_election_colnames <- c("State", "County ID (if applicable)", "Precinct name (if applicable)", "Office ID",
                                "Office Name", "District", "Candidate ID", "Candidate Name (First/Last/Suffix all in one field)", 
                                "Suffix (not used)", "Incumbent Code (not used)", "Party Abbreviation", "Number of Precincts reporting",
                                "Total number of precincts voting for the office", "Votes for Candidate", 
                                "Percentage of Votes for Candidate out of Total Votes for Office", "Total number of votes for Office in area")
  
  temp <- GET(url)
  data_raw <- content(temp, "text", encoding = "ISO-8859-1")
  data_read <- read_delim(data_raw, delim = ";", col_names = FALSE)
  colnames(data_read) <- senate_election_colnames
  data_read$District <- as.character(data_read$District)
  data_read %<>% mutate(year = year)
  return(data_read)
}

acs_needed_vars_wrapper <- function(geography, state, year, survey){
  ## Pulls ACS data from the census API using tidycensus
  ## No variable selection from outside of function
  vars <- load_variables(year, survey, cache = TRUE)
  
  ## DATA AT THIS LEVEL ONLY AVAILABE IN ACS5
  age_vars <- filter(vars, concept == "SEX BY AGE" | 
                       concept == "SEX BY AGE (WHITE ALONE)" |
                       concept == "SEX BY AGE (BLACK OR AFRICAN AMERICAN ALONE)" |
                       concept == "SEX BY AGE (AMERICAN INDIAN AND ALASKA NATIVE ALONE)" |
                       concept == "SEX BY AGE (ASIAN ALONE)" |
                       concept == "SEX BY AGE (SOME OTHER RACE ALONE)" |
                       concept == "MEDIAN AGE BY SEX" |
                       concept == "HISPANIC OR LATINO ORIGIN BY SPECIFIC ORIGIN" |
                       concept == "NATIVITY AND CITIZENSHIP STATUS IN THE UNITED STATES" |
                       name == "B19013_001")  %>% select(name) %>% unlist() %>% unname()
  
  mn_acs5 <- get_acs(geography = geography, 
                     variables = age_vars, 
                     state = state, 
                     year = year, 
                     survey = survey) %>% mutate(year = year)
  
  
  ## CALCULATE MALE AND FEMALE TOTALS
  mn_acs5_processed <- mn_acs5 %>% pivot_wider(id_cols = c("GEOID", "NAME", "year"), 
                                               names_from = variable, values_from = estimate) %>%
    mutate(women_voting_age = B01001_031 + B01001_032 + B01001_033 + B01001_034 +
             B01001_035 + B01001_036 + B01001_037 + B01001_038 +
             B01001_039 + B01001_040 + B01001_041 + B01001_042 +
             B01001_043 + B01001_044 + B01001_045 + B01001_046 +
             B01001_047 + B01001_048 + B01001_049,
           men_voting_age =   B01001_007 + B01001_008 + B01001_009 + B01001_010 +
             B01001_011 + B01001_012 + B01001_013 + B01001_014 +
             B01001_015 + B01001_016 + B01001_017 + B01001_018 +
             B01001_019 + B01001_020 + B01001_021 + B01001_022 +
             B01001_023 + B01001_024 + B01001_025,
           voting_age_pop = women_voting_age + men_voting_age)
  
  ## CALCULATE WHITE MALE AND FEMALE TOTALS
  mn_acs5_processed <- mn_acs5_processed %>% 
    mutate(white_women_voting_age = B01001A_022 + B01001A_023 + B01001A_024 + B01001A_025 + 
             B01001A_026 + B01001A_027 + B01001A_028 + B01001A_029 + 
             B01001A_030 + B01001A_031,
           white_men_voting_age =   B01001A_007 + B01001A_008 + B01001A_009 + B01001A_010 + 
             B01001A_011 + B01001A_012 + B01001A_013 + B01001A_014 + 
             B01001A_015 + B01001A_016,
           white_voting_age_pop = white_women_voting_age + white_men_voting_age)
  
  ## CALCULATE BLACK MALE AND FEMALE TOTALS
  mn_acs5_processed <- mn_acs5_processed %>% 
    mutate(black_women_voting_age = B01001B_022 + B01001B_023 + B01001B_024 + B01001B_025 + 
             B01001B_026 + B01001B_027 + B01001B_028 + B01001B_029 + 
             B01001B_030 + B01001B_031,
           black_men_voting_age =   B01001B_007 + B01001B_008 + B01001B_009 + B01001B_010 + 
             B01001B_011 + B01001B_012 + B01001B_013 + B01001B_014 + 
             B01001B_015 + B01001B_016,
           black_voting_age_pop = black_women_voting_age + black_men_voting_age)
  
  
  ## CALCULATE NATIVE MALE AND FEMALE TOTALS
  mn_acs5_processed <- mn_acs5_processed %>% 
    mutate(native_women_voting_age = B01001C_022 + B01001C_023 + B01001C_024 + B01001C_025 + 
             B01001C_026 + B01001C_027 + B01001C_028 + B01001C_029 + 
             B01001C_030 + B01001C_031,
           native_men_voting_age =   B01001C_007 + B01001C_008 + B01001C_009 + B01001C_010 + 
             B01001C_011 + B01001C_012 + B01001C_013 + B01001C_014 + 
             B01001C_015 + B01001C_016,
           native_voting_age_pop = native_women_voting_age + native_men_voting_age)
  
  ## CALCULATE ASIAN MALE AND FEMALE TOTALS
  mn_acs5_processed <- mn_acs5_processed %>% 
    mutate(asian_women_voting_age = B01001D_022 + B01001D_023 + B01001D_024 + B01001D_025 + 
             B01001D_026 + B01001D_027 + B01001D_028 + B01001D_029 + 
             B01001D_030 + B01001D_031,
           asian_men_voting_age =   B01001D_007 + B01001D_008 + B01001D_009 + B01001D_010 + 
             B01001D_011 + B01001D_012 + B01001D_013 + B01001D_014 + 
             B01001D_015 + B01001D_016,
           asian_voting_age_pop = asian_women_voting_age + asian_men_voting_age)
  
  
  ## CALCULATE OTHER MALE AND FEMALE TOTALS
  mn_acs5_processed <- mn_acs5_processed %>% 
    mutate(other_women_voting_age =  B01001F_022 + B01001F_023 + B01001F_024 + B01001F_025 + 
             B01001F_026 + B01001F_027 + B01001F_028 + B01001F_029 + 
             B01001F_030 + B01001F_031,
           other_men_voting_age =    B01001F_007 + B01001F_008 + B01001F_009 + B01001F_010 + 
             B01001F_011 + B01001F_012 + B01001F_013 + B01001F_014 + 
             B01001F_015 + B01001F_016,
           other_voting_age_pop = other_women_voting_age + other_men_voting_age)
  
  return(mn_acs5_processed)
  
}