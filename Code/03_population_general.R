#=================================================================================================#
#=================================================================================================#
#
#   YLL & Vaccines
#   Population counts
#
#=================================================================================================#
#=================================================================================================#

# Description:
# Creates population counts for the different age groups.


#=================================================================================================#
#
#   LIBRARIES
#  
#
#=================================================================================================#

library(tidyverse)
library(here)
library(HMDHFDplus)
library(readxl)

#=================================================================================================#
#
#   DATA
#  
#
#=================================================================================================#

# Countries
#=================================================================================================#

covid19_deaths <- read_rds("Output/covid19_deaths.rds") %>%
  mutate(Country = ifelse(Country == "Moldova", "Republic of Moldova", Country))
hmd_cts_cds <- read_csv("Data/country_codes.csv") 

cts_all <- covid19_deaths %>% 
  select(Country) %>% 
  unique() 

cts_hmd <- cts_all %>% 
  left_join(hmd_cts_cds) %>% 
  drop_na() %>% 
  select(Country, hmd_code)

# list of countries with data in HMD
cts_hmd %>% 
  dplyr::pull(Country)

# HMD codes
cds_hmd <- cts_hmd %>% 
  dplyr::pull(hmd_code)


# countries that need data from WPP
cts_wpp <- cts_all %>% 
  left_join(hmd_cts_cds) %>% 
  filter(is.na(hmd_code)) %>%
  pull(Country)

cts_wpp


# Population - HMD
#=================================================================================================#

# User and password
if(!'hmd_us' %in% ls()){
  cat("Please instert your HMD User:")
  hmd_us <- userInput()
}
if(!'hmd_pw' %in% ls()){
  cat("Please instert your HMD password:")
  hmd_pw <- userInput()
}
ct <- "USA"
# Our sample
pop_hmd <- tibble()
for(ct in cds_hmd){
  chunk <- readHMDweb(ct, "Population", hmd_us, hmd_pw) %>%
    filter(Year == max(Year)) %>%
    select(Year, Age, Female1, Male1, Total1) %>%
    rename(f = Female1,
           m = Male1,
           t = Total1) %>% 
    gather(-Year, -Age, key = Sex, value = Population) %>% 
    mutate(Code = ct) %>%
    as_tibble()
  pop_hmd <- pop_hmd %>%
    bind_rows(chunk)
}

# Renaming columns

pop_hmd2 <- pop_hmd %>% 
  rename(hmd_code = Code) %>% 
  left_join(cts_hmd) %>% 
  select(-hmd_code)


# Population - WPP
#=================================================================================================#
pop_wpp_t <- 
  read_xlsx("Data/WPP2019_INT_F03_1_POPULATION_BY_AGE_ANNUAL_BOTH_SEXES.xlsx",
            skip = 16) %>% 
  select(3, 8:109) %>% 
  rename(Country = 1, 
         Year = 2) %>% 
  filter(Country %in% cts_wpp, 
         Year == 2020) %>% 
  gather(-Country, -Year, key = "Age", value = "Population") %>% 
  mutate(Age = as.integer(Age),
         Population = as.double(Population) * 1000,
         Sex = "t") %>% 
  arrange(Country, Age)

pop_wpp_f <- 
  read_xlsx("Data/WPP2019_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.xlsx",
            skip = 16) %>% 
  select(3, 8:109) %>% 
  rename(Country = 1, 
         Year = 2) %>% 
  filter(Country %in% cts_wpp, 
         Year == 2020) %>% 
  gather(-Country, -Year, key = "Age", value = "Population") %>% 
  mutate(Age = as.integer(Age),
         Population = as.double(Population) * 1000,
         Sex = "f") %>% 
  arrange(Country, Age)

pop_wpp_m <- 
  read_xlsx("Data/WPP2019_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.xlsx",
            skip = 16) %>% 
  select(3, 8:109) %>% 
  rename(Country = 1, 
         Year = 2) %>% 
  filter(Country %in% cts_wpp, 
         Year == 2020) %>% 
  gather(-Country, -Year, key = "Age", value = "Population") %>% 
  mutate(Age = as.integer(Age),
         Population = as.double(Population) * 1000,
         Sex = "m") %>% 
  arrange(Country, Age)


pop_wpp <- 
  bind_rows(pop_wpp_t, pop_wpp_f, pop_wpp_m)

# merging HMD and WPP population estimates
pop <- bind_rows(pop_wpp, pop_hmd2)

# saving population in sigle-year ages
write_rds(pop, "Output/pop_single_year_age.rds")

#=================================================================================================#
#
#   ASSIGNING AGE GROUPS
#  
#
#=================================================================================================#

# Function for grouping population age groups in same categories as deaths 
assign_age_intervals <- function(ct){

  int <- covid19_deaths %>% 
    filter(Country == ct) %>% 
    pull(Age) %>% 
    unique()
  
  if(max(int) <= 110){
    int <- c(int, 110)
  }
  
  labs <- int[1:length(int)-1]
  
  pop_int <- pop %>% 
    filter(Country == ct) %>% 
    mutate(Age_int = cut(Age, breaks = int, include.lowest = TRUE, right = FALSE, labels = labs),
           Age_int = as.numeric(as.character(Age_int)))
  
}

# grouping population age groups in same categories as deaths 
cts <- unique(covid19_deaths$Country)
pop_ints <- tibble()
for(ct in cts){
  
  temp_pop <- assign_age_intervals(ct)

  pop_ints <- pop_ints %>% 
    bind_rows(temp_pop)
}

# summarizing population estimates in each age interval
pop_all <- pop_ints %>% 
  group_by(Country, Year, Sex, Age_int) %>% 
  summarise(Population = sum(Population)) %>% 
  ungroup() %>% 
  rename(Age = Age_int) %>% 
  mutate(Country = ifelse(Country == "Republic of Moldova", "Moldova", Country))


#=====================================================================================================#
# Saving
#=====================================================================================================#

saveRDS(pop_all, "Output/pop_all.rds")

