#=================================================================================================#
#=================================================================================================#
#
#   YLL & Vaccines
#   Covid-19 deaths
#
#=================================================================================================#
#=================================================================================================#

# Description:
# Compiles data on covid-19 official death counts.



#=================================================================================================#
#
#   LIBRARIES
#  
#
#=================================================================================================#

library(readxl)
library(tidyverse)
library(lubridate)
library(osfr)


#=================================================================================================#
#
#   DATA
#  
#
#=================================================================================================#

# COVID-19 deaths 
#=================================================================================================#

# As many countries as possible


# ~~~~~~~~~
# INED data
# ~~~~~~~~~

# Loading some countries from INED
ined_db <- 
  read_csv("Data/covid_pooled_2021-03-05.csv")


# selecting data for all countries in the closest date to 15 January 2021.
ined_db2 <- 
  ined_db %>% 
  filter(!(country == "England & Wales" & excelsheet == "NHS_Daily_Data")) %>% 
  select(country, age_group, death_reference_date, cum_death_male, cum_death_female, cum_death_both) %>% 
  rename(Country = country,
         Date = death_reference_date,
         Age = age_group,
         b = cum_death_both,
         f = cum_death_female,
         m = cum_death_male) %>% 
  gather(b, m, f, key = Sex, value = Deaths) %>% 
  mutate(Date = dmy(Date),
         dist = Date - ymd("2021-01-15"),
         Country = case_when(Country == "England & Wales" ~ "England and Wales", 
                             Country == "United States" ~ "USA", 
                             TRUE ~ Country)) %>% 
  group_by(Country) %>% 
  filter(abs(dist) == min(abs(dist))) %>% 
  ungroup() %>% 
  arrange(Country, Sex) %>% 
  select(-dist) %>% 
  drop_na()

unique(ined_db2$Age)
ined_cts <- unique(ined_db2$Country)

ined_db3 <- ined_db2 %>% 
  filter(!Age %in% c("Total known", "Total unknown", "Total")) %>% 
  mutate(Age = str_sub(Age, 1, 2),
         Age = case_when(Age == "0-" ~ "0",
                         Age == "<1" ~ "0",
                         Age == "<4" ~ "0",
                         Age == "1-" ~ "1",
                         Age == "5-" ~ "5",
                         TRUE ~ Age),
         Age = as.integer(Age)) %>% 
  filter(!(Country %in% c("Sweden", "South Korea") & Sex != "b"))

unique(ined_db3$Age)
unique(ined_db3$Country)

# ~~~~~~~~~~~~~~~~
# COVerAGE-DB Data
# ~~~~~~~~~~~~~~~~

# adjusted version of Hungary
hungary_adj <- read_rds("Data/Hungary_adjusted.rds") %>% 
  mutate(AgeInt = as.character(AgeInt),
         Value = as.character(Value))

# # downloading data from OSF (as of 15.03.2021)
# osf_retrieve_file("9dsfk") %>%
#   osf_download(conflicts = "overwrite",
#                path = "Data")

cvg_db <-  read_csv("Data/InputDB.zip",
                skip = 1,
                col_types = cols(.default = "c")) %>%
  select(-Short) %>% 
  filter(Country != "Hungary") %>% 
  bind_rows(hungary_adj) %>% 
  mutate(Date = dmy(Date),
         Value = as.double(Value)) %>% 
  filter(Region == "All",
         Measure == "Deaths") %>%
  filter(Date > "2020-08-01") %>% 
  drop_na()

unique(cvg_db$Country)

cvg_db2 <- cvg_db %>%
  mutate(dist = Date - ymd("2021-01-15")) %>% 
  group_by(Country) %>% 
  filter(abs(dist) == min(abs(dist))) %>% 
  ungroup()


# taking care of double dates 
cvg_db2 %>% 
  filter(Sex == "b",
         Age == 0) %>% 
  group_by(Country) %>% 
  mutate(n = n()) %>% 
  filter(n > 1)
  
dates_country <- 
  cvg_db2 %>% 
  select(Country, Date) %>% 
  unique() %>% 
  group_by(Country) %>% 
  mutate(Date_inc = max(Date)) %>% 
  select(-Date) %>% 
  ungroup()

cvg_db3 <- cvg_db2 %>% 
  left_join(dates_country) %>% 
  filter(Date == Date_inc)
  
# Countries with data for total sex from input
cts_all_sex <- cvg_db3 %>% 
  filter(Sex == "b",
         Age != "TOT") %>% 
  pull(Country) %>% 
  unique()

# data for total sexes for those which are not in the input
cvg_sex <- cvg_db3 %>% 
  filter(Sex != "b",
         Age != "TOT",
         !Country %in% cts_all_sex) %>% 
  group_by(Country, Date, Age) %>% 
  summarise(Value = sum(Value)) %>% 
  ungroup() %>% 
  mutate(Sex = "b")

unique(cvg_sex$Country)

# merging new total sexes
cvg_db4 <- cvg_db3 %>% 
  select(Country, Date, Sex, Age, Value) %>% 
  filter(Age != "TOT",
         Age != "Unknown",
         Sex != "UNK") %>% 
  bind_rows(cvg_sex) %>% 
  mutate(Age = as.integer(Age)) %>% 
  arrange(Country, Sex, Age) %>% 
  rename(Deaths = Value)

unique(cvg_db4$Country)

total <- cvg_db4 %>% 
  filter(Sex == "b") %>% 
  group_by(Country) %>% 
  summarise(Deaths = sum(Deaths))

# Merging data from both sources 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# taking care of duplicates in INED and COVerAGE-DB
db <- 
  cvg_db4 %>% 
  filter(!Country %in% ined_cts) %>% 
  bind_rows(ined_db3) %>% 
  ungroup() %>% 
  # exluding only "England"
  filter(Country != "England")

# Selecting countries with minimum 500 deaths in all ages
cts_min_deaths <- 
  db %>% 
  filter(Sex == "b") %>% 
  group_by(Country) %>% 
  filter(sum(Deaths) > 500) %>% 
  pull(Country) %>% 
  unique() 

# function to close ages at maximum 95+ 
close_max_95 <- function(db, ct){
  int <- db %>% 
    filter(Country == ct) %>% 
    pull(Age) %>% 
    unique()
  
  incs <- int  %>% .[.<= 95]
  int2 <- c(incs, 110)
  labs <- int2[1:length(int2)-1]
  
  db2 <- db %>% 
    filter(Country == ct) %>% 
    mutate(Age_int = cut(Age, breaks = int2, include.lowest = TRUE, right = FALSE, labels = labs),
           Age_int = as.numeric(as.character(Age_int))) %>% 
    group_by(Country, Date, Sex, Age_int) %>% 
    summarise(Deaths = sum(Deaths)) %>% 
    ungroup() %>% 
    rename(Age = Age_int)
  
}

# closing age groups at max 95+, summarizing deaths in the last interval 
# and change sex "b" to "t"
db_deaths <- tibble()
for(ct in cts_min_deaths){
  db_temp <- 
    close_max_95(db, ct) %>% 
    mutate(Sex = ifelse(Sex == "b", "t", Sex))

  db_deaths <- 
    db_deaths %>% 
    bind_rows(db_temp)
  
}

unique(db_deaths$Country)


# grouping single-year ages into 5-year ages
cts_single <- c("Argentina", "Colombia", "Peru", "Mexico", "Czechia", 
                "Hungary", "India", "Paraguay", "Philippines")

db_singles <- 
  db_deaths %>% 
  filter(Country %in% cts_single) %>% 
  mutate(Age = floor(Age / 5) * 5) %>% 
  group_by(Country, Date, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup()

# adding Japan recent dates
jp <- read_csv("Data/Japan_recent_deaths.csv") %>% 
  mutate(Date = dmy(Date),
         Sex = "t",
         Country = "Japan") %>% 
  filter(Date == "2021-01-13")

db_deaths2 <- 
  db_deaths %>% 
  filter(!Country %in% cts_single,
         Country != "Japan") %>% 
  bind_rows(db_singles, jp) %>% 
  arrange(Country, Sex, Age)


#=====================================================================================================#
# Saving
#=====================================================================================================#

write_rds(db_deaths2, "Output/covid19_deaths.rds")
