#=================================================================================================#
#=================================================================================================#
#
#   YLL & Vaccines
#   YLL estimation using excess mortality
#
#=================================================================================================#
#=================================================================================================#

# Description:
# Estimate age-specific death rates, and YLL from excess mortality estimates in
# several countries from the HMD.

library(tidyverse)
library(here)
library(lubridate)

db_exc_stmf <- read_csv("Data/cumulative_excess_age_2020_2021.csv",
                        col_types = cols(.default = "c"))
# db_exc_mx_pe <- read_csv("Data/cumulative_excess_age_2020_2021_mx_pe.csv")
db_exc_orig <- read_csv("Data/cumulative_excess_original_ages_2020_2021.csv",
                        col_types = cols(.default = "c"))

# Confirmed deaths data to aggregate ages in the same intervals 
covid19_deaths <- read_rds("Output/covid19_deaths.rds") %>% 
  mutate(Country = ifelse(Country == "Moldova", "Republic of Moldova", Country))


# excluding a few countries were age distribution of deaths was heavily impossed
# to_exclude <- c("England_Wales", "Canada")
to_exclude <- c("Chile", "USA")

# adjusting some typos and values
db_exc2 <- db_exc_stmf %>% 
  mutate(Sex = recode(Sex,
                      "b" = "t")) %>% 
  filter(!Country %in% to_exclude,
         Sex == "t")

# selecting last available date in each country
db_exc3 <- 
  db_exc2 %>%
  bind_rows(db_exc_orig %>% 
              filter(Sex == "t")) %>% 
  mutate(Date = ymd(Date)) %>% 
  filter(Date <= "2021-01-17") %>% 
  group_by(Country) %>% 
  filter(Date == max(Date)) %>% 
  ungroup() %>% 
  rename(Excess = CumEpi) %>% 
  select(-CumExc, -CumPos, -Exposure) %>% 
  mutate(Excess = as.double(Excess),
         Age = as.integer(Age),
         Country = ifelse(Country == "United States", "USA", Country))

unique(db_exc3$Country)
# selecting countries with at least 500 deaths
cts_500plus <- 
  db_exc3 %>% 
  filter(Sex == "t") %>% 
  group_by(Country) %>% 
  filter(sum(Excess) >= 500) %>% 
  ungroup() %>% 
  pull(Country) %>% 
  unique()
  
db_exc4 <- 
  db_exc3 %>%
  filter(Country %in% cts_500plus) %>% 
  arrange(Country, Sex, Age)


# aggregate ages in the same configuration as confirmed deaths data
cts_excess <- 
  unique(db_exc4$Country)

ages_deaths <- covid19_deaths %>% 
  filter(Country %in% cts_excess) %>% 
  select(Country, Age) %>% 
  unique()


# Function for grouping population age groups in same intervals as deaths 
assign_age_intervals <- function(ct){
  
  int <- ages_deaths %>% 
    filter(Country == ct) %>% 
    pull(Age) %>% 
    unique()
  
  if(max(int) <= 110){
    int <- c(int, 110)
  }
  
  labs <- int[1:length(int)-1]
  
  exc_int <- db_exc4 %>% 
    filter(Country == ct) %>% 
    mutate(Age_int = cut(Age, breaks = int, include.lowest = TRUE, right = FALSE, labels = labs),
           Age_int = as.numeric(as.character(Age_int)))
  
}

# grouping population age groups in same categories as deaths 
cts <- unique(ages_deaths$Country)
exc_ints <- tibble()
for(ct in cts){
  
  temp_exc <- assign_age_intervals(ct)
  
  exc_ints <- exc_ints %>% 
    bind_rows(temp_exc)
}

# summarizing population estimates in each age interval
excess <- exc_ints %>% 
  group_by(Country, Date, Sex, Age_int) %>% 
  summarise(Excess = sum(Excess)) %>% 
  ungroup() %>% 
  rename(Age = Age_int) %>% 
  drop_na()

# saving excess deaths   
write_rds(excess, "Output/cumulative_excess_selected_countries.rds")
