#=================================================================================================#
#=================================================================================================#
#
#   YLL & Vaccines
#   Life expectancy
#
#=================================================================================================#
#=================================================================================================#

# Description:
# Assings remaining life expectancies to the relevant age groups.

# For non-open age group intervals, we pick the LE from single age life tables associated with 
# the middle of the interval (interpolated for age group intervals with odd number of individual ages)

# For the open interval, we do: 1) last age interval + LE at the beginning of the last age interval. 
# Assing to 1) LE at that single year of age.



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


# Countries
#=================================================================================================#
# Some countries require interpolation and cannot be taken directly from HMD
# countries_list <- unique(covid19_deaths$Country)
# countries_list_hmd <- c('Germany','South Korea','Ukraine', 'Denmark', 'Netherlands', 'USA', 'Czechia', 'Chile')
# countries_list_not_hmd <- c('Mexico', 'Colombia', 'Peru')

# SLE - HMD based
#=================================================================================================#
# Country specific life expectancies

# Life tables from HMD
# ~~~~~~~~~~~~~~~~~~~~
# HMD user and password
if(!'hmd_us' %in% ls()){
  cat("Please instert your HMD User:")
  hmd_us <- userInput()
}
if(!'hmd_pw' %in% ls()){
  cat("Please instert your HMD password:")
  hmd_pw <- userInput()
}

# Checking abbreviations
# cds_hmd_all <- getHMDcountries()

ltbs_hmd <- tibble()
for(ct in cds_hmd){
  chunk_t <- readHMDweb(ct, "bltper_1x1", hmd_us, hmd_pw) %>%
    filter(Year == max(Year)) %>%
    select(Age, ex) %>%
    as_tibble() %>%
    mutate(Code = ct,
           Sex = "t")
  
  chunk_f <- readHMDweb(ct, "fltper_1x1", hmd_us, hmd_pw) %>%
    filter(Year == max(Year)) %>%
    select(Age, ex) %>%
    as_tibble() %>%
    mutate(Code = ct,
           Sex = "f")
  
  chunk_m <- readHMDweb(ct, "mltper_1x1", hmd_us, hmd_pw) %>%
    filter(Year == max(Year)) %>%
    select(Age, ex) %>%
    as_tibble() %>%
    mutate(Code = ct,
           Sex = "m")
  
  ltbs_hmd <- ltbs_hmd %>%
    bind_rows(chunk_t, chunk_m, chunk_f)
}

ltbs_hmd2 <- 
  ltbs_hmd %>% 
  rename(hmd_code = Code) %>% 
  left_join(hmd_cts_cds) %>% 
  select(Country, Sex, Age, ex)


# Life tables from WPP
# ~~~~~~~~~~~~~~~~~~~~
ltbs_wpp_t <- 
  read_xlsx("Data/WPP2019_MORT_F16_1_LIFE_EXPECTANCY_BY_AGE_BOTH_SEXES.xlsx",
            sheet = 1,
            skip = 16) %>% 
  filter(Period == "2015-2020") %>% 
  rename(Country = 3) %>% 
  select(Country, 9:30) %>% 
  filter(Country %in% cts_wpp) %>% 
  gather(-Country, key = Age, value = ex) %>% 
  mutate(Sex = "t",
         ex = as.double(ex),
         Age = ifelse(Age == "100+", 100, as.integer(Age)))

ltbs_wpp_f <- 
  read_xlsx("Data/WPP2019_MORT_F16_3_LIFE_EXPECTANCY_BY_AGE_FEMALE.xlsx",
            sheet = 1,
            skip = 16) %>% 
  filter(Period == "2015-2020") %>% 
  rename(Country = 3) %>% 
  select(Country, 9:30) %>%
  filter(Country %in% cts_wpp) %>% 
  gather(-Country, key = Age, value = ex) %>% 
  mutate(Sex = "f",
         ex = as.double(ex),
         Age = ifelse(Age == "100+", 100, as.integer(Age)))

ltbs_wpp_m <- 
  read_xlsx("Data/WPP2019_MORT_F16_2_LIFE_EXPECTANCY_BY_AGE_MALE.xlsx",
            sheet = 1,
            skip = 16) %>% 
  filter(Period == "2015-2020") %>% 
  rename(Country = 3) %>% 
  select(Country, 9:30) %>% 
  filter(Country %in% cts_wpp) %>% 
  gather(-Country, key = Age, value = ex) %>% 
  mutate(Sex = "m",
         ex = as.double(ex),
         Age = ifelse(Age == "100+", 100, as.integer(Age)))

ltbs_wpp <- 
  bind_rows(ltbs_wpp_t, ltbs_wpp_f, ltbs_wpp_m)


# merging life expectancies from WPP and from HMD
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ltbs <- 
  bind_rows(ltbs_wpp, ltbs_hmd2) %>% 
  arrange(Country, Sex, Age)

#=================================================================================================#
#
#   INTERPOLATION
#  
#
#=================================================================================================#

# function to interpolate life expectancy into 0.01-year age intervals 
interpol_ex <- function(chunck){
  ages <- chunck$Age
  exs <- chunck$ex
  new_ages <- seq(0, 100, 0.01)
  
  md <- splinefun(x = ages, y = exs, method="fmm",  ties = mean)
  
  lt_sm <- tibble(Age = new_ages,
                  ex = md(new_ages))
  
}

cts <- unique(ltbs$Country)

ltbs_all <- tibble()
for(ct in cts){
  for(sx in c("t", "f", "m")){
    chunk <- 
      ltbs %>% 
      filter(Country == ct,
             Sex == sx)
    chunk_sm <- 
      interpol_ex(chunk) %>% 
      mutate(Country = ct,
             Sex = sx,
             Age = round(Age, 2))
    ltbs_all <- 
      bind_rows(ltbs_all, chunk_sm)
  }
}

# visual inspection
ct <- "Peru"
sx <- "t"
ltbs_all %>%
  filter(Country == ct,
         Sex == sx) %>%
  ggplot()+
  geom_line(aes(Age, ex), size = 1)+
  geom_point(data = ltbs %>% filter(Country == ct, Sex == sx),
             aes(Age, ex), col = "red")


write_rds(ltbs_all, "Output/lifetables_mid_ages.rds")
ltbs_all <- read_rds("Output/lifetables_mid_ages.rds")

#=================================================================================================#
#
#  ASSIGNING REMAINING LE
#  
#
#=================================================================================================#


# #=====================================================================================================#
# # Mid point intervals
# #=====================================================================================================#

# Using Japan e(x) for sensitivity tests
ex_jp <- ltbs_all %>% 
  filter(Country == "Japan") %>% 
  rename(ex_jp = ex) %>% 
  select(-Country)

# Average age of death from COVID-19 
# ==================================
# As defined by Goldstein et al (2021):
# "The average age of death from COVID-19 for the open interval was approximated 
# using all-cause HMD life tables as follows: 85+ (92.0) for the United States, 
# 100+ (101.9) for Germany, and 80+ (89.5) for South Korea."

# Identifying ex for the last age interval 
last_mid_points <- covid19_deaths %>% 
  group_by(Country, Sex) %>% 
  filter(Age == max(Age)) %>% 
  ungroup() %>% 
  select(Country, Sex, Age) %>% 
  left_join(ltbs_all) %>% 
  # using Japan life tables for robustness check
  left_join(ex_jp) %>% 
  mutate(mid_point = Age + ex,
         mid_point_jp = Age + ex_jp) %>% 
  select(Country, Sex, Age, mid_point, mid_point_jp) %>% 
  drop_na()

# Estimating midpoints of all COVID-19 mortality reporting 
# age groups (half of the interval), and include the midpoint of last interval
sle_all <- 
  covid19_deaths %>% 
  select(Country, Sex, Age) %>% 
  left_join(last_mid_points) %>% 
  group_by(Country, Sex) %>% 
  mutate(last = ifelse(Age == max(Age), 1, 0),
         mid_point = ifelse(last == 0, (Age + lead(Age)) / 2, mid_point),
         mid_point_jp = ifelse(last == 0, (Age + lead(Age)) / 2, mid_point_jp),
         mid_point_lowest_b = ifelse(last == 0, (Age + lead(Age)) / 2, Age)) %>% 
  ungroup() %>% 
  mutate(mid_point = round(mid_point, 2),
         mid_point_jp = round(mid_point_jp, 2))


# #=====================================================================================================#
# # Adjusted life expectancy and adding alternative ex for robustness check
# #=====================================================================================================#

## Alternative scenarios for robustness check
# 1) using Japan ex in all age intervals (ex_jp)
# 2) using Japan ex only in the last age interval (ex_jp_last)
# 3) all people in the last age interval die at the beginning of that interval (ex_lowest_b)
# 4) creating upper and lower bounds with +/-20% of the ex in the last age interval (ex_lb & ex_ub)
# 5) using the previous mx to estimate ex in the last age interval (not yet included, it would iimply reestimating life tables)

sle_adjusted <- 
  sle_all %>% 
  left_join(ltbs_all %>% 
              rename(mid_point = Age)) %>% 
  # including scenario where last age group has ex at its low bracket
  left_join(ltbs_all %>% 
              rename(mid_point_lowest_b = Age,
                     ex_lowest_b = ex)) %>% 
  # including scenario where all ages have Japan ex
  left_join(ex_jp %>% 
              rename(mid_point_jp = Age)) %>% 
  mutate(Country = ifelse(Country == "Republic of Moldova", "Moldova", Country),
         # including scenario where all ages have their own ex, but the last one has Japan ex
         ex_jp_last = ifelse(last == 0, ex, ex_jp),
         # including scenarios where last age interval has +/-20% of ex
         ex_lb = ifelse(last == 0, ex, ex * 0.8),
         ex_ub = ifelse(last == 0, ex, ex * 1.2),)

#=====================================================================================================#
# Saving
#=====================================================================================================#

write_rds(sle_adjusted,"Output/sle_adjusted.rds")

