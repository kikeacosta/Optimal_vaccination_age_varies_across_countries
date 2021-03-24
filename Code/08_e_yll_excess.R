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

# Let LE(x) be life expectancy at life x and risk_death_covid(x) the covid mortality rate at Age x.
# Then, expected of years of life lost to covid-19 by each person of group Age x are:
# E[YLL_covid-19|X=x,no vaccine]=LE(x)*risk_death_covid(x)
# Let vaccine be the fraction by which death rates from covid-19 improve with the vaccine,
# and assume it is Age independent.The risk of dying from covid with the vaccine is:
# risk_death_covid(x)*vaccine
# Then, Under the vaccine
# E[YLL_covid-19|X=x,vaccine]=LE(x)*risk_death_covid(x)*vaccine
# Thus, the gain from the vaccine is
# Vaccine gain:=E[YLL_covid-19|X=x,no vaccine]-E[YLL_covid-19|X=x,vaccine]
# =LE(x)*risk_death_covid(x)-LE(x)*risk_death_covid(x)*vaccine
# =LE(x)*risk_death_covid(x)*(1-vaccine)
# Now, for two Ages x and y, the vaccine should be given to x iff:
# LE(x)*risk_death_covid(x)*(1-vaccine)>LE(y)*risk_death_covid(y)*(1-vaccine)
# or risk_death_covid(x)/risk_death_covid(y)>LE(y)/LE(x)



#=================================================================================================#
#
#   LIBRARIES
#  
#
#=================================================================================================#

library(tidyverse)
library(here)

#=================================================================================================#
#
#   DATA
#  
#
#=================================================================================================#

# COVID-19 deaths 
#=================================================================================================#
# Specific countries with good disaggregation at older ages (read here to keep in mind age groups)
db_excess <- read_rds("Output/cumulative_excess_selected_countries.rds")
unique(db_excess$Country)

# List of countries in the example:
# countries_list <- unique(db_excess$Country)
# countries_example <- countries_list

# Life expectancies
#=================================================================================================#
sle_adjusted <- readRDS("Output/sle_adjusted_for_excess.rds")
unique(sle_adjusted$Country) %>% sort()
# Population counts
#=================================================================================================#
pop_all <- readRDS("Output/pop_for_excess.rds")
unique(pop_all$Country) %>% sort()

#=================================================================================================#
#
#   EYLL CALCULATIONS
#  
#
#=================================================================================================#

#=================================================================================================#
# Covid-19 mortality rates & YLL per Age group
#=================================================================================================#

age_rates_yll <- 
  db_excess %>% 
  left_join(pop_all) %>% 
  left_join(sle_adjusted) %>% 
  rename(Deaths = Excess) %>% 
  # mortality rates (mx) and expected number of YLL per Age, given the risk of death
  mutate(mx = Deaths / Population,
         e_yll = mx * ex) %>% 
  select(Country, Sex, Age, mid_point, Deaths, Population, mx, ex, e_yll)

#=================================================================================================#
# Optimal age groups
#=================================================================================================#
vaccine_age <- 
  age_rates_yll %>% 
  group_by(Country, Sex) %>% 
  filter(e_yll == max(e_yll)) %>% 
  select(Country, Sex, Age, e_yll)


#=================================================================================================#
#
#   ROBUSTNESS: ACCEPT LE, HOW HIGH MORTALITY RATES?
#  
#
#=================================================================================================#
# Pick as a reference the age with the highest mortality rates from covid-19 (call that y);
# this tends to be the oldest age group.

# We know that for two ages x and y, the vaccine should be given to x iff:
# LE(x)*risk_death_covid(x)*(1-vaccine)>LE(y)*risk_death_covid(y)*(1-vaccine)
# or risk_death_covid(x)/risk_death_covid(y)>LE(y)/LE(x)  

# So for x to be at least as good as y, it would need to be risk_death_covid(x)=LE(y)/LE(x)*risk_death_covid(y)
# = risk_death_covid(x)_optimal = YLL(y)/LE(x)
# Here x is the younger age group.

# selecting the 
max_mx <- 
  age_rates_yll %>% 
  group_by(Country, Sex) %>% 
  filter(mx == max(mx)) %>% 
  select(Country, Sex, Age, mx, e_yll) %>% 
  rename(my = mx,
         e_yll_y = e_yll)

age_rates_yll_optimal <- 
  age_rates_yll %>% 
  left_join(max_mx %>% 
              select(-Age), by = c("Country", "Sex")) %>% 
  mutate(mx_opt = e_yll_y / ex) 


#=================================================================================================#
# Saving
#=================================================================================================#

# Full data on mortality rates by age and e_yll
saveRDS(age_rates_yll,"Output/age_m_rates_excess.rds")
# Full data on minimum mortality rates by age and e_yll
saveRDS(age_rates_yll_optimal,"Output/age_m_rates_optimal_excess.rds")
# Vaccination age data
saveRDS(vaccine_age,"Output/vaccine_age_excess.rds")


# ====================
