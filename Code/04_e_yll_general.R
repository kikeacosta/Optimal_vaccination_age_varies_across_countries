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
covid19_deaths <- read_rds('Output/covid19_deaths.rds') 
unique(covid19_deaths$Country)

# covid19_deaths <- read_rds("Output/covid19_deaths_gmprtz.rds")
# unique(covid19_deaths$Country)


# List of countries in the example:
# countries_list <- unique(covid19_deaths$Country)
# countries_example <- countries_list

# Life expectancies
#=================================================================================================#
sle_adjusted <- readRDS("Output/sle_adjusted.rds")
unique(sle_adjusted$Country) %>% sort()
# Population counts
#=================================================================================================#
pop_all <- readRDS("Output/pop_all.rds")
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

## Alternative scenarios for robustness check
# 1) using Japan ex in all age intervals (ex_jp)
# 2) using Japan ex only in the last age interval (ex_jp_last)
# 3) all people in the last age interval die at the beginning of that interval (ex_lowest_b)
# 4) creating upper and lower bounds with +/-20% of the ex in the last age interval (ex_lb & ex_ub)
# 5) using the previous mx to estimate ex in the last age interval (not yet included, it would iimply reestimating life tables)


#=================================================================================================#
# Covid-19 mortality rates & YLL per Age group
#=================================================================================================#

age_rates_yll <- 
  covid19_deaths %>% 
  left_join(sle_adjusted) %>% 
  left_join(pop_all) %>% 
  filter(Sex == "t") %>% 
  # mortality rates (mx) and expected number of YLL per Age, given the risk of death
  mutate(mx = Deaths / Population,
         yll = mx * ex,
         yll_jp = mx * ex_jp,
         yll_jp_last = mx * ex_jp_last,
         yll_lb = mx * ex_lb,
         yll_ub = mx * ex_ub) 

age_yll <- 
  age_rates_yll %>% 
  select(Country, Age, mid_point, mx, yll, yll_jp, yll_jp_last, yll_lb, yll_ub) %>% 
  gather(-c("Country", "Age", "mid_point", "mx"), key = Scenario, value = yll) %>% 
  mutate(Scenario = ifelse(Scenario == "yll", "Central ex", str_sub(Scenario, 5, 12)))

age_ex <- 
  age_rates_yll %>% 
  select(Country, Age, mid_point, mx, ex, ex_jp, ex_jp_last, ex_lb, ex_ub) %>% 
  gather(-c("Country", "Age", "mid_point", "mx"), key = Scenario, value = ex) %>% 
  mutate(Scenario = ifelse(Scenario == "ex", "Central ex", str_sub(Scenario, 4, 12)))

age_ex_yll <- age_ex %>% 
  left_join(age_yll) %>% 
  mutate(Scenario = recode(Scenario,
                           "jp" = "Japan ex",
                           "jp_last" = "Japan ex in last age",
                           "lb" = "Lower bound (- 20%)",
                           "ub" = "Upper bound (+20%)"))

unique(age_ex_yll$Scenario)
unique(age_ex_yll$Country)

#=================================================================================================#
# Optimal age groups
#=================================================================================================#

vaccine_age <- 
  age_ex_yll %>% 
  group_by(Country, Scenario) %>% 
  mutate(last_age = max(Age)) %>% 
  filter(yll == max(yll)) %>% 
  select(Country, Scenario, Age, last_age, yll) %>% 
  arrange(Country, Scenario)


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

max_mx <- 
  age_ex_yll %>% 
  group_by(Country, Scenario) %>% 
  filter(mx == max(mx)) %>% 
  select(Country, Scenario, Age, mx, yll) %>% 
  rename(my = mx,
         yll_y = yll)

levs_sce <- c("Central ex", 
              "Lower bound (- 20%)",
              "Upper bound (+20%)",
              "Japan ex",
              "Japan ex in last age")

age_rates_yll_optimal <- 
  age_ex_yll %>% 
  left_join(max_mx %>% 
              select(-Age), by = c("Country", "Scenario")) %>% 
  mutate(mx_opt = yll_y / ex,
         Scenario = factor(Scenario, 
                           levels = levs_sce))

unique(age_rates_yll_optimal$Scenario)


age_rates_yll_optimal %>% 
  filter(Country %in% c("USA", "Chile", "Peru")) %>%
  mutate(Country = factor(Country, levels = c("USA", "Chile", "Peru"))) %>% 
  ggplot()+
  geom_line(aes(mid_point, mx))+
  geom_line(aes(mid_point, mx_opt, col = Scenario, linetype = Scenario), alpha = 0.8)+
  facet_wrap(~Country, scales = "free")+
  scale_color_manual(values = c("#e63946", "#e63946", "#e63946", "#0077b6", "#2a9d8f"))+
  scale_linetype_manual(values = c("solid", "dashed", "dashed", "solid", "solid"))+
  theme_bw()+
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    # strip.text.x = element_blank(),
    axis.text.y = element_text(angle = 90, hjust = .5, size = 8)
  )

# ggsave("Figures/scenarios_crossover.png")

age_rates_yll_optimal %>% 
  filter(Country %in% c("USA", "Chile", "Peru"),
         Age > 20) %>%
  mutate(Country = factor(Country, levels = c("USA", "Chile", "Peru"))) %>% 
  ggplot()+
  geom_line(aes(mid_point, mx))+
  geom_line(aes(mid_point, mx_opt, col = Scenario, linetype = Scenario), alpha = 0.8)+
  scale_y_log10()+
  facet_wrap(~Country, scales = "free")+
  scale_color_manual(values = c("#e63946", "#e63946", "#e63946", "#0077b6", "#2a9d8f"))+
  scale_linetype_manual(values = c("solid", "dashed", "dashed", "solid", "solid"))+
  theme_bw()+
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    # strip.text.x = element_blank(),
    axis.text.y = element_text(angle = 90, hjust = .5, size = 8)
  )

# ggsave("Figures/scenarios_crossover_log.png")

#=================================================================================================#
# Saving
#=================================================================================================#

# Full data on mortality rates by age and e_yll
saveRDS(age_ex_yll,"Output/age_ex_yll_all.rds")
# Full data on minimum mortality rates by age and e_yll
saveRDS(age_rates_yll_optimal,"Output/age_m_rates_optimal_all.rds")
# Vaccination age data including robustness chaecks
saveRDS(vaccine_age,"Output/vaccine_age.rds")

