#=================================================================================================#
#=================================================================================================#
#
#   YLL & Vaccines
#   Visualization
#
#=================================================================================================#
#=================================================================================================#

# Description:
# Creates plots


#=================================================================================================#
#
#   LIBRARIES
#  
#
#=================================================================================================#

library(tidyverse)
library(here)
library(cowplot)

#=================================================================================================#
#
#   DATA
#  
#
#=================================================================================================#

# Mortality rates by age and e_yll
#=================================================================================================#
# # Full data on mortality rates by age and e_yll

# Confirmed deaths estimates
age_rates_yll_optimal_cov <- 
  readRDS("Output/age_m_rates_optimal_all.rds") %>% 
  select(Country, Age, mid_point, mx, Scenario, yll, mx_opt)

# Excess estimates
age_rates_yll_optimal_ex <- readRDS("Output/age_m_rates_optimal_excess.rds") %>% 
  filter(Sex == "t") %>%
  select(Country, Age, mid_point, mx, e_yll, mx_opt) %>% 
  mutate(Scenario = "Excess") %>% 
  rename(yll = e_yll,
         mx_opt = mx_opt,
         mid_point = mid_point)

# merging confirmed deaths and excess estimates
age_rates_yll_optimal <-
  age_rates_yll_optimal_cov %>% 
  bind_rows(age_rates_yll_optimal_ex) 

# All cause mortality
all_cause_mx <- read_rds("Output/all_cause_mortality_rates.rds") %>% 
  select(Country, Age, mx_all) 
  

# Optimal vaccination ages
#=================================================================================================#
# Vaccination age data
# vaccine_age <- readRDS("Output/vaccine_age_excess.rds")

#=================================================================================================#
#
#   MANIPULATION
#  
#
#=================================================================================================#

# standardizing all cause mortality so it crosses in the last age interval 
# with covid mortality
all_cause_mx2 <- 
  all_cause_mx %>% 
  left_join(age_rates_yll_optimal_cov %>% 
              filter(Scenario == "Central ex") %>% 
              select(Country, Age, mid_point, mx))

# estimating adjustment factor to normalize at the last age group 
mx_adj <- all_cause_mx2 %>% 
  group_by(Country) %>% 
  filter(Age == max(Age)) %>%
  mutate(fct_adj = mx / mx_all) %>% 
  select(Country, fct_adj) %>% 
  ungroup()

all_cause_mx3 <- 
  all_cause_mx2 %>% 
  left_join(mx_adj) %>% 
  mutate(mx_std = mx_all * fct_adj) %>% 
  select(Country, Age, mx_std)


# selected countries for Figure 1
cts_temp <- c("USA", "Chile", "Peru")
# Ordering Countries as factor variable
labs_order <- c("USA", "Chile", "Peru")
# selecting scenarios
scns <- c("Central ex")

age_rates_yll2 <- 
  age_rates_yll_optimal %>% 
  filter(Country %in% cts_temp,
         Scenario %in% scns) %>%
  # adding normalized all cause mortality rates
  left_join(all_cause_mx3) %>% 
  mutate(Country = factor(Country, levels = labs_order)) %>% 
  select(Country, Age, mid_point, yll, mx, mx_opt, mx_std) %>% 
  gather(mx, mx_opt, mx_std, key = Source, value = mx) %>% 
  mutate(Source = recode(Source,
                         "mx" = "COVID-19",
                         "mx_opt" = "Theoretical",
                         "mx_std" = "All-cause"),
         Source = factor(Source, levels = c("COVID-19",
                                           "Theoretical",
                                           "All-cause")))
  

#=================================================================================================#
#
#   PLOTS
#  
#
#=================================================================================================#

#=================================================================================================#
# Figure 1. Covid death rates, crossover, all-cause mortality, and Years of life saved
#=================================================================================================#

p1 <-
  age_rates_yll2 %>% 
  filter(mid_point >= 30) %>% 
  ggplot()+
  geom_line(aes(mid_point, mx, col = Source, size = Source, alpha = Source))+
  geom_point(aes(mid_point, mx, col = Source, alpha = Source, shape = Source), size = 1.5)+
  scale_color_manual(values = c("black", "#e63946", '#1a759f'))+
  scale_size_manual(values = c(1, 0.8, 0.8))+
  scale_alpha_manual(values = c(1, 0.5, 0.5))+
  scale_shape_manual(values = c(1, 16, 16))+
  scale_y_log10()+
  scale_x_continuous(breaks = seq(0, 100, 20))+
  facet_wrap(~Country, scales = "free", nrow = 1)+
  coord_cartesian(xlim = c(35, 100))+
  theme_bw()+
  labs(y = "Death Rate", x = "Age")+
  theme(
    legend.position = c(0.93, 0.2),
    legend.background = element_rect(fill="transparent"),
    strip.background = element_rect(fill = "transparent"),
    legend.title = element_blank(),
    strip.text = element_text(size = 10, face = "bold"),
    # strip.text = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(angle = 90, hjust = .5, size = 8)
  )
    
p2 <- age_rates_yll2 %>% 
  filter(mid_point >= 30) %>% 
  ggplot()+
  geom_line(aes(mid_point, yll), size = 1)+
  geom_point(aes(mid_point, yll), alpha = 1, size = 1.5, shape = 1)+
  scale_x_continuous(breaks = seq(0, 100, 20))+
  facet_wrap(~ Country, scales = "free", nrow = 1)+
  coord_cartesian(xlim = c(35, 100))+
  theme_bw()+
  labs(y = "Years Saved", x = "Age")+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = .5, size = 8))


# plot_grid(p1, p2, labels = c('A', 'B'), label_size = 12, ncol = 1)
plot_grid(p1, p2, labels = c('A', 'B'), label_size = 12, ncol = 1)
ggsave("Figures/all_countries_mx_ex_yll_counterfactual_combined.png", width = 10, height = 6)


# ==================================================== #
# plotting ages intervals and the age at crossover
# ==================================================== #
tx <- 8
exclude <- c("India", "Poland", "Romania", "England and Wales", "Northern Ireland", "Scotland", "Greece")

# crossover for confirmed deaths and "usual" ex
cross_mx <- 
  age_rates_yll_optimal %>% 
  filter(!Country %in% exclude,
         Scenario == "Central ex") %>% 
  group_by(Country) %>% 
  mutate(cross = ifelse(round(mx, 8) >= round(mx_opt, 8) & lag(mx) < lag(mx_opt), 1, 0), 
         age_int = lead(Age) - Age,
         age_int = ifelse(is.na(age_int), 100 - Age, age_int), 
         cross = ifelse(sum(cross) > 1 & Age * cross == max(Age * cross), 0, cross),
         age_max = ifelse(Age == max(Age), 1, 0),
         age_cross = max((Age + age_int * 0.5) * cross + 10 * age_max)) %>% 
  ungroup() 

unique(age_rates_yll_optimal$Scenario)

# crossover for upper bound (+20%) of ex in last age group
cross_ub <- 
  age_rates_yll_optimal %>% 
  filter(!Country %in% exclude,
         Scenario == "Upper bound (+20%)") %>% 
  select(Country, Age, mx, mx_opt) %>% 
  group_by(Country) %>% 
  mutate(cross = ifelse(round(mx, 8) >= round(mx_opt, 8) & lag(mx) < lag(mx_opt), 1, 0), 
         age_int = lead(Age) - Age,
         age_int = ifelse(is.na(age_int), 100 - Age, age_int), 
         cross = ifelse(sum(cross) > 1 & Age * cross == max(Age * cross), 0, cross),
         age_max = ifelse(Age == max(Age), 1, 0),
         age_cross = max((Age + age_int * 0.5) * cross + 10 * age_max)) %>% 
  ungroup() %>% 
  rename(cross_ub = cross) %>% 
  select(Country, Age, cross_ub)

# crossover for excess mortality
cross_ex <- 
  age_rates_yll_optimal %>% 
  filter(!Country %in% exclude,
         Scenario == "Excess") %>% 
  drop_na() %>% 
  group_by(Country) %>% 
  mutate(cross = ifelse(round(mx, 8) >= round(mx_opt, 8) & lag(mx) < lag(mx_opt), 1, 0), 
         age_int = lead(Age) - Age,
         age_int = ifelse(is.na(age_int), 100 - Age, age_int), 
         cross = ifelse(sum(cross) > 1 & Age * cross == max(Age * cross), 0, cross),
         age_max = ifelse(Age == max(Age), 1, 0),
         age_cross = max((Age + age_int * 0.5) * cross + 10 * age_max)) %>% 
  ungroup() %>% 
  rename(cross_exc = cross) %>% 
  select(Country, Age, cross_exc)

# merging crossovers and identifying ages in each case
cross_mx_all <- 
  cross_mx %>% 
  select(Country, Age, age_int, age_cross, cross) %>% 
  left_join(cross_ub) %>% 
  left_join(cross_ex) %>% 
  group_by(Country) %>% 
  fill(cross_exc) %>% 
  ungroup() %>% 
  mutate(cross = factor(cross),
         cross_ub = ifelse(cross_ub == 0, NA, cross_ub),
         cross_ub = factor(cross_ub),
         cross_exc = ifelse(cross_exc == 1, 2, NA),
         cross_exc = factor(cross_exc),
         mid_point = Age + age_int * .5,
         age_ub = ifelse(cross_ub == 1, mid_point, NA),
         age_exc = ifelse(cross_exc == 2, mid_point, NA))

# plot of three different threshold ages
cross_mx_all %>%
  ggplot()+
  geom_tile(aes(mid_point, reorder(Country, -age_cross), 
                width = age_int, 
                fill = cross), 
            col = "black", alpha = 0.8, size = 0.5)+
  # adding upper bound of ex (+20%)
  geom_point(aes(age_ub, Country, shape = cross_ub), size = 3, stroke = 0.8)+
  # adding excess estimates
  geom_point(aes(age_exc, Country, shape = cross_exc), size = 3, stroke = 0.8)+
  scale_fill_manual(values = c("transparent", "#e63946"), breaks="1", 
                    labels = c("Cross-over"))+
  scale_shape_manual(values = c(0, 4), labels = c("Last ex + 20%", "Excess deaths"))+
  guides(fill = guide_legend(order = 1), shape = guide_legend(order = 2))+
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 100, 10))+
  scale_y_discrete(expand = c(0, 0))+
  labs(x = "Age")+
  theme_bw()+
  theme(
    legend.position="bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = tx + 4),
    axis.text.x = element_text(size = tx + 4),
    axis.text.y = element_text(size = tx + 4),
    axis.title.x = element_text(size = tx + 5),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.margin = unit(c(5, 5, 5, 5), "mm"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1.2)
  )
ggsave("Figures/ages_intervals_all_measures.png", width = 8, height = 10)

