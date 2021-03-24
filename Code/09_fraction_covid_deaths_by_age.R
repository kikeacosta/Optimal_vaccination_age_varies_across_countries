library(tidyverse)
library(lubridate)
library(ISOweek)

# mortality from all causes
all_us <- read_csv("Data/USAstmf.csv")
all_cl <- read_csv("Data/CHLstmf.csv")
all_pe <- read_csv("Data/peru_deaths_weekly.csv")
# confirmed deaths
covid19_deaths <- readRDS("Output/covid19_deaths.rds")
# populations
pop_all <- readRDS("Output/pop_all.rds")
unique(pop_all$Country) %>% sort()

# Confirmed Covid deaths between March 2020 (week 8 2020) 
# and January 15, 2021 (week 2 2021)

# summarizing mortality from all causes in USA, Chile, and Peru during 
# the same period
all_cause <- 
  bind_rows(all_us, all_cl) %>% 
  filter(Sex == "b",
         Age != "TOT") %>% 
  select(PopCode, Year, Week, Age, Deaths) %>% 
  mutate(Country = recode(PopCode,
                          "USA" = "USA",
                          "CHL" = "Chile"),
         Age = Age %>% as.integer) %>% 
  select(-PopCode) %>% 
  bind_rows(all_pe)

all_cause2 <- all_cause %>% 
  filter(Year == 2020 & Week >= 8 | Year == 2021 & Week <= 2) %>% 
  group_by(Country, Age) %>% 
  summarise(All_Deaths = sum(Deaths)) %>% 
  ungroup()



# Covid deaths
covid19_deaths2 <- 
  covid19_deaths %>% 
  filter(Country %in% c("USA", "Chile", "Peru"),
         Sex == "t") %>% 
  select(Country, Age, Deaths) %>% 
  rename(Covid = Deaths)


# comparison of deaths
db_comp <- 
  all_cause2 %>% 
  left_join(covid19_deaths2) %>% 
  mutate(frac = Covid / All_Deaths,
         Country = factor(Country, levels = c("USA", "Chile", "Peru")))

tx <- 10
db_comp %>% 
  ggplot()+
  geom_line(aes(Age, frac, col = Country), size = 0.8, alpha = 0.8)+
  theme_bw()+
  labs(x = "Age", y = "??")+
  theme(
    axis.text.x = element_text(size = tx + 2),
    axis.text.y = element_text(size = tx + 2),
    axis.title.x = element_text(size = tx + 3),
    axis.title.y = element_blank(),
    plot.margin = unit(c(5, 5, 5, 5), "mm"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1)
  )
# ggsave("Figures/fraction_covid_mort_over_age.png", width = 6, height = 3)


# mortality rates all cause
pop2 <- pop_all %>% 
  filter(Sex == "t",
         Country %in% c("USA", "Chile", "Peru")) %>% 
  select(-Sex, -Year)

all_cause_mx <- 
  all_cause2 %>% 
  left_join(pop2) %>% 
  mutate(mx_all = All_Deaths / Population)
  
write_rds(all_cause_mx, "Output/all_cause_mortality_rates.rds")
