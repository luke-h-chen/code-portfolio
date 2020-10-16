#--------------------
#Objective: Analysis of Stay-At-Home and Mask Mandate Policy Effects (working script)
#Date: Oct. 13, 2020
#Email: lukechen@uchicago.edu
#Author: Luke Chen
#--------------------

#Libraries (some may be unnecessary while this script is actively being worked on)
library(tidyverse)
library(lubridate)
library(sf)
library(broom)
library(readxl)
library(dotwhisker)
library(sandwich)
library(ggpubr)
library(here)
library(viridis)

#I use the "here" package to hopefully make this script more easily accessed by people who are not me
setwd(here())

#Housekeeping
rm(list = ls())

#Importing data. Most data has been prepped in a separate script and should be included in the repository of this project
#Pre-cleaned SafeGraph mobility data
mobility_data <- read_csv(here("/clean_data/updated_safegraph_data.csv"))

# "consolidated_data.csv" is out-dated and not useful for now in light of CDC SAHP and Wright et al Mask Policy data
# Instead, use the "iat_data.csv" file
# IAT data is pretty messy still... Some last minute cleaning done here.
iat_data <- read_csv(here("/clean_data/iat_data.csv")) %>%
  select(mean_att, median_att, state_code, CountyNo) %>%
  mutate(state_code = str_pad(state_code, width = 2, side = "left", pad = "0"),
         CountyNo = str_pad(CountyNo, width = 3, side = "left", pad = "0"),
         GEOID = str_c(state_code, CountyNo)) %>%
  select(-state_code, -CountyNo)

#Mask Mandate data taken from the August 1st working version of the Wright, Chawla, Chen, Farmer Mask Mandate Database
mask_mandates <- read_xlsx(here("/clean_data/DPSS - Mask Mandate Data.xlsx"), sheet = "County") %>%
  select(couty_fips, mask_policy_start, mask_policy_end, escalation, defiance) %>%
  mutate(couty_fips = as.character(couty_fips))

#Publicized New York Times data on number of cases and deaths across counties in the US
nyt_data <- read_csv(here("/clean_data/nyt_data.csv"))

#County-level characteristics, this time taken from the USDA website
usda_county_data <- read_csv(here("/clean_data/usda_county_data.csv"))

#Shapefiles for maps
county_geo <- st_read(here("/clean_data/cb_2018_us_county_500k/cb_2018_us_county_500k.shp"))

#CDC SAH data
cdc_sah <- read_csv(here("/clean_data/SAHP_dates.csv"))

#Climate change opinion data
climate_beliefs_data <- read_csv(here("/clean_data/sci_belief_data_cleaned.csv"))

#2016 Presidential Election Vote Share Data
vote_share_data <- read_csv(here("clean_data/r_2016voteshare.csv"))

#Religious composition data from the "Religious Census 2010"
religion_data <- read_csv(here("clean_data/religion_data.csv"))

#Join all the different data to create a unified data set for all counties
df <- mobility_data %>%
  left_join(iat_data, by = "GEOID") %>%
  left_join(mask_mandates, by = c("GEOID" = "couty_fips")) %>%
  left_join(nyt_data, by = c("GEOID" = "fips", "date")) %>%
  left_join(usda_county_data, by = c("GEOID" = "FIPStxt")) %>%
  left_join(cdc_sah, by = "GEOID") %>%
  left_join(climate_beliefs_data, by = "GEOID") %>%
  left_join(vote_share_data, by = c("GEOID" = "FIPS")) %>%
  left_join(religion_data, by = "GEOID")

#From this point on, "df" is the main working data set (the others can be deleted to save environment space)
# rm(list = setdiff(ls(), "df"))

#Identify the first date of an observed covid case in each county
first_cases.df <- df %>% filter(cases > 0) %>% group_by(GEOID) %>% summarise(first_case = min(date))
df <- df %>% left_join(first_cases.df, by = "GEOID")

# Create a whole bunch of useful variables
df <- df %>% mutate(days_since_first = as.numeric(date - first_case), #days since first case
                    mask_mandate_from_first = ifelse(is.na(mask_policy_start), #days between first case and mask mandate
                                                     999, #999 used as a placeholder for now. *** Come back to this later ***
                                                     as.numeric(lubridate::date(mask_policy_start) - first_case)),
                    have_mask_mandate = 1-is.na(mask_policy_start),
                    sah_from_first = as.numeric(earliest_sah - first_case),
                    days_since_sah = as.numeric(date - earliest_sah),
                    post_sah = date > earliest_sah,
                    positive_cases = date > first_case,
                    ma_median_time_home = SafeGraphR::ma(median_percentage_time_home),
                    ma_pct_completely_home = SafeGraphR::ma(percentage_completely_home)
                    ) %>%
  filter(state_fips < 60) #This line gets rid of territories

# "Fix" the Rural-urban code data for the counties missing it
df <- df %>% mutate(Rural_urban_continuum_code_2013 = case_when(
  !is.na(Rural_urban_continuum_code_2013) ~ Rural_urban_continuum_code_2013,
  is.na(Rural_urban_continuum_code_2013) & pop_2018 > 1000000 ~ 1,
  is.na(Rural_urban_continuum_code_2013) & pop_2018 %in% 250000:999999 ~ 2,
  is.na(Rural_urban_continuum_code_2013) & pop_2018 %in% 50000:250000 ~ 3,
  is.na(Rural_urban_continuum_code_2013) & pop_2018 %in% 20000:50000 ~ 4,
  is.na(Rural_urban_continuum_code_2013) & pop_2018 %in% 2500:19999 ~ 6,
  is.na(Rural_urban_continuum_code_2013) & pop_2018 < 2500 ~ 8
  ),
  Metro_2013 = Rural_urban_continuum_code_2013 %in% 1:3)


# Create a cross-sectional version of the data using the last day of data (Sept.30)
df_cross <- df %>% group_by(GEOID) %>% arrange(date) %>% slice(n()) %>% ungroup() %>%
  mutate(have_sahp = as.numeric(!is.na(earliest_sah)))

#### An examination of the relationship between population and policy timing relative to first case ####
SAHP_df <- df %>% group_by(GEOID) %>%
  mutate(sah_from_first = ifelse(sah_from_first < 999, sah_from_first, NA)) %>%
  summarise(sah_from_first = mean(sah_from_first),
            pop_2018 = mean(pop_2018),
            first_case = mean(first_case)) %>%
  na.omit(sah_from_first)

SAHP_populations_plot <- SAHP_df %>% ggplot() +
  geom_point(aes(x=log10(pop_2018), y=sah_from_first, color = first_case), alpha = 0.7) +
  geom_smooth(aes(x = log10(pop_2018), y = sah_from_first), method = "lm", se = F, color = "violet", alpha = 0.7) +
  labs(title = "Stay at Home Policy Timing, First Case Timing, and Population",
       subtitle = paste0("N = ", nrow(SAHP_df), " Counties with Stay at Home Policies"),
       x = "Log10(Population)",
       y = "Days from First Case to SAHP",
       color = "Date of First Case") +
  scale_color_date(high = "#003c42", low = "#42d4ff") +
  theme_minimal()

SAHP_populations_plot

no_SAHP_df <- df %>% group_by(GEOID) %>%
  filter(positive_cases == T) %>%
  mutate(sah_from_first = ifelse(sah_from_first < 999, sah_from_first, 999)) %>%
  filter(sah_from_first == 999) %>%
  summarise(pop_2018 = mean(pop_2018),
            cases = max(cases, na.rm = T),
            first_case = mean(first_case))

no_SAHP_pop_plot <- no_SAHP_df %>%
  ggplot() + geom_histogram(aes(x=log10(pop_2018)), bins = 88, color =  "grey50", fill = "#42d4ff" ) +
  labs(title = "Histogram of Populations for Counties With No SAHP Yet",
       x = "Log10(Population)",
       y = "Number of Counties",
       subtitle = paste0("N =", nrow(no_SAHP_df), " Counties with Positive Covid Cases but no SAHP")) +
  theme_minimal()

no_SAHP_pop_plot

no_SAHP_fc_plot <- no_SAHP_df %>%
  mutate(days_first_to_september = as.numeric(mdy("09/30/2020") - first_case)) %>%
  ggplot() + geom_histogram(aes(x = days_first_to_september), bins = 88, color = "grey50", fill = "#42d4ff") +
  labs(title = "Days Since First Case for Counties with No SAHP",
       subtitle = "Using September 30 as Benchmark",
       x = "Days from First Case to September 30",
       y = "Number of Counties") +
  theme_minimal()

no_SAHP_fc_plot

combined_pop_plots <- ggarrange(SAHP_populations_plot, ggarrange(no_SAHP_pop_plot, no_SAHP_fc_plot, nrow = 1), ncol = 1)
combined_pop_plots


#### An examination of the relationship between first case date and absolute SAHP date by state ####
rural_urban_key_dates_plot <- df %>% group_by(Stabr, GEOID) %>%
  summarise(first_case = mean(first_case, na.rm = T),
            earliest_sah = mean(earliest_sah, na.rm = T),
            Rural_urban_continuum_code_2013 = mean(Rural_urban_continuum_code_2013, na.rm = T),
            order_code = mean(order_code, na.rm = T)) %>%
  ggplot() + geom_point(aes(x = first_case, y = earliest_sah, color = Rural_urban_continuum_code_2013), alpha = 0.7) +
  geom_abline(slope = 1) +
  labs(x = "Date of First Case",
       y = "Earliest Stay At Home Policy",
       color = "USDA Rural Urban \nContinuum Code",
       title = "State Differences in Local Stay at Home Policy Timings",
       caption = "Solid line is 45 degree line
       \nCounties with No Policy are excluded") +
  scale_color_viridis_c() +
  facet_wrap(~Stabr) +
  theme_minimal()
rural_urban_key_dates_plot

order_type_key_dates_plot <- df %>% group_by(Stabr, GEOID) %>%
  summarise(first_case = mean(first_case, na.rm = T),
            earliest_sah = mean(earliest_sah, na.rm = T),
            Rural_urban_continuum_code_2013 = mean(Rural_urban_continuum_code_2013, na.rm = T),
            order_code = mean(order_code, na.rm = T)) %>%
  filter(order_code != 1) %>%
  mutate(order_type = factor(case_when(order_code == 7 ~ "Mandatory for All",
                                order_code == 6 ~ "Mandatory in Certain Areas",
                                order_code == 5 ~ "Mandatory for At-Risk Individuals",
                                order_code == 4 ~ "Mandatory for Minors",
                                order_code == 2 ~ "Advisory/Recommendation",
                                order_code == 1 ~ "No Policy"),
                             levels = c("Mandatory for All",
                                        "Mandatory in Certain Areas",
                                        "Mandatory for At-Risk Individuals",
                                        "Mandatory for Minors",
                                        "Advisory/Recommendation"))) %>%
  ggplot() + geom_point(aes(x = first_case, y = earliest_sah, color = order_type), alpha = 0.7) +
  geom_abline(slope = 1) +
  scale_color_viridis_d() +
  labs(x = "Date of First Case",
       y = "Earliest Stay At Home Policy",
       color = "Policy Type",
       title = "State Differences in Local Stay at Home Policy Timings",
       sutitle = "Breakdown by Policy Conditions",
       caption = "Solid line is 45 degree line
       \nCounties with No Policy are excluded") +
  facet_wrap(~Stabr) +
  theme_minimal()
order_type_key_dates_plot

key_dates_combined_plots <- ggarrange(rural_urban_key_dates_plot, order_type_key_dates_plot, nrow =2)
key_dates_combined_plots

### Recreate plots now with only pop_2018 > 50k (an arbitrary definition of "urban")
#### An examination of the relationship between population and policy timing relative to first case ####
SAHP_df_bigpop <- df %>% group_by(GEOID) %>% filter(Rural_urban_continuum_code_2013 %in% 1:3) %>%
  mutate(sah_from_first = ifelse(sah_from_first < 999, sah_from_first, NA)) %>%
  summarise(sah_from_first = mean(sah_from_first),
            pop_2018 = mean(pop_2018),
            first_case = mean(first_case)) %>%
  na.omit(sah_from_first)

SAHP_populations_plot <- SAHP_df_bigpop %>% ggplot() +
  geom_point(aes(x=log10(pop_2018), y=sah_from_first, color = first_case), alpha = 0.7) +
  geom_smooth(aes(x = log10(pop_2018), y = sah_from_first), method = "lm", se = F, color = "violet", alpha = 0.7) +
  labs(title = "Stay at Home Policy Timing, First Case Timing, and Population",
       subtitle = paste0("N = ", nrow(SAHP_df_bigpop), " Metro Counties with Stay at Home Policies"),
       x = "Log10(Population)",
       y = "Days from First Case to SAHP",
       color = "Date of First Case") +
  scale_color_date(high = "#003c42", low = "#42d4ff") +
  theme_minimal()

SAHP_populations_plot

no_SAHP_df_bigpop <- df %>% group_by(GEOID) %>% filter(Rural_urban_continuum_code_2013 %in% 1:3) %>%
  filter(positive_cases == T) %>%
  mutate(sah_from_first = ifelse(sah_from_first < 999, sah_from_first, 999)) %>%
  filter(sah_from_first == 999) %>%
  summarise(pop_2018 = mean(pop_2018),
            cases = max(cases, na.rm = T),
            first_case = mean(first_case))

no_SAHP_pop_plot <- no_SAHP_df_bigpop %>%
  ggplot() + geom_histogram(aes(x=log10(pop_2018)), bins = 88, color =  "grey50", fill = "#42d4ff" ) +
  labs(title = "Histogram of Populations for Counties With No SAHP Yet",
       x = "Log10(Population)",
       y = "Number of Counties",
       subtitle = paste0("N =", nrow(no_SAHP_df_bigpop), " Metro Counties with Positive Covid Cases but no SAHP")) +
  theme_minimal()

no_SAHP_pop_plot

no_SAHP_fc_plot <- no_SAHP_df_bigpop %>%
  mutate(days_first_to_september = as.numeric(mdy("09/30/2020") - first_case)) %>%
  ggplot() + geom_histogram(aes(x = days_first_to_september), bins = 88, color = "grey50", fill = "#42d4ff") +
  labs(title = "Days Since First Case for Counties with No SAHP",
       subtitle = "Using September 30 as Benchmark",
       x = "Days from First Case to September 30",
       y = "Number of Counties") +
  theme_minimal()

no_SAHP_fc_plot

combined_pop_plots <- ggarrange(SAHP_populations_plot, ggarrange(no_SAHP_pop_plot, no_SAHP_fc_plot, nrow = 1), ncol = 1)
combined_pop_plots


#### An examination of the relationship between first case date and absolute SAHP date by state ####
rural_urban_key_dates_plot <- df %>% group_by(Stabr, GEOID) %>%
  filter(Rural_urban_continuum_code_2013 %in% 1:3) %>%
  summarise(first_case = mean(first_case, na.rm = T),
            earliest_sah = mean(earliest_sah, na.rm = T),
            Rural_urban_continuum_code_2013 = mean(Rural_urban_continuum_code_2013, na.rm = T),
            order_code = mean(order_code, na.rm = T)) %>%
  ggplot() + geom_point(aes(x = first_case, y = earliest_sah, color = Rural_urban_continuum_code_2013), alpha = 0.7) +
  geom_abline(slope = 1) +
  labs(x = "Date of First Case",
       y = "Earliest Stay At Home Policy",
       color = "USDA Rural Urban \nContinuum Code",
       title = "State Differences in Local Stay at Home Policy Timings",
       subtitle = "Breakdown by USDA Rural-Urban Continuum Code | Metro Counties Only",
       caption = "Solid line is 45 degree line
       \nCounties with No Policy are excluded") +
  scale_color_viridis_c() +
  facet_wrap(~Stabr) +
  theme_minimal()
rural_urban_key_dates_plot

order_type_key_dates_plot <- df %>% group_by(Stabr, GEOID) %>%
  filter(Rural_urban_continuum_code_2013 %in% 1:3) %>%
  summarise(first_case = mean(first_case, na.rm = T),
            earliest_sah = mean(earliest_sah, na.rm = T),
            Rural_urban_continuum_code_2013 = mean(Rural_urban_continuum_code_2013, na.rm = T),
            order_code = mean(order_code, na.rm = T)) %>%
  filter(order_code != 1) %>%
  mutate(order_type = factor(case_when(order_code == 7 ~ "Mandatory for All",
                                       order_code == 6 ~ "Mandatory in Certain Areas",
                                       order_code == 5 ~ "Mandatory for At-Risk Individuals",
                                       order_code == 4 ~ "Mandatory for Minors",
                                       order_code == 2 ~ "Advisory/Recommendation",
                                       order_code == 1 ~ "No Policy"),
                             levels = c("Mandatory for All",
                                        "Mandatory in Certain Areas",
                                        "Mandatory for At-Risk Individuals",
                                        "Mandatory for Minors",
                                        "Advisory/Recommendation"))) %>%
  ggplot() + geom_point(aes(x = first_case, y = earliest_sah, color = order_type), alpha = 0.7) +
  geom_abline(slope = 1) +
  scale_color_viridis_d() +
  labs(x = "Date of First Case",
       y = "Earliest Stay At Home Policy",
       color = "Policy Type",
       title = "State Differences in Local Stay at Home Policy Timings",
       subtitle = "Breakdown by Policy Conditions | Metro Counties Only",
       caption = "Solid line is 45 degree line
       \nCounties with No Policy are excluded") +
  facet_wrap(~Stabr) +
  theme_minimal()
order_type_key_dates_plot

key_dates_combined_plots <- ggarrange(rural_urban_key_dates_plot, order_type_key_dates_plot, nrow =2)
key_dates_combined_plots

#### Cross-sectional analyses ####
why_no_policy <- df_cross %>%
  glm(data = ., have_sahp ~ log(pop_2018) + r_2016voteshare + POVALL_2018 + personalOppose + evangelical_pct + catholic_pct,
      family = binomial(link = "logit"))

summary(why_no_policy)

#### Visualization of mobility data across state based on time and/or Covid ####

df %>%
  ggplot() + geom_line(aes(x = days_since_first, y = ma_median_time_home, group = GEOID, color = state_fips), alpha = 0.1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "maroon") +
  facet_wrap(~Metro_2013) +
  theme(legend.position = "none")

df$Metro_2013 <- factor(df$Metro_2013, levels = c(FALSE, TRUE),
                  labels = c("Non-Metro", "Metro"))

sah_since_case <- df %>%
  filter(!is.na(earliest_sah), !is.na(first_case)) %>%
  ggplot() +
  geom_smooth(aes(x = days_since_first,
                y = percentage_completely_home,
                color = Rural_urban_continuum_code_2013,
                group = Rural_urban_continuum_code_2013),
            se = F) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "maroon") +
  facet_wrap(~Metro_2013) +
  xlim(-50,75) +
  labs(x = "Days Since First Covid-19 Case",
       y = "Percentage Completely Home",
       title = "Response to First Covid-19 Case by Urban Status",
       subtitle = "Analysis of Counties with Positive Cases and SAHPs",
       color = "USDA Rural-Urban\nContinuum Code") +
  scale_color_viridis_c() +
  theme_minimal()

sah_since_policy <- df %>%
  filter(!is.na(earliest_sah), !is.na(first_case)) %>%
  ggplot() +
  geom_smooth(aes(x = days_since_sah,
                  y = percentage_completely_home,
                  color = Rural_urban_continuum_code_2013,
                  group = Rural_urban_continuum_code_2013),
              se = F) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "maroon") +
  facet_wrap(~Metro_2013) +
  labs(x = "Days Since Earliest SAHP",
       y = "Percentage Completely Home",
       color = "USDA Rural-Urban\nContinuum Code") +
  scale_color_viridis_c() +
  xlim(-50, 75) +
  theme_minimal()

sah_combined_plots <- ggarrange(sah_since_case, sah_since_policy, ncol = 1)
sah_combined_plots

# Maybe people are actually responding to state info?
df <- df_cross %>%
  group_by(state_fips) %>%
  summarise(state_first_case = min(first_case, na.rm = T),
            state_first_sah = min(earliest_sah, na.rm = T)) %>%
  right_join(df, by = "state_fips") %>%
  mutate(days_since_state_first_sah = as.numeric(difftime(date, state_first_sah)),
         days_since_state_first_case = as.numeric(difftime(date, state_first_case)))

sah_since_case <- df %>%
  ggplot() +
  geom_smooth(aes(x = days_since_state_first_case,
                  y = percentage_completely_home,
                  color = Rural_urban_continuum_code_2013,
                  group = Rural_urban_continuum_code_2013),
              se = F) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "maroon") +
  facet_wrap(~Metro_2013) +
#  xlim(-50,75) +
  labs(x = "Days Since First Covid-19 Case",
       y = "Percentage Completely Home",
       title = "Response to First Covid-19 Case by Urban Status",
       subtitle = "Analysis of Counties with Positive Cases and SAHPs",
       color = "USDA Rural-Urban\nContinuum Code") +
  scale_color_viridis_c() +
  theme_minimal()

sah_since_policy <- df %>%
  ggplot() +
  geom_smooth(aes(x = days_since_state_first_sah,
                  y = percentage_completely_home,
                  color = Rural_urban_continuum_code_2013,
                  group = Rural_urban_continuum_code_2013),
              se = F) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "maroon") +
  facet_wrap(~Metro_2013) +
  labs(x = "Days Since Earliest SAHP",
       y = "Percentage Completely Home",
       color = "USDA Rural-Urban\nContinuum Code") +
  scale_color_viridis_c() +
#  xlim(-50, 75) +
  theme_minimal()

sah_combined_plots <- ggarrange(sah_since_case, sah_since_policy, ncol = 1)
sah_combined_plots


#### ####
urban_counts <- df_cross %>% group_by(Rural_urban_continuum_code_2013) %>%
  summarise(count = n()) %>% pull(count) %>% as.character()

df_cross %>% ggplot() +
  geom_boxplot(aes(x = factor(Rural_urban_continuum_code_2013),
                   y = sah_from_first,
                   color = factor(Rural_urban_continuum_code_2013))) +
  scale_y_continuous(breaks = seq(-200, 75, 50)) +
  scale_color_viridis_d()+
  annotate("text", x = 1:9, y = 70, label = paste("N =",urban_counts), colour = "grey50", size = 3) +
  labs(x = "Rural-Urban Continuum Code",
       y = "Days from First Case to SAHP",
       title = "Distribution of Policy Timings by Urban Status") +
  theme(legend.position = "none",
        panel.background = element_rect(color = "white", fill = "grey95"),
        panel.grid.major.y = element_line(color = "light grey", size = 0.1),
        panel.grid.minor.y = element_line(color = "light grey", size = 0.1),
        panel.grid.major.x = element_blank())
