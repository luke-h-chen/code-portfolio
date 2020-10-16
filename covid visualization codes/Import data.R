# Use these first two lines of code to install the SafeGraphR package from Github
#install.packages('remotes')
#remotes::install_github('SafeGraphInc/SafeGraphR')
library(SafeGraphR)
library(lubridate)
library(aws.s3)
library(tidyverse)
library(jsonlite)
library(tidycensus)
library(here)
library(haven)
library(readxl)

# Prepping the R environment to be able to import the data. The Access Keys and stuff are obtained from SafeGraph
s3_bucket <- "sg-c19-response/"
Sys.setenv("AWS_ACCESS_KEY_ID"       = "SBIWI7QS504XKIST4JQS"
           , "AWS_SECRET_ACCESS_KEY" = "Kp9mt9AWCcJezVFeLdtC2nHfnoDLLwEoAedgMXyW"
           , "AWS_DEFAULT_REGION"    = "us-east-1"
           , "AWS_S3_ENDPOINT"       = "s3.wasabisys.com"
)

# Command from the SafeGraphR package to download the data from the big data pool hosted on AWS
fetch_safegraph_data <- safegraph_aws(
  path = "G:/safegraph",
  dataset = "distancing",
  bucket_only = FALSE,
  base_url = "s3.wasabisys.com",
  key = "SBIWI7QS504XKIST4JQS",
  secret = "Kp9mt9AWCcJezVFeLdtC2nHfnoDLLwEoAedgMXyW",
  region = "",
  prefix = "2020/"
)


# After downloading the data, the following lines load the data files from their saved location on the computer
# The command is supposed to be capable of aggregating by county but it results in weird, buggy data so just load the non-aggregated, CBG-level data

#First, this is a list/ vector of the variables we actually care about
variables <- c("origin_census_block_group",
               "device_count",
               "distance_traveled_from_home",
               "completely_home_device_count",
               "median_home_dwell_time",
               "part_time_work_behavior_devices",
               "full_time_work_behavior_devices",
               "delivery_behavior_devices",
               "median_non_home_dwell_time",
               "candidate_device_count",
               "median_percentage_time_home")

# Read in the data from the downloaded files.
# Unfortunately, my computer lacks memory and the read_distancing function is a little wonky so we have to do it "the dumb way"
safegraph1 <- read_distancing(mdy("02/01/2020"), mdy("07/18/2020"), gen_fips = TRUE, by = FALSE,
                             dir = "G:/safegraph/",
                             select = variables)

safegraph2 <- read_distancing(mdy("07/19/2020"), mdy("08/01/2020"), gen_fips = TRUE, by = FALSE,
                              dir = "G:/safegraph/",
                              select = variables)

safegraph3 <- read_distancing(mdy("08/02/2020"), mdy("08/16/2020"), gen_fips = TRUE, by = FALSE,
                              dir = "G:/safegraph/",
                              select = variables)

safegraph4 <- read_distancing(mdy("08/17/2020"), mdy("08/31/2020"), gen_fips = TRUE, by = FALSE,
                              dir = "G:/safegraph/",
                              select = variables)

safegraph5 <- read_distancing(mdy("09/01/2020"), mdy("09/15/2020"), gen_fips = TRUE, by = FALSE,
                              dir = "G:/safegraph/",
                              select = variables)

safegraph6 <- read_distancing(mdy("09/16/2020"), mdy("09/30/2020"), gen_fips = TRUE, by = FALSE,
                              dir = "G:/safegraph/",
                              select = variables)

# We will combine these disjoint data sets into a more comprehensive, use-able data set later after retrieving
# population data which will be used to adjust for sampling rates
# Import Census (ACS 2019 1-year estimate) data

#census_api_key("2a9a460a07d0e4d2087c2a72ea0e1e0aa529375c") # Census API key should be unique for every user

#var_key_2018 <- load_variables(2018, "acs5", cache = T) # Retrieves ACS codebook for the 2014-2018 ACS5
#var_key_2019 <- load_variables(2019,"acs1", cache = T) # Retrieves ACS codebook for the 2019 ACS1

county_pop_2018 <- get_acs(geography = "county", variables = c(pop_2018 = "B01001_001"), year = 2018, survey = "acs5")
#county_pop_2019 <- get_acs(geography = "county", variables = c(unweighted_pop = "B01001_001"), year = 2019, survey = "acs1")

# Using 2018 data is recommended because 2019 data only has data on counties with population > 65k

# Get rid of territories and create columns for county and state names
county_pop_2018 <- county_pop_2018 %>% filter(GEOID < 60000) %>%
  separate(NAME, into = c("County", "State"), sep = ", ", remove = FALSE)

# Join the disjoint mobility data sets
safegraph_merged <- rbind(safegraph1, safegraph2, safegraph3, safegraph4, safegraph5, safegraph6)

# Aggregate data at the county level
safegraph_merged <- safegraph_merged %>% group_by(state_fips, county_fips, date) %>%
  summarise(tot_device_count = sum(device_count),
            distance_traveled_from_home = sum(distance_traveled_from_home),
            completely_home_device_count = sum(completely_home_device_count),
            median_percentage_time_home = weighted.mean(median_percentage_time_home, w = device_count))

# Create a variable for percent of devices completely home using hierarchical Bayesian shrinkage and also GEOID variable
safegraph_merged <- safegraph_merged %>%
  mutate(percentage_completely_home = hb_shrink(completely_home_device_count, tot_device_count)*100,
         state_fips = str_pad(state_fips, width = 2, pad = "0", side = "left"),
         county_fips = str_pad(county_fips, width = 3, pad = 0, side = "left"),
         GEOID = str_c(state_fips, county_fips))

safegraph_out <- safegraph_merged %>% left_join(county_pop_2018, by = "GEOID") %>%
  rename("pop_2018" = "estimate") %>% select(-variable, -moe, -NAME)

# Export data to a csv
write.csv(safegraph_out, here("/clean_data/updated_safegraph_data.csv"), row.names = F)

rm(list = ls()) # Clear environment for next step

# Import IAT data
iat <- read.csv("G:/IAT/Race_IAT.public.2020.January-June_Abridged.csv/Race IAT.public.2020.Jan-June.csv")
iat <- iat %>% filter(countryres_num == 1) %>%
  select(date, D_biep.White_Good_all, birthmonth, birthyear, birthSex, num_002, raceombmulti, ethnicityomb, edu,
         religion2014, religionid, STATE, CountyNo, deathanxiety001, fearcovid001, fearcovid002,fearcovid003, fearcovid004,
         fearcovid005, fearcovid006, fearcovid007, fearcovid008)

iat_race_count <- iat %>% filter(STATE %in% c(state.abb, "DC")) %>% drop_na(D_biep.White_Good_all) %>%
  count(STATE, CountyNo, name = "n_race")

iat_race <-iat %>% filter(STATE %in% c(state.abb, "DC")) %>% drop_na(D_biep.White_Good_all) %>%
  group_by(STATE, CountyNo) %>% summarise(median_att = median(D_biep.White_Good_all),
                                          mean_att = mean(D_biep.White_Good_all)) %>% left_join(iat_race_count, by = c("STATE", "CountyNo"))

iat_covid_count <- iat %>% filter(STATE %in% c(state.abb,"DC")) %>%
  drop_na(fearcovid001, fearcovid002, fearcovid003, fearcovid004, fearcovid005, fearcovid006, fearcovid007, fearcovid008) %>%
  count(STATE, CountyNo, name = "n_fearcovid")

iat_covid <- iat %>% filter(STATE %in% c(state.abb,"DC")) %>%
  drop_na(fearcovid001, fearcovid002, fearcovid003, fearcovid004, fearcovid005, fearcovid006, fearcovid007, fearcovid008) %>%
  group_by(STATE, CountyNo) %>%
  summarise(mean_fear001 = mean(fearcovid001),
            mean_fear002 = mean(fearcovid002),
            mean_fear003 = mean(fearcovid003),
            mean_fear004 = mean(fearcovid004),
            mean_fear005 = mean(fearcovid005),
            mean_fear006 = mean(fearcovid006),
            mean_fear007 = mean(fearcovid007),
            mean_fear008 = mean(fearcovid008)) %>%
  left_join(iat_covid_count, by = c("STATE", "CountyNo"))

iat_religion_count <- iat %>% filter(STATE %in% c(state.abb, "DC"), religion2014!=7) %>% drop_na(religion2014, religionid) %>%
  count(STATE, CountyNo, name = "n_religion")
iat_religion <- iat %>% filter(STATE %in% c(state.abb,"DC"), religion2014!=7) %>% drop_na(religion2014, religionid) %>%
  group_by(STATE, CountyNo) %>%
  summarise(pct_catholic = mean(religion2014 == 2),
            pct_protestant = mean(religion2014==3),
            pct_hindu = mean(religion2014 == 4),
            pct_eastern = mean(religion2014==1),
            pct_jewish = mean(religion2014 == 5),
            pct_muslim = mean(religion2014==6),
            pct_other = mean(religion2014 == 8),
            mean_religiosity = mean(religionid)) %>%
  left_join(iat_religion_count, by = c("STATE", "CountyNo"))

iat_education_count <- iat %>% filter(STATE %in% c(state.abb, "DC")) %>% drop_na(edu) %>% group_by(STATE,CountyNo) %>%
  count(name = "n_education")
iat_education <- iat %>% filter(STATE %in% c(state.abb, "DC")) %>% drop_na(edu) %>% group_by(STATE,CountyNo) %>%
  group_by(STATE,CountyNo) %>%
  summarise(nohighschool = sum(edu < 4),
            highschool = sum(edu %in% 4:6),
            college = sum(edu %in% c(7,8)),
            masters = sum(edu == 9),
            professional = sum(edu %in% c(10,11,13,14)),
            doctorate = sum(edu == 12)) %>%
  left_join(iat_education_count, by = c("STATE", "CountyNo")) %>% group_by(STATE, CountyNo) %>%
  summarise(nohighschool = nohighschool / n_education,
            highschool = highschool / n_education,
            college = college / n_education,
            masters = masters/ n_education,
            professional = professional / n_education,
            doctorate = doctorate / n_education,
            n_education = n_education)

iat_complete <- full_join(iat_covid,iat_education, by = c("STATE", "CountyNo")) %>%
  full_join(iat_race, by = c("STATE", "CountyNo")) %>%
  full_join(iat_religion, by = c("STATE", "CountyNo"))

data("fips_codes")

iat_complete <- iat_complete %>%
  mutate(CountyNo = str_pad(CountyNo, width = 3, pad = 0, side = "left")) %>%
  left_join(fips_codes, by = c("CountyNo" = "county_code", "STATE" = "state"))


write.csv(iat_complete, here("/clean_data/iat_data.csv"), row.names = F)

rm(list=ls()) #Clear environment for next step

# New York Times Case and mask usage data
nyt_case <- read_csv("C://Users/Administrator/Documents/Github/covid-19-data/us-counties.csv")
nyt_masks <- read_csv("C://Users/Administrator/Documents/Github/covid-19-data/mask-use/mask-use-by-county.csv")
nyt_all <- left_join(nyt_case, nyt_masks, by = c("fips" = "COUNTYFP")) %>%
  rename(mask_never = NEVER,
         mask_rarely = RARELY,
         mask_sometimes = SOMETIMES,
         mask_frequently = FREQUENTLY,
         mask_always = ALWAYS)

write.csv(nyt_all, here("/clean_data/nyt_data.csv"), row.names = F)

rm(list=ls()) #Clear environment for next step

# USDA Poverty, Education, Population, and Employment data
usda_unemp <- read_xls(here("data/Unemployment.xls"), sheet = "Unemployment Med HH Income") %>%
  separate(area_name, into = c("area_name", NULL), sep = ",")
usda_educ <- read_xls(here("data/Education.xls"), sheet = "Education 1970 to 2018")
usda_pov <- read_xls(here("data/PovertyEstimates.xls"), sheet = "Poverty Data 2018")
usda_pop <- read_xls(here("data/PopulationEstimates.xls"), sheet = "Population Estimates 2010-19")

usda_merged <- right_join(usda_unemp, usda_educ,
                         by = c("FIPStxt" = "FIPS Code", "Stabr" = "State", "area_name" = "Area name")) %>%
  left_join(usda_pov, by = c("FIPStxt", "Stabr", "area_name" = "Area_name")) %>%
  left_join(usda_pop, by = c("FIPStxt", "Stabr" = "State", "area_name" = "Area_Name")) %>%
  select(-contains("CI90"))

write.csv(usda_merged, here("clean_data/usda_county_data.csv"),row.names = F)

rm(list=ls()) # Clean environment

# Get CDC stay at home order data
cdc_sah_url <- "https://data.cdc.gov/resource/qz3x-mf9n.csv?$limit=400000"
cdc_sah <- read_csv(cdc_sah_url) %>%
  select(fips_state, fips_county, order_code, stay_at_home_order, effective_date) %>%
  mutate(fips_state = str_pad(fips_state, side = "left", pad = "0", width = 2),
         fips_county = str_pad(fips_county, side = "left", pad = "0", width = 3),
         GEOID = str_c(fips_state, fips_county)) %>%
  group_by(GEOID) %>%
  arrange(effective_date) %>%
  slice(1) %>%
  ungroup() %>%
  rename("earliest_sah" = "effective_date") %>%
  select(-fips_state, -fips_county)

write.csv(cdc_sah, here("clean_data/SAHP_dates.csv"), row.names = F)

rm(list=ls()) #Clean environment

# Get 2016 Presidential election data
elec_data <- read_csv(here("data/countypres_2000-2016.csv")) %>% filter(year == 2016, party == "republican") %>%
  mutate(r_2016voteshare = candidatevotes / totalvotes,
         FIPS = str_pad(FIPS, width = 5, side = "left", pad = "0")) %>%
  select(FIPS, r_2016voteshare)

write.csv(elec_data, here("clean_data/r_2016voteshare.csv"), row.names = F)

# Climate change belief data
sci_belief_data <- read_csv(here("data/yale_climate_belief_data.csv"))
sci_belief_data <- sci_belief_data %>%
  filter(GeoType == "County") %>%
  mutate(GEOID = str_pad(GEOID, width = 5, side = "left", pad = "0")) %>%
  select(GEOID, grep("Oppose",colnames(sci_belief_data)))

write.csv(sci_belief_data, here("clean_data/sci_belief_data_cleaned.csv"), row.names = F)

# Pew Religion data
relig_data <- read_dta(here("data/religious_census.dta"))
relig_data <- relig_data %>%
  select(fips, evanrate, mprtrate, cathrate) %>%
  mutate(GEOID = str_pad(fips, width = 5, side = "left", pad = "0"),
         evangelical_pct = ifelse(!is.na(evanrate),evanrate/10, 0),
         protestant_pct = ifelse(!is.na(mprtrate), mprtrate/10, 0),
         catholic_pct = ifelse(!is.na(cathrate), cathrate/10, 0)) %>%
  select(GEOID, evangelical_pct, protestant_pct, catholic_pct)

write.csv(relig_data, here("clean_data/religion_data.csv"), row.names = F)
