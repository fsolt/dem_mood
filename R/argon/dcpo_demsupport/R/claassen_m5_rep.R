##### Working Environment ####
## The script for creating output was run on the University of Iowa's high-performance computing cluster.
## The working environment requires R version 3.5.1,Rcpp_1.0.0, and rstan_2.18.2. 

library(tidyverse)
library(rstan)
library(DCPOtools)

# Creating claassen_replication_input

# Note that to setup input raw data, one needs raw survey datasets.
# See details at https://github.com/fsolt/DCPOtools

# claassen_input_raw <- DCPOtools:::claassen_setup(vars = read_csv(here::here("data-raw", "mood_dem.csv"),
#                                                                 col_types = "cccccc"),
#                                                 file = "data/claassen_input_raw.csv")

# dcpo_input_raw <- DCPOtools::dcpo_setup(vars = read_csv(here::here("data-raw", "mood_dem.csv"),
#                                                         col_types = "cccccc"),
#                                         file = "data/dcpo_input_raw.csv")

dcpo_input_raw1 <- read_csv(here::here("data", "dcpo_input_raw.csv"), col_types = "cdcddcd") %>% 
    filter(!(str_detect(survey, "army_wvs") & # WVS obs identified as problematic by Claassen 
                 ((country=="Albania" & year==1998) |
                      (country=="Indonesia" & (year==2001 | year==2006)) |
                      (country=="Iran" & year==2000) |
                      (country=="Pakistan" & (year==1997 | year==2001)) | # 1996 in Claassen
                      (country=="Vietnam" & year==2001)) |
                 (str_detect(item, "strong_wvs") &
                      ((country=="Egypt" & year==2012) |
                           (country=="Iran" & (year==2000 | year==2007)) | # 2005 in Claassen
                           (country=="India") |
                           (country=="Pakistan" & (year==1997 | year==2001)) | # 1996 in Claassen
                           (country=="Kyrgyzstan" & (year==2003 | year==2011)) |
                           (country=="Romania" & (year==1998 | year==2005 | year==2012)) |
                           (country=="Vietnam" & year==2001))) |
                 (country %in% c("Puerto Rico", "Northern Ireland", 
                                 "SrpSka Republic", "Hong Kong SAR China")))) %>%
    with_min_yrs(2)

# Note that before running `dataverse::get_file()` below, one should set their personal token and server in their system environment first: 
# Sys.setenv("DATAVERSE_KEY" = "exampleToken")
# Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")
# These values can be set to persist across R sessions using `usethis::edit_r_environ()`

if (!file.exists(here::here("data", "supdem raw survey marginals.tab"))) {
    tempfile <- dataverse::get_file("supdem raw survey marginals.tab", "doi:10.7910/DVN/HWLW0J") # AJPS replication file
    
    writeBin(tempfile, here::here("data", "supdem raw survey marginals.tab"))
}

supdem <- read_csv(here::here("data", "supdem raw survey marginals.tab"), col_types = "cdcddcdc")

supdem_cy <- supdem %>%                                             # 1390 obs
    janitor::clean_names() %>% 
    mutate(old_country = country,
           country = countrycode::countrycode(old_country, "country.name", "country.name")) %>%
    dplyr::select(country, year, project) %>% 
    distinct()

claassen_input_cy <- read_csv(here::here("data", "dcpo_input_raw.csv"),
                              col_types = "cdcddcd") %>%           # 1864 obs
    mutate(p_dcpo = str_extract(survey, "^[a-z]+"), 
           project = case_when(p_dcpo == "afrob" ~ "afb",
                               p_dcpo == "amb" ~ "lapop",
                               p_dcpo == "arabb" ~ "arb",
                               p_dcpo == "asiab" ~ "asb",
                               p_dcpo == "asianb" ~ "asnb",
                               p_dcpo == "neb" ~ "ndb",
                               p_dcpo == "sasianb" ~ "sab",
                               TRUE ~ p_dcpo),
           y_dcpo = year) %>%
    dplyr::select(country, year, y_dcpo, survey, project) %>% 
    unique()

no_problems <- inner_join(supdem_cy, claassen_input_cy)             # 1299 obs

needed <- anti_join(supdem_cy, claassen_input_cy)                   # 92 obs

available <- anti_join(claassen_input_cy, supdem_cy)                # 566 obs

year_fixes <- left_join(needed, available, by = c("country", "project")) %>% # 89 obs
    mutate(diff = year.x - year.y) %>% 
    group_by(country, project, year.x) %>% 
    mutate(closest_to_claassen = min(abs(diff))) %>% 
    ungroup() %>% 
    group_by(country, project, year.y) %>% 
    mutate(closest_to_dcpo = min(abs(diff))) %>% 
    ungroup() %>% 
    filter(closest_to_claassen == abs(diff) & closest_to_dcpo == abs(diff) & abs(diff) <= 3) %>% 
    filter(!(country == "Egypt" & year.x == 2014 & survey == "afrob5")) # double match (it's really afrob6)

cys_crosswalk <- year_fixes %>% 
    select(country, y_dcpo, y_claassen = year.x, survey)

missing_cyps <- anti_join(needed, year_fixes,  by = c("country", "year" = "year.x", "project")) # 3 obs; listed in issue #5 (Bahrain now in)

cys_to_drop <- anti_join(available, year_fixes, by = c("country", "year" = "year.y", "project")) %>% # 477 obs
    select(-y_dcpo)

claassen_replication_input_raw1 <- read_csv(here::here("data", "claassen_input_raw.csv"), col_types = "cdcddcd") %>% 
    filter(!(str_detect(item, "army_wvs") & # WVS obs identified as problematic by Claassen 
                 ((country=="Albania" & year==1998) |
                      (country=="Indonesia" & (year==2001 | year==2006)) |
                      (country=="Iran" & year==2000) |
                      (country=="Pakistan" & (year==1997 | year==2001)) | #in DCPO,it is 1997 instead of 1996
                      (country=="Vietnam" & year==2001)) |
                 (str_detect(item, "strong_wvs") &
                      ((country=="Egypt" & year==2012) |
                           (country=="Iran" & (year==2000 | year==2007)) | #in DCPO, the year is 2007 rather than 2005
                           (country=="India") |
                           (country=="Pakistan" & (year==1997 | year==2001)) | #in DCPO,it is 1997 instead of 1996
                           (country=="Kyrgyzstan" & (year==2003 | year==2011)) |
                           (country=="Romania" & (year==1998 | year==2005 | year==2012)) |
                           (country=="Vietnam" & year==2001))))) %>% 
    mutate(p_dcpo = str_extract(survey, "^[a-z]+"), 
           project = case_when(p_dcpo == "afrob" ~ "afb",
                               p_dcpo == "amb" ~ "lapop",
                               p_dcpo == "arabb" ~ "arb",
                               p_dcpo == "asiab" ~ "asb",
                               p_dcpo == "asianb" ~ "asnb",
                               p_dcpo == "neb" ~ "ndb",
                               p_dcpo == "sasianb" ~ "sab",
                               TRUE ~ p_dcpo),
           i_dcpo = item,
           i_claassen = paste(str_replace(item, "_.*", "_"), p_dcpo)) %>% 
    left_join(cys_crosswalk, by = c("country", "year" = "y_dcpo", "survey")) %>% 
    anti_join(cys_to_drop, by = c("country", "year", "survey")) %>% # surveys unused by Claassen
    mutate(year = if_else(!is.na(y_claassen), y_claassen, year)) %>% # use Claassen's year codings
    mutate(item = if_else(item == "strong_amb_1" & year == 2004, "strong_amb_2", item)) %>% # items conflated in amb_combo file
    with_min_yrs(2) %>% 
    DCPOtools::format_claassen() %>% 
    pluck("data") %>% 
    mutate(p_dcpo = str_extract(survey, "^[a-z]+"), 
           project = case_when(p_dcpo == "afrob" ~ "afb",
                               p_dcpo == "amb" ~ "lapop",
                               p_dcpo == "arabb" ~ "arb",
                               p_dcpo == "asiab" ~ "asb",
                               p_dcpo == "asianb" ~ "asnb",
                               p_dcpo == "neb" ~ "ndb",
                               p_dcpo == "sasianb" ~ "sab",
                               TRUE ~ p_dcpo),
           i_dcpo = item,
           i_claassen0 = paste0(str_replace(item, "_.*", "_"), project),
           i_claassen = case_when(i_claassen0 == "army_afb" ~ "army_afrob",
                                  i_claassen0 == "strong_afb" ~ "strong_afrob",
                                  i_claassen0 == "threestate_afb" ~ "threestate_afrob",
                                  i_claassen0 == "party_afb" ~ "party_afrob",
                                  i_claassen0 == "election_sab" ~ "elec_sab",
                                  TRUE ~ i_claassen0))

supdem1 <- supdem %>%                                             # 1390 obs
    janitor::clean_names() %>% 
    mutate(old_country = country,
           country = countrycode::countrycode(old_country, "country.name", "country.name"),
           i_claassen0 = tolower(item),
           i_claassen = case_when(str_detect(i_claassen0, "strong_arb") ~ "strong_arb",
                                  str_detect(i_claassen0, "strong_lapop") ~ "strong_lapop",
                                  str_detect(i_claassen0, "strong_pew") ~ "strong_pew",
                                  TRUE ~ i_claassen0),
           year = if_else(c_abb == "INS" & year == 2010 & i_claassen0 == "strong_pew",
                          2011,
                          year)) %>% 
    with_min_yrs(2) 

more_cys_to_drop <- anti_join(claassen_replication_input_raw1, supdem1,
                              by = c("country", "year", "i_claassen")) # strange assortment of excluded country-year-items

claassen_replication_input_raw2 <- read_csv(here::here("data", "claassen_input_raw.csv"), col_types = "cdcddcd") %>% 
    filter(!(str_detect(item, "army_wvs") & # WVS obs identified as problematic by Claassen 
                 ((country=="Albania" & year==1998) |
                      (country=="Indonesia" & (year==2001 | year==2006)) |
                      (country=="Iran" & year==2000) |
                      (country=="Pakistan" & (year==1997 | year==2001)) | #in DCPO,it is 1997 instead of 1996
                      (country=="Vietnam" & year==2001)) |
                 (str_detect(item, "strong_wvs") &
                      ((country=="Egypt" & year==2012) |
                           (country=="Iran" & (year==2000 | year==2007)) | #in DCPO, the year is 2007 rather than 2005
                           (country=="India") |
                           (country=="Pakistan" & (year==1997 | year==2001)) | #in DCPO,it is 1997 instead of 1996
                           (country=="Kyrgyzstan" & (year==2003 | year==2011)) |
                           (country=="Romania" & (year==1998 | year==2005 | year==2012)) |
                           (country=="Vietnam" & year==2001))))) %>% 
    mutate(p_dcpo = str_extract(survey, "^[a-z]+"), 
           project = case_when(p_dcpo == "afrob" ~ "afb",
                               p_dcpo == "amb" ~ "lapop",
                               p_dcpo == "arabb" ~ "arb",
                               p_dcpo == "asiab" ~ "asb",
                               p_dcpo == "asianb" ~ "asnb",
                               p_dcpo == "neb" ~ "ndb",
                               p_dcpo == "sasianb" ~ "sab",
                               TRUE ~ p_dcpo),
           i_claassen0 = paste0(str_replace(item, "_.*", "_"), project),
           i_claassen = case_when(i_claassen0 == "army_afb" ~ "army_afrob",
                                  i_claassen0 == "strong_afb" ~ "strong_afrob",
                                  i_claassen0 == "threestate_afb" ~ "threestate_afrob",
                                  i_claassen0 == "party_afb" ~ "party_afrob",
                                  i_claassen0 == "election_sab" ~ "elec_sab",
                                  TRUE ~ i_claassen0)) %>% 
    left_join(cys_crosswalk, by = c("country", "year" = "y_dcpo", "survey")) %>% 
    anti_join(cys_to_drop, by = c("country", "year", "survey")) %>% # surveys unused by Claassen
    mutate(year = if_else(!is.na(y_claassen), y_claassen, year)) %>% # use Claassen's year codings
    anti_join(more_cys_to_drop, by = c("country", "year", "item")) %>% 
    mutate(item = if_else(item == "strong_amb_1" & year == 2004, "strong_amb_2", item)) %>% # items conflated in amb_combo file
    with_min_yrs(2)

anti_join(supdem1, claassen_replication_input_raw2, 
          by = c("country", "year", "i_claassen"))

claassen_replication_input <- DCPOtools::format_claassen(claassen_replication_input_raw2)
save(claassen_replication_input, file = here::here("data", "claassen_replication_input.rda"))

# claassen_replication_input has 5 fewer observations than supdem (3 are no longer available and 2 seem to have never existed)
# and 1 more survey item (Claassen conflates two Arabbarometer items with different response categories)


# Creating claassen_replication_output

download.file("https://github.com/fsolt/dcpo_dem_mood/raw/master/data/claassen_replication_input.rda", "data/claassen_replication_input.rda")

load("data/claassen_replication_input.rda")

iter <- 3000

claassen_m5 <- stan(file = 'R/supdem.stan.mod5.stan',
                    data = claassen_replication_input,
                    iter = iter,
                    chains= 4,
                    cores = 4,
                    thin = iter/500,
                    pars = c("mu_lambda","sigma_lambda","sigma_delta","sigma_theta","phi","lambda","delta","theta",
                             "x_pred","log_lik"),
                    control = list(adapt_delta=0.99, stepsize=0.02, max_treedepth=11))

save(claassen_replication_input, claassen_m5,
     file = "data/claassen_replication_output.rda")
