library(tidyverse)
library(DCPOtools)
library(here)

# Data cleaning and checking ---------------------------------------------------------

if (!file.exists(here("data", "supdem raw survey marginals.tab"))) {
  tempfile <- dataverse::get_file("supdem raw survey marginals.tab", "doi:10.7910/DVN/HWLW0J") # AJPS replication file, not included in APSR replication
  
  writeBin(tempfile, here("data", "supdem raw survey marginals.tab"))
  rm(tempfile)
}

sd <- read_csv(here("data", "supdem raw survey marginals.tab"), col_types = "cdcddcdc") %>% 
    mutate(item_fam = str_extract(tolower(Item), "^[a-z]+"),
           country = countrycode::countrycode(Country, "country.name", "country.name"),
           dataset = "supdem") %>% 
    rename(year = Year, project = Project) %>% 
    with_min_yrs(2) # Selecting data w. at least two years

claassen_input_raw <- read_csv(here("data", "claassen_input_raw.csv"),
                               col_types = "cdcddcd")

claassen_input_raw <- claassen_input_raw %>%
    filter(!((
        str_detect(item, "army_wvs") &
            # WVS obs identified as problematic by Claassen
            ((country == "Albania" & year == 1998) |
                 (country == "Indonesia" &
                      (year == 2001 | year == 2006)) |
                 (country == "Iran" & year == 2000) |
                 (country == "Pakistan" &
                      (year == 1997 | year == 2001)) | # 1996 in Claassen
                 (country == "Vietnam" & year == 2001)
            )
    ) |
        (
            str_detect(item, "strong_wvs") &
                ((country == "Egypt" & year == 2012) |
                     (country == "Iran" &
                          (year == 2000 | year == 2007)) | # 2005 in Claassen
                     (country == "India") |
                     (country == "Pakistan" &
                          (year == 1997 | year == 2001)) | # 1996 in Claassen
                     (country == "Kyrgyzstan" &
                          (year == 2003 | year == 2011)) |
                     (country == "Romania" &
                          (year == 1998 | year == 2005 | year == 2012)) |
                     (country == "Vietnam" & year == 2001)
                )
        ) |
        (
            country %in% c(
                "Puerto Rico",
                "Northern Ireland",
                "SrpSka Republic",
                "Hong Kong SAR China"
            )
        ))) %>%
    with_min_yrs(2)

claassen_input <- DCPOtools:::format_claassen(claassen_input_raw)

cri <- claassen_input$data %>% 
    mutate(p_dcpo = str_extract(survey, "^[a-z]+"), 
           project = case_when(p_dcpo == "afrob" ~ "afb",
                               p_dcpo == "amb" ~ "lapop",
                               p_dcpo == "arabb" ~ "arb",
                               p_dcpo == "asiab" ~ "asb",
                               p_dcpo == "asianb" ~ "asnb",
                               p_dcpo == "neb" ~ "ndb",
                               p_dcpo == "sasianb" ~ "sab",
                               TRUE ~ p_dcpo),
           item_fam = str_extract(item, "^[a-z]+"),
           item_fam = if_else(item_fam == "election", "elec", item_fam),
           dataset = "cri")

supdem_cri <- full_join(sd, cri, by = c("country", "year", "item_fam", "project"))

no_problems <- inner_join(sd %>% select(-dataset), 
                          cri %>% select(-dataset),
                          by = c("country", "year", "item_fam", "project")) %>% # 3405 obs
    mutate(prop_cls = Response/Sample,
           prop_ours = x/samp,
           diff = prop_cls - prop_ours,
           diff_x = round(Response - x),
           diff_samp = round(Sample - samp)) %>% 
    select(-CAbb, -COWCode)

saveRDS(no_problems, here("data", "data_comparison.rds"))

# Correlation -------------------------------------------------------------------------

needed <- anti_join(sd, cri, by = c("country", "year", "item_fam", "project")) 
# 309 obs Old comment: 311 obs 
# note: I have confirmed that Dominican Rep lb 2003; Spain lb 2015; and Bahrain wvs 2014 do not exist,
# which account for 6 (and so drop Bahrain 2006, accounting for 4 more)

available <- anti_join(cri, sd, by = c("country", "year", "item_fam", "project")) 
# 1310 obs; Old comment: 1420 obs

corrs <- no_problems %>%
    group_by(country) %>% 
    summarize(corr = cor(Response/Sample, x/samp)) %>% 
    arrange(corr)

saveRDS(corrs, here("data", "data_correlation.rds"))

## Does Claassen use the exact same data in both articles?  Yep

if (!file.exists(here("data", "dem_mood_apsr.rds"))) {
  load(here("data", "dem_mood_apsr.RData"))
  saveRDS(x, here("data", "dem_mood_apsr.rds")) # using rds for later reading as an object
}

compare_supdem <- readRDS(here("data", "dem_mood_apsr.rds")) %>%
    select(Country, Year, apsr_supdem_trim = SupDem_trim) %>%
    full_join(read.csv(here("data", "Support_democracy_ajps.csv")) %>%
                  select(Country, Year, ajps_supdem_trim = SupDem_trim)) %>%
    mutate(diff = apsr_supdem_trim - ajps_supdem_trim)

summary(compare_supdem) # diff all zeros, 1615 NAs in both

compare_supdem %>% 
    filter(!is.na(apsr_supdem_trim) & is.na(ajps_supdem_trim)) # 0 rows: all NAs are shared


# Regression -------------------------------------------------------------------------

load(here("data", "correct_cls_apsr.rda"))
df_control <- readRDS(here("data", "control_variables_apsr.rds"))

df_theta <- map_dfr(correct_cls_apsr, ~select(., country, year, theta)) %>% 
    group_by(country, year) %>% 
    summarise(theta = mean(theta))

df_apsr <- left_join(df_theta, df_control)
saveRDS(df_apsr, here("data", "data_apsr_corrected.rds"))

dcpo_input_raw1 <- read_csv(here("data", "dcpo_input_raw.csv"), col_types = "cdcddcd") %>% 
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

if (!file.exists(here("data", "supdem raw survey marginals.tab"))) {
    tempfile <- dataverse::get_file("supdem raw survey marginals.tab", "doi:10.7910/DVN/HWLW0J") # AJPS replication file
    
    writeBin(tempfile, here("data", "supdem raw survey marginals.tab"))
}

supdem <- read_csv(here("data", "supdem raw survey marginals.tab"), col_types = "cdcddcdc")

supdem_cy <- supdem %>%                                             # 1390 obs
    janitor::clean_names() %>% 
    mutate(old_country = country,
           country = countrycode::countrycode(old_country, "country.name", "country.name")) %>%
    dplyr::select(country, year, project) %>% 
    unique()

claassen_input_cy <- read_csv(here("data", "dcpo_input_raw.csv"),
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

no_problems <- inner_join(supdem_cy, claassen_input_cy)             # 1298 obs

needed <- anti_join(supdem_cy, claassen_input_cy)                   # 93 obs

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

missing_cyps <- anti_join(needed, year_fixes,  by = c("country", "year" = "year.x", "project")) # 4 obs; listed in issue #5 

cys_to_drop <- anti_join(available, year_fixes, by = c("country", "year" = "year.y", "project")) %>% # 477 obs
    select(-y_dcpo)

claassen_replication_input_raw1 <- read_csv(here("data", "claassen_input_raw.csv"), col_types = "cdcddcd") %>% 
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
    anti_join(cys_to_drop, by = c("country", "year", "survey")) %>% # surveys unused by Claassen
    left_join(cys_crosswalk, by = c("country", "year" = "y_dcpo", "survey")) %>% 
    mutate(year = if_else(!is.na(y_claassen), y_claassen, year)) %>% # use Claassen's year codings
    mutate(item = if_else(item == "strong_amb_1" & year == 2004, "strong_amb_2", item)) %>% # items conflated in amb_combo file
    with_min_yrs(2)

claassen_replication_input <- DCPOtools::format_claassen(claassen_replication_input_raw1)

# save(claassen_replication_input, file = here("data", "claassen_replication_input.rda"))
# 
# load(here("data", "claassen_replication_input.rda"))

claassen_m5 <- rstan::stan(file = 'R/argon/dcpo_demsupport/R/supdem.stan.mod5.stan',
                           data = claassen_replication_input,
                           iter = 3000,
                           chains = 4,
                           cores = 4,
                           thin = 3,
                           pars = c("mu_lambda", "sigma_lambda", "sigma_delta", "sigma_theta", "phi", "lambda", "delta", "theta", "x_pred","log_lik"),
                           control = list(adapt_delta=0.99, stepsize=0.02, max_treedepth=11))

save(claassen_m5, file = load(here("data", "claassen_m5.rda")))


# Preparing apsr data --------------------------------------------------------------

load(here("output", "correct_cls_apsr.rda"))
saveRDS(correct_cls_apsr, here("data", "correct_cls_apsr.rds"))
