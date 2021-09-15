library(tidyverse)
library(DCPOtools)
library(here)

# Data cleaning and checking ---------------------------------------------------------

sd <- read_csv(here::here("data", "supdem raw survey marginals.tab"), col_types = "cdcddcdc") %>% 
    mutate(item_fam = str_extract(tolower(Item), "^[a-z]+"),
           country = countrycode::countrycode(Country, "country.name", "country.name"),
           dataset = "supdem") %>% 
    rename(year = Year, project = Project) %>% 
    with_min_yrs(2) # Selecting data w. at least two years

claassen_input_raw <- read_csv(here::here("data", "claassen_input_raw.csv"),
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

# load(here("data", "dem_mood_apsr.rda"))
# saveRDS(dem_mood_apsr, here("data", "dem_mood_apsr.rds")) # using rds for later reading as an object

compare_supdem <- readRDS(here("data", "dem_mood_apsr.rds")) %>%
    select(Country, Year, apsr_supdem_trim = SupDem_trim) %>%
    full_join(read.csv(here::here("data", "Support_democracy_ajps.csv")) %>%
                  select(Country, Year, ajps_supdem_trim = SupDem_trim)) %>%
    mutate(diff = apsr_supdem_trim - ajps_supdem_trim)

summary(compare_supdem) # diff all zeros, 1615 NAs in both

compare_supdem %>% 
    filter(!is.na(apsr_supdem_trim) & is.na(ajps_supdem_trim)) # 0 rows: all NAs are shared

