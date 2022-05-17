# The profile to generate correct data

if (!require(pacman)) install.packages("pacman")
library(pacman)
# load all the packages you will use below 
p_load(
  here,
  purrr,
  countrycode,
  tidyverse,
  rstan,
  osfr
) 


osf_retrieve_file("uvn4g") %>%
  osf_download(here::here("data"))  ##corrected

# Functions preload
set.seed(313)
reformat_dcpo_output <- function(x, parameter_name) {
  df_temp <- x %>% 
    as_tibble(.name_repair = ~ls_country) %>% 
    mutate(year = first_year + row_number() - 1) %>% 
    pivot_longer(cols = all_of(ls_country),
                 names_to = "country",
                 values_to = parameter_name) %>%
    arrange(country, year)
  return(df_temp)
}


##################   Create Mean Theta   #############
################ From Claassen Correct Data ##########
load(here::here("data", "claassen_m5_3k_07-27-15-40.rda")) 
load(here::here("data","claassen_replication_input.rda"))
claassen_m5_theta <- rstan::extract(claassen_m5, pars = "theta")  ##137*30 


claassen_replication <- claassen_replication_input$data %>%
  janitor::clean_names() %>% 
  DCPOtools::with_min_yrs(2) 


length(unique(claassen_replication$country)) ##137
min(claassen_replication$year) #1988
max(claassen_replication$year) #2018


ls_year <- 1988:2017
ls_country <- claassen_replication$country %>% unique()
first_year <- min(claassen_replication$year)



df_clsMean <- purrr::map_df(1:1000, function(anEntry) {
  claassen_m5_theta$theta[anEntry,,] %>% 
    reformat_dcpo_output("theta")}) %>% 
  group_by(country, year) %>% 
  summarise(theta = mean(theta))

saveRDS(df_clsMean, file = here::here("output", "estimates_clsMean.RDS"))


################################################################
######## Create Data for Correct Claassen + APSR ###############
################## APSR: correct_cls_apsr #####################
################################################################

df_clsMean <- readRDS(here::here("output", "estimates_clsMean.rds"))
load(here("data","cls_apsr_cntrl.rda"))
load(here::here("data","country_regionUN.rda"))
cpi_95_19_update <- readRDS(here::here("data","cpi95_19_update.rds")) 
vdem_cntrl <- readRDS(here::here("data","control_variables_apsr.rds"))  %>%
  select(1:2,3:6)


cpi_95_19 <- cpi_95_19_update %>%
  select(country,year,cpi_100) %>%
  mutate(cpi = 100 - cpi_100) %>% ## reverse cpi
  select(country,year,cpi)

## Standardize cls_libdem_list Libdem_z, poly, liberal, Corrup
cntrl_z <- vdem_cntrl %>%
  left_join(cpi_95_19, by=c("country", "year")) %>%
  left_join(cls_apsr_cntrl, by=c("country", "year"="Year")) %>%
        mutate(Libdem_z = as.vector(scale(Vdem_libdem)), 
               Polyarchy_z = as.vector(scale(Vdem_polyarchy)),
               Liberal_z = as.vector(scale(Vdem_liberal)),
               Corrup_TI_z = as.vector(scale(cpi))) %>%
        group_by(country) %>% 
        arrange(year, .by_group = TRUE) %>% 
        mutate(Libdem_m1 = lag(Libdem_z)) %>%
        ungroup() %>%
        mutate(ChgDem = Libdem_z - Libdem_m1) %>%
        ungroup() 

saveRDS(cntrl_z, file = here("data","cls_apsr_cntrl.rds"))


# merge with theta variable and produce trim data.

correct_cls_apsr <- cntrl_z  %>%
    filter(year > 1986) %>%
    left_join(df_clsMean,by = c("year", "country"))  %>% 
    mutate(SupDem_trim = ifelse(year < First_yr, NA, theta)) %>%
    select(country, year, First_yr,theta, SupDem_trim,contains("z"),everything()) 
    

save(correct_cls_apsr, file = here("data","correct_cls_apsr.rda"))


