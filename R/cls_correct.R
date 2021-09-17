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


load(here("data", "claassen_m5_3k_07-27-15-40.rda")) 
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

save(df_clsMean, file = here::here("data", "estimates_clsMean.RDS"))
