##### Working Environment ####
## The script for creating output was run on the University of Iowa's high-performance computing cluster.
## The working environment requires R version 3.5.1,Rcpp_1.0.0, and rstan_2.18.2. 

library(tidyverse)
library(rstan)
library(DCPOtools)


# Creating exp_claassen_input by using expanded data
# Note that to setup input raw data, one needs raw survey datasets.
# See details at https://github.com/fsolt/DCPOtools


# claassen_input_raw <- DCPOtools:::claassen_setup(vars = read_csv(here::here("data-raw", "mood_dem.csv"),
#                                                                  col_types = "cccccc"),
#                                                  file = "data/claassen_input_raw.csv") 
#Note that claassen_input_raw.csv should be in data folder if one run claassen_m5_rep.R first.

claassen_input_raw <- read_csv(here::here("data", "claassen_input_raw.csv"),
                               col_types = "cdcddcd")

claassen_input_raw1 <- claassen_input_raw %>% 
    filter(!((str_detect(item, "army_wvs") & # WVS obs identified as problematic by Claassen 
                  ((country=="Albania" & year==1998) |
                       (country=="Indonesia" & (year==2001 | year==2006)) |
                       (country=="Iran" & year==2000) |
                       (country=="Pakistan" & (year==1997 | year==2001)) | # 1996 in Claassen
                       (country=="Vietnam" & year==2001))) |
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

exp_claassen_input <- DCPOtools:::format_claassen(claassen_input_raw1)
save(exp_claassen_input, file = here::here("data", "exp_claassen_input.rda"))

# Creating expanded data output

load("data/exp_claassen_input.rda")

iter <- 3000

exp_claassen_m5 <- stan(file = 'R/supdem.stan.mod5.stan',
                    data = expanded_claassen_input,
                    iter = iter,
                    chains= 4,
                    cores = 4,
                    thin = iter/500,
                    pars = c("mu_lambda","sigma_lambda","sigma_delta","sigma_theta","phi","lambda","delta","theta",
                             "x_pred","log_lik"),
                    control = list(adapt_delta=0.99, stepsize=0.02, max_treedepth=11))

save(exp_claassen_input, exp_claassen_m5,
     file = "data/exp_claassen_output.rda")

