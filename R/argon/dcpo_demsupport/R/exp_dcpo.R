##### Working Environment ####
## The script for creating output was run on the University of Iowa's high-performance computing cluster.
## The working environment requires R version 3.5.1,Rcpp_1.0.0, and rstan_2.18.2. 

library(tidyverse)
library(DCPOtools)
if (!require("DCPO")) install.packages("DCPO", repos = "https://cloud.r-project.org/"); library(DCPO)


# Note that to setup input raw data, one needs raw survey datasets.
# See details at https://github.com/fsolt/DCPOtools

# dcpo_input_raw <- DCPOtools::dcpo_setup(vars = read_csv(here::here("data-raw", "mood_dem.csv"),
#                                                        col_types = "cccccc"),
#                                        file = "data/dcpo_input_raw.csv")

dcpo_input_raw1 <- read_csv(here::here("data", "dcpo_input_raw.csv"), 
                            col_types = "cdcddcd") %>%
    with_min_yrs(2)

exp_dcpo_input <- DCPOtools::format_dcpo(dcpo_input_raw1,
                                         scale_q = "church_wvs",
                                         scale_cp = 2)
save(exp_dcpo_input, file = here::here("data", "exp_dcpo_input.rda"))


download.file("https://github.com/fsolt/dcpo_dem_mood/raw/master/data/exp_dcpo_input.rda", "data/dcpo_input.rda")

load("data/dcpo_input.rda")

dcpo_input <- exp_dcpo_input

iter <- 2000

dcpo_output <- dcpo(dcpo_input,
                    iter = iter,
                    chains = 4,
                    thin = iter/500, # this yields 250 draws per chain, 1000 draws total
                    pars = c("sd_delta","sd_theta_evolve", "sd_sigma_evolve", "sigma","phi","beta","alpha","delta","theta","y_r_pred","log_lik"))

save(dcpo_input, dcpo_output,
     file = "data/exp_dcpo_output.rda")
