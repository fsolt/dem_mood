library(tidyverse)
if (!require("DCPO")) install.packages("DCPO", repos = "https://cloud.r-project.org/"); library(DCPO)


download.file("https://github.com/fsolt/dcpo_dem_mood/raw/master/data/dcpo_replication_input.rda", "data/dcpo_replication_input.rda")

load("data/dcpo_replication_input.rda")

iter <- 3000

dcpo_replication_output <- dcpo(dcpo_replication_input,
                                iter = iter,
                                chains = 4,
                                thin = iter/500, # this yields 250 draws per chain, 1000 draws total
                                pars = c("sd_delta","sd_theta_evolve", "sd_sigma_evolve", "sigma","phi","beta","alpha","delta","theta","y_r_pred","log_lik"))

save(dcpo_replication_input, dcpo_replication_output,
     file = str_c("data/church_lb2_rep_", round(iter/1000), "k_",
                  str_replace_all(Sys.time(), "[ :]", "-") %>%
                      str_replace("20\\d{2}-", "") %>%
                      str_replace("-\\d{2}$", ""),
                  ".rda"))
