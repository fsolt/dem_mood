library(tidyverse)
library(rstan)

load("data/expanded_claassen_input.rda")

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

save(expanded_claassen_input, exp_claassen_m5,
     file = str_c("data/exp_claassen_m5_", round(iter/1000), "k_",
                  str_replace_all(Sys.time(), "[ :]", "-") %>%
                      str_replace("20\\d{2}-", "") %>%
                      str_replace("-\\d{2}$", ""),
                  ".rda"))
