library(tidyverse)
if (!require("DCPO")) install.packages("DCPO", repos = "https://cloud.r-project.org/"); library(DCPO)

fold <- commandArgs(trailingOnly=TRUE) %>%
    as.numeric()

print(fold)


download.file("https://github.com/fsolt/dcpo_dem_mood/raw/master/data/dcpo_input_con.rda", "data/dcpo_input_con.rda")

load("data/dcpo_input_con.rda")


dcpo_con_kfold <- dcpo_xvt(dcpo_input_con,
                           fold_number = fold, 
                           number_of_folds = 10,
                           iter = 2000)

save(dcpo_con_kfold, 
     file = str_c("data/dcpo_con_fold_",
                  fold,
                  ".rda"))
