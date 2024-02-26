Replication Files for Yue Hu, Yuehong Cassandra Tai, & Frederick Solt, "Revisiting the Evidence on Thermostatic Response to Democratic Change: Degrees of Democratic Support or Researcher Degrees of Freedom?"


The `dem_mood.Rmd` file reproduces all the results in the main text and online supplementary materials.


# Setup

First of all, please set the working directory to where the dataverse file is located, e.g., 

```r
setwd(~/THE ACTUAL PATH/dataverse_files)
```
To smoothly compile the file also requires the following software environment:

- R version >= 4.3.2

Then extract the `renv.zip` in the current directory as a folder of renv/.
Make sure you have the directory structure as below to process the following steps:

    ~/
    |   
    |   apsr.bst  
    |   dem_mood.Rmd
    |   dem-mood-text.bib
    |   readme.txt
    |   renv.lock
    |   
    |   
    +---data
        |   claassen_input_raw.csv
        |   claassen_input.rds
        |   dem_mood_apsr.csv
        |   df_apsr_list.rds
        |   supdem raw survey marginals.csv
        \---highest
            \---lower
                | supdem.stan.mod5-202402171324-1.csv
                | supdem.stan.mod5-202402171324-2.csv
                | supdem.stan.mod5-202402171324-3.csv
                | supdem.stan.mod5-202402171324.json
	    \---mar
                | supdem.stan.mod5-202402171324-1.csv"
                | supdem.stan.mod5-202402171324-2.csv
                | supdem.stan.mod5-202402171324-3.csv
                | supdem.stan.mod5-202402171324.json
	    \---theory
                | supdem.stan.mod5-202402171324-1.csv
                | supdem.stan.mod5-202402171324-2.csv
                | supdem.stan.mod5-202402171324-3.csv
                | supdem.stan.mod5-202402171324.json
	    \---upper
                | supdem.stan.mod5-202402171324-1.csv
                | supdem.stan.mod5-202402171324-2.csv                | supdem.stan.mod5-202402171324-3.csv
                | supdem.stan.mod5-202402171324.json
        \---lowest
            \---lower
                | supdem.stan.mod5-202402171404-1.csv
                | supdem.stan.mod5-202402171404-2.csv
                | supdem.stan.mod5-202402171404-3.csv
                | supdem.stan.mod5-202402171404.json
	    \---mar
                | supdem.stan.mod5-202402171404-1.csv
                | supdem.stan.mod5-202402171404-2.csv
                | supdem.stan.mod5-202402171404-3.csv
                | supdem.stan.mod5-202402171404.json
	    \---theory
                | supdem.stan.mod5-202402171404-1.csv
                | supdem.stan.mod5-202402171404-2.csv
                | supdem.stan.mod5-202402171404-3.csv
                | supdem.stan.mod5-202402171404.json
	    \---upper
                | supdem.stan.mod5-202402171404-1.csv
                | supdem.stan.mod5-202402171404-2.csv
                | supdem.stan.mod5-202402171404-3.csv
                | supdem.stan.mod5-202402171404.json
        \---median
            \---lower
                | supdem.stan.mod5-202402171343-1.csv
                | supdem.stan.mod5-202402171343-2.csv
                | supdem.stan.mod5-202402171343-3.csv
                | supdem.stan.mod5-202402171343.json
	    \---mar
                | supdem.stan.mod5-202402171343-1.csv
                | supdem.stan.mod5-202402171343-2.csv
                | supdem.stan.mod5-202402171343-3.csv
                | supdem.stan.mod5-202402171343.json
	    \---theory
                | supdem.stan.mod5-202402171343-1.csv
                | supdem.stan.mod5-202402171343-2.csv
                | supdem.stan.mod5-202402171343-3.csv
                | supdem.stan.mod5-202402171343.json
	    \---upper
                | supdem.stan.mod5-202402171343-1.csv
                | supdem.stan.mod5-202402171343-2.csv
                | supdem.stan.mod5-202402171343-3.csv
                | supdem.stan.mod5-202402171343.json
        \---orig
            \---lower
                | supdem.stan.mod5-202402171306-1.csv
                | supdem.stan.mod5-202402171306-2.csv
                | supdem.stan.mod5-202402171306-3.csv
                | supdem.stan.mod5-202402171306.json
	    \---mar
                | supdem.stan.mod5-202402171306-1.csv
                | supdem.stan.mod5-202402171306-2.csv
                | supdem.stan.mod5-202402171306-3.csv
                | supdem.stan.mod5-202402171306.json
	    \---theory
                | supdem.stan.mod5-202402171306-1.csv		| supdem.stan.mod5-202402171306-2.csv
                | supdem.stan.mod5-202402171306-3.csv 
                | supdem.stan.mod5-202402171306.json
	    \---upper
                | supdem.stan.mod5-202402171306-1.csv
                | supdem.stan.mod5-202402171306-2.csv
                | supdem.stan.mod5-202402171306-3.csv
                | supdem.stan.mod5-202402171306.json
    |       
    +---R
    |       supdem.stan.mod5.stan
    |       
    |       
    \---renv
        |   .gitignore
        |   activate.R
        |   settings.json
        |   
        \---library
            \*
        
Based on the above setting, one can render the file through the following command in R: 

```r
current_dir <- getwd()Sys.setenv(RENV_PROJECT = current_dir)
if(!require(renv)) install.packages("renv")
renv::load()

rmarkdown::render('dem_mood.Rmd',  encoding = 'UTF-8')
```

Notes: If replicators run into issues when compiling the file, try to update tinytex by using tinytex::tlmgr_update().


If replicators would like to recreate data, results, figures and tables step by step. They can either Knit dem_mood.Rmd or execute the codes chunk by chunk.


# Replicating the results in the manuscript and supplementary materials
 
`dem_mood.Rmd` requires the following files to produce results:


- Files to compile the PDF (saving them at the same directory as the rmd file)
    - dem_mood_text.bib
    - apsr.bst

- data/
    - claassen_input_raw.csv
    - claassen_input.rds
    - dem_mood_apsr.csv
    - df_apsr_list.rds
    - supdem raw survey marginals.csv


# Recreating source files and the 16 versions of measures

We have included code that allows for the automatic download of source files and measures via the `dataverse` or `osfr` package. However, obtaining an API key from the dataverse or OSF website is a prerequisite.

To ensure full transparency in our analysis, we also provide codes that enable the recreation of measures based on 16 different choices, located in the data/ directory. It's important to note that going through these steps isn't necessary for compiling the Rmd file or producing the figures and tables presented in the paper.

For readers interested in recreating the 16 measure versions stored in the data/highest, data/lowest, data/median, and data/orig folders, the file R/supdem.stan.mod5.stan is essential. Replication can be achieved by removing eval = FALSE. Please be advised that regenerating these 16 versions of measures is time-consuming.

	
Basic working environment:
- R version >= 4.3.2,
- `DCPOtools` 0.1.0.9000,
- `cmdstanr` 0.7.1,
- `tinytex` 0.45.