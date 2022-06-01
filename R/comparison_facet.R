
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

# 
# osf_retrieve_file("uvn4g") %>%
#   osf_download(here::here("data"), conflict = "skip")  ##corrected

df_claassen <- rio::import(here("data", "dem_mood_apsr.rda"))

df_corrected <- rio::import(here("data", "correct_cls_apsr.rda")) %>%
  rename(Country = country, Year = year)


# Wrong w. right

df_claassenAsia <- df_claassen %>% 
  mutate(continent = countrycode::countrycode(sourcevar = Country, 
                                              origin = "country.name",
                                              destination = "continent"),
         data = "Original") %>% 
  filter(continent == "Asia", # Choosing Asian countries
         Year > 1987,
         Year >= First_yr) %>% # Claassen's filter
  select(Country, Year,  SupDem_trim, SupDem_l90, SupDem_u90, data)

df_correctedAsia <- df_corrected %>% 
  mutate(continent = countrycode::countrycode(sourcevar = Country, 
                                              origin = "country.name",
                                              destination = "continent"),
         data = "Corrected") %>%
  filter(continent == "Asia", # Choosing Asian countries
         Year > 1987,
         Year >= First_yr) %>% # Claassen's filter
  select(Country, Year,  SupDem_trim, data)


# df_claassenDist <- rio::import(here("data", "cls_apsr.rda")) %>%
#   map(~{
#     rename(., Country = country, Year = year) %>%
#       mutate(continent = countrycode::countrycode(sourcevar = Country,
#                                                   origin = "country.name",
#                                                   destination = "continent")) %>%
#       filter(continent == "Asia", # Choosing Asian countries
#              Year > 1987,
#              Year >= First_yr) %>% # Claassen's filter
#       select(Country, Year,  SupDem_trim)
#   })
# 
# saveRDS(df_claassenDist, "output/df_claassenDist.rds")
# 
# df_correctedDist <- rio::import(here("data", "correct_cls_apsrDistr.rda")) %>%
#   map(~{
#     rename(., Country = country, Year = year) %>%
#     mutate(continent = countrycode::countrycode(sourcevar = Country,
#                                               origin = "country.name",
#                                               destination = "continent")) %>%
#   filter(continent == "Asia", # Choosing Asian countries
#          Year > 1987,
#          Year >= firstyear) %>% # Claassen's filter
#   select(Country, Year,  SupDem_trim)
#   })
# 
# saveRDS(df_correctedDist, "output/df_correctedDist.rds")


df_correctedDist <- readRDS("output/df_correctedDist.rds") %>% 
  bind_rows() %>% 
  group_by(Country, Year) %>% 
  summarise(SupDem_l90 = quantile(SupDem_trim, .10), 
            SupDem_u90 = quantile(SupDem_trim, .90))

df_correctedAsia <- left_join(df_correctedAsia, df_correctedDist) %>% 
  select(Country, Year,  SupDem_trim, SupDem_l90, SupDem_u90, data)


df_asianCompare <- bind_rows(df_claassenAsia, df_correctedAsia)

## Plotting

ggplot(df_asianCompare,
       aes(
         x = Year,
         y = SupDem_trim,
         fill = data
       )) +
  geom_line() +
  geom_ribbon(aes(ymin = SupDem_l90,
                  ymax = SupDem_u90),
              alpha = 0.5) +
  facet_wrap(~ Country)

ggsave("output/compare_ci90.png")

