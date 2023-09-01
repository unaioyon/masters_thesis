# Import packages
library(tidyverse)
library(readxl)
library(janitor) # cleaning var names
library(textclean) # clean texts
library(sf) # geo
library(ggridges) # plotting
library(viridis) # plotting
library(hrbrthemes) # plotting
library(RColorBrewer) # color palette
library(stargazer)
library(plotrix) # compute standard errors of mean
library(did) # CALLAWAY & SANT'ANNA
library(fixest) # SUN & ABRAHAM
library(bacondecomp) # Goodman-Bacon decomposition
library(plm) # fixed effects
library(fastDummies) # generate dummies from categorical variables


# Set working directory
setwd("/Users/unaioyon/Desktop/masters_thesis/data/fra")

options(scipen = 999)

########################################
##### DV3F DATA ########################
########################################

dv3f <- read_csv("dv3f_new_variables/dv3f_modified.csv")

# Create two variables for age of construction of newest and oldest in each transaction
dv3f <- dv3f %>% 
  mutate(oldest = 2019 - ffancstmin,
         newest = 2019 - ffancstmax)



##### CREATE A SELECTION OF VARIABLES TO TAKE TO STARGAZER
# Create dummies for categorical variables
dv3f_stats <- dv3f %>% 
  dummy_cols(select_columns = c("anneemut", "libnatmut", "filtre", "libtypbien", "slow_zone_year", "arr"))

# Select & subset
# NA price (dropped), impute ffsbati using sbati as a lower bound
dv3f_stats <- dv3f_stats %>% 
  mutate(ffsbati = ifelse(is.na(ffsbati),
                          sbati,
                          ifelse(ffsbati == 0 & sbati != 0,
                                 sbati,
                                 ffsbati)))

dv3f_stats <- dv3f_stats %>% 
  filter(!(valeurfonc == 0 | is.na(valeurfonc) | ffsbati == 0)) %>% 
  mutate(price_m2 = valeurfonc/ffsbati,
         .after = valeurfonc)

# Create log price
dv3f_stats <- dv3f_stats %>% 
  mutate(price_m2 = valeurfonc/ffsbati,
         log_price_m2 = log(price_m2))

# Filter outliers
dv3f_stats <- dv3f_stats %>% 
  filter(dplyr::between(price_m2, min(price_m2), quantile(price_m2, 0.99)))

# Now, go for a stargazer table
dv3f_stats <- dv3f_stats %>% 
  select(-c("anneemut", "libnatmut", "filtre", "libtypbien", "slow_zone_year", "arr"))

dv3f_stats <- dv3f_stats %>% 
  select(price_m2, log_price_m2,
         ffsbati, sbatapt, sbatmai, sbatact,
         nblocmai, nblocapt, nblocdep, nblocact,
         nblocanc, nblocrecen, nblocneuf,
         starts_with("anneemut"), starts_with("libnatmut"),
         starts_with("libtypbien"),
         starts_with("arr"),
         starts_with("slow_zone_year"),
         slow_zone_d)


stargazer(as.data.frame(dv3f_stats), summary = TRUE, header = FALSE,
          digits = 2, summary.stat = c("min", "median", "mean", "max", "sd"))


######## NOW JUST APARTMENTS AND HOUSES
dv3f_sel <- dv3f %>%
  dummy_cols(select_columns = c("anneemut", "libnatmut", "filtre", "libtypbien", "slow_zone_year", "arr"))

# Select apartments and houses with the aid of variable "codtypbien"

dv3f_sel <- dv3f_sel %>% 
  filter(str_sub(as.character(codtypbien), 1, 2) == "11" | str_sub(as.character(codtypbien), 1, 2) == "12")


# 94 with NA price (dropped), impute ffsbati using sbati as a lower bound
dv3f_sel <- dv3f_sel %>% 
  mutate(ffsbati = ifelse(is.na(ffsbati),
                          sbati,
                          ifelse(ffsbati == 0 & sbati != 0,
                                 sbati,
                                 ffsbati)))
# Overall, drop 427 observations + 8 of ffsbati == 0, 435 (as there is no overlap b/n the two var. conditions)
# also create the normalized price by ffsbati
dv3f_sel <- dv3f_sel %>% 
  filter(!(valeurfonc == 0 | is.na(valeurfonc) | ffsbati == 0)) %>% 
  mutate(price_m2 = valeurfonc/ffsbati,
         .after = valeurfonc)

dv3f_sel <- dv3f_sel %>% 
  filter(dplyr::between(price_m2, min(price_m2), quantile(price_m2, 0.99)))


# Now, go for a stargazer table
dv3f_sel <- dv3f_sel %>% 
  select(-c("anneemut", "libnatmut", "filtre", "libtypbien", "slow_zone_year", "arr"))

dv3f_sel <- dv3f_sel %>% 
  mutate(log_price_m2 = log(price_m2))

dv3f_sel <- dv3f_sel %>% 
  select(price_m2, log_price_m2,
         ffsbati, sbatapt, sbatmai, sbatact,
         nblocmai, nblocapt, nblocdep, nblocact,
         nblocanc, nblocrecen, nblocneuf,
         starts_with("anneemut"), starts_with("libnatmut"),
         starts_with("libtypbien"),
         starts_with("arr"),
         starts_with("slow_zone_year"),
         slow_zone_d)

stargazer(as.data.frame(dv3f_sel), summary = TRUE, header = FALSE,
          digits = 2, summary.stat = c("min", "median", "mean", "max", "sd"))



############################################
##### ACCIDENT DATA ########################
############################################

acc_stats <- st_read("accidents/final/accidents_final_geo.geojson") %>% 
  st_drop_geometry() %>% 
  as_tibble()


# Check missing values
lapply(acc_stats, function(x) sum(is.na(x)))

# Accidents
acc_stats <- acc_stats %>% 
  dummy_cols(select_columns = c("an","lum", "agg", "int", "atm", "col",
                                "catr", "circ", "vosp", "surf", "infra", "situ",
                                "catv"))

stargazer(as.data.frame(acc_stats), summary = TRUE, header = FALSE,
          digits = 2, summary.stat = c("min", "median", "mean", "max", "sd"))
