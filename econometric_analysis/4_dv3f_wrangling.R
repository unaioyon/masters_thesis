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

# Set working directory
setwd("/Users/unaioyon/Desktop/masters_thesis/data/fra")

options(scipen = 999)


# Import the dv3f data without the geometry
dv3f_all <- read_csv("dv3f_new_variables/dv3f_modified.csv")

#######################################
##### 1. PRELIMINARY EXPLORATION ######
#######################################

# Now, explore the variables and the data
colnames(dv3f_all)
view(dv3f_all)
view(dv3f_all[, c(50:ncol(dv3f_all))])

# Select apartments and houses with the aid of variable "codtypbien"

dv3f <- dv3f_all %>% 
  filter(str_sub(as.character(codtypbien), 1, 2) == "11" | str_sub(as.character(codtypbien), 1, 2) == "12") 

# Missing values
dv3f_na <- t(as_tibble(lapply(dv3f, function(x) sum(is.na(x)))))

# Select variables
dv3f <- dv3f %>% 
  select(c(1, 7, 8, 9, 10, 11, 13, 14, 23, 24, 26, 27, 30, 31, 32, 25, 26, 61, 62, 58, 59, 60,
         43, 44, 45, 47, 48, 49, 50, c(51:57),
         17, 18))

rm(dv3f_all)

# Missing values after variable selection
dv3f_na_new <- t(as_tibble(lapply(dv3f, function(x) sum(is.na(x)))))

# See how different the two definitions of surface are (as "sterr" is filled with 0s)
view(dv3f[, c("sbati", "ffsbati", "valeurfonc", "filtre")])

# 94 with NA price (dropped), impute ffsbati using sbati as a lower bound
dv3f <- dv3f %>% 
  mutate(ffsbati = ifelse(is.na(ffsbati),
                          sbati,
                          ifelse(ffsbati == 0 & sbati != 0,
                                 sbati,
                                 ffsbati)))

# See the result
view(dv3f[, c("sbati", "ffsbati", "valeurfonc", "filtre")])

# Number of zeroes and NAs now:
sum(is.na(dv3f$ffsbati)) # 0
sum(dv3f$ffsbati == 0) # 8 

# Overall, the data is very clean now. Let's go for outliers

# Price = 0 should be removed:
sum(dv3f$valeurfonc == 0, na.rm = TRUE) # 333 observations have price = 0
sum(is.na(dv3f$valeurfonc)) # 94 missing values

# Overall, drop 427 observations + 8 of ffsbati == 0, 435 (as there is no overlap b/n the two var. conditions)
# also create the normalized price by ffsbati
dv3f <- dv3f %>% 
  filter(!(valeurfonc == 0 | is.na(valeurfonc) | ffsbati == 0)) %>% 
  mutate(price_m2 = valeurfonc/ffsbati,
         .after = valeurfonc)

336041 - 435 == dim(dv3f)[1] # yep


# Now, explore the possibility of removing outliers as a function of price
summary(dv3f$valeurfonc) # mean 500,897 & median 345,000

dv3f_out2 <- dv3f %>% 
  filter(between(valeurfonc, min(valeurfonc), quantile(valeurfonc, 0.99)))


dv3f_out <- dv3f %>% 
  filter(dplyr::between(price_m2, min(price_m2), quantile(price_m2, 0.99)))

summary(dv3f_out$valeurfonc) # mean 500,897 & median 345,000
summary(dv3f_out$price_m2)
summary(dv3f_out2$price_m2)
summary(dv3f$price_m2)

# FILTERING USING NORMALIZED PRICES LEADS TO A MUCH BETTER DISTIRBUTION OF NORMALIZED PRICES,
# AS APPARENTLY, SOME OF THE VERY HIGH NON-NORMALIZED PRICES CORRESPOND TO LOW SURFACE BUILDINGS.

#################################################################################
##### 3. TREATMENT STATUS for the did PACKAGE (Callaway & Sant'Anna, 2021) ######
#################################################################################

# First, define the groups' (E_i) coding, with special attention on
# how to define never-treated observations (E_i == 0 for obs. not in slow zones and in the 2007 and 2008 ones).

dv3f <- dv3f %>% 
  mutate(e_i = case_when(is.na(slow_zone_year) |  slow_zone_year == 2007 | slow_zone_year == 2008 ~ 0,
                         slow_zone_year == 2010 ~ 1,
                         slow_zone_year == 2011 ~ 2,
                         slow_zone_year == 2012 ~ 3,
                         slow_zone_year == 2013 ~ 4,
                         slow_zone_year == 2014 ~ 5,
                         slow_zone_year == 2015 ~ 6,
                         slow_zone_year == 2016 ~ 7,
                         slow_zone_year == 2017 ~ 8,
                         slow_zone_year == 2018 ~ 9,
                         slow_zone_year == 2019 ~ 10))

dv3f_out <- dv3f_out %>% 
  mutate(e_i = case_when(is.na(slow_zone_year) |  slow_zone_year == 2007 | slow_zone_year == 2008 ~ 0,
                         slow_zone_year == 2010 ~ 1,
                         slow_zone_year == 2011 ~ 2,
                         slow_zone_year == 2012 ~ 3,
                         slow_zone_year == 2013 ~ 4,
                         slow_zone_year == 2014 ~ 5,
                         slow_zone_year == 2015 ~ 6,
                         slow_zone_year == 2016 ~ 7,
                         slow_zone_year == 2017 ~ 8,
                         slow_zone_year == 2018 ~ 9,
                         slow_zone_year == 2019 ~ 10))

table(dv3f$slow_zone_year)
sum(is.na(dv3f$slow_zone_year))


# Save it:
write_csv(dv3f_out, "dv3f_new_variables/dv3f_clean_no_outliers.csv")


##### Aggregate at the IRIS-year level to construct a balanced (?) panel that will make it easier
# to use the Sun & Abraham approach and will make Callaway & Sant'Anna easier to interpret
dv3f_g <- dv3f %>% 
  group_by(iris_code, anneemut) %>% 
  summarise(n = n(),
            avg_price = mean(price_m2)) %>% 
  arrange(iris_code, anneemut) %>% 
  mutate(id = paste(as.character(iris_code),
                    as.character(anneemut),
                    sep = "."))

# Check if it generates a balanced panel (nope!!!!)
length(unique(dv3f_out$iris_code)) * 11 != 9792 # so there are some missing values (exactly 218).

# Generate all possible combinations
iris_years <- as_tibble(apply(expand.grid(unique(dv3f_out$iris_code), as.character(c(2010:2020))),
                    1,
                    paste,
                    collapse=".")) %>% 
  rename(id = value) %>% 
  arrange(id)

# 218 combinations of IRIS + year (2.16% of the total combinations)
iris_years_na <- iris_years[!(iris_years$id %in% unique(dv3f_g$id)), "id"]

# How many IRIS? 63 IRIS, i.e. 6.35%
iris_years_na %>% 
  mutate(iris_na = str_sub(id, 1, 9)) %>% 
  select(iris_na) %>% 
  unique() %>% 
  nrow()

a <- c("a", "b")
b <- c("c", "d")
c <- apply(expand.grid(a, b), 1, paste, collapse=".")
c
#############################################################################
##### 4. TREATMENT STATUS for the fixest PACKAGE (Sun & Abraham, 2021) ######
#############################################################################
