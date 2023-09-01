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

dv3f <- st_read("dv3f_new_variables/dv3f_modified.geojson") %>% 
  select(idmutation, geometry)

st_write(dv3f, "dv3f_new_variables/dv3f_geo_light.geojson")


acc <- st_read("accidents/final/accidents_final_geo.geojson") %>% 
  select(Num_Acc, geometry)

st_write(acc, "")