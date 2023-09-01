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
library(bacondecomp)
library(plm) # fixed effects


# memory problems
library(usethis) 
usethis::edit_r_environ()

# Set working directory
setwd("/Users/unaioyon/Desktop/masters_thesis/data/fra")

options(scipen = 999)



#####################################
##### 1. DV3F DATA: formatting ######
#####################################
# Import DV3F normal data clean of outliers and just with apartments and houses
dv3f <- read_csv("dv3f_new_variables/dv3f_clean_no_outliers.csv")

# Modify the format and select the relevant variables
dv3f_sd_did <- dv3f %>% 
  mutate(ffancstmin_dif = 2019 - ffancstmin,
         ffancstmax_dif = 2019 - ffancstmax,
         arr = as.character(arr),
         iris_fe = as.character(iris_code),
         treatment_twfe = ifelse(slow_zone_d == 0,
                            0,
                            ifelse(slow_zone_d == 1 & is.na(slow_zone_year),
                                   NA,
                                   ifelse(anneemut >= slow_zone_year,
                                          1,
                                          0))),
         anneemut = as.character(anneemut)) %>%
  mutate(log_price_m2 = log(price_m2),
         .after = price_m2) %>% 
  select(log_price_m2, price_m2,  treatment_twfe, libnatmut, nblocmai, nblocapt,
         ffancstmin_dif, ffancstmax_dif, libtypbien, arr, anneemut,
         iris_fe, slow_zone_d, slow_zone_year)

rm(dv3f)

# missing year of implementation of slow zone needs to be dropped
sum(is.na(dv3f_sd_did[dv3f_sd_did$slow_zone_d == 1,]$slow_zone_year))


dv3f_sd_did <- dv3f_sd_did %>% 
  filter(!(slow_zone_d == 1 & is.na(slow_zone_year))) # well-subsetted

# Also, slow zones implemented in 2007 and 2008 need to be changed to NA
dv3f_sd_did <- dv3f_sd_did %>% 
  mutate(slow_zone_year = case_when(slow_zone_year == 2007 | slow_zone_year == 2008 ~ NA,
                                    slow_zone_year == slow_zone_year ~ slow_zone_year))


# TIME WINDOW [-2, +2]
for (i in unique(dv3f_sd_did$slow_zone_year)[2:length(unique(dv3f_sd_did$slow_zone_year))]) {
  assign(sprintf("stacked_%s",
                 str_sub(as.character(i),
                         3,
                         4)),
         dv3f_sd_did %>% 
           filter((slow_zone_year == i | slow_zone_year > i + 2 | is.na(slow_zone_year))
                  & anneemut %in% c((i-2):(i+2))) %>% 
           mutate(wave = i,
                  post = ifelse(anneemut >= wave,
                                1,
                                0),
                  zone = ifelse(slow_zone_year == wave & !is.na(slow_zone_year),
                                1,
                                ifelse(is.na(slow_zone_year),
                                       0,
                                       0)),
                  treated = ifelse(slow_zone_year == wave & anneemut >= wave & !is.na(slow_zone_year),
                                   1,
                                   ifelse(is.na(slow_zone_year),
                                          0,
                                          0))))
}

## Merge all in a single file and make some variables categorical so that dummies can be created
stacked <- rbind(stacked_10, stacked_13, stacked_14, stacked_15,
                 stacked_16, stacked_17, stacked_18, stacked_19) %>%  # literally equivalent to "treated" lol
  mutate(arr = as.character(arr))


# Drop the individual datasets to free up some memory
rm(list = c("stacked_10","stacked_13","stacked_14","stacked_15",
            "stacked_16","stacked_17", "stacked_18","stacked_19"))


######### adding fixed effects
stacked <- stacked %>% 
  mutate(wave_fe = as.character(wave))


# Use a loop for n.3:
for (i in unique(stacked$anneemut)[order(unique(stacked$anneemut))]) {
  for (j in unique(stacked$wave)[order(unique(stacked$wave))]) {
    
    assign(sprintf("fe_an_%s", paste0(as.character(i),
                                   as.character(j))),
           ifelse(stacked$anneemut == i & stacked$wave == j,
                  1,
                  0))
    stacked <- stacked %>% 
      cbind(as_tibble(get(sprintf("fe_an_%s", paste0(as.character(i),
                                                  as.character(j))))))
    
    rm(list = ls()[2])
    
  }
}


# Before moving on to define the FE for iris-wave, I have to rename variables
year_wave_combinations <- apply(expand.grid(unique(stacked$anneemut)[order(unique(stacked$anneemut))],
                                            unique(stacked$wave)[order(unique(stacked$wave))]),
                                1,
                                paste,
                                collapse="") # yep, 88 combinations

# Re-order to make consistent with the underlying variables generated in the loop
# and add "fe_" at the beginning for tractability
year_wave_combinations <- year_wave_combinations[order(year_wave_combinations)]
year_wave_combinations <- paste("fe_an",
                                year_wave_combinations,
                                sep = "_")

colnames(stacked)[20:ncol(stacked)] <- year_wave_combinations

rm(list = c("i", "j", "year_wave_combinations"))


# Use a loop for group-by-wave FE:
for (i in unique(stacked$slow_zone_year)[order(unique(stacked$slow_zone_year))]) {
  for (j in unique(stacked$wave)[order(unique(stacked$wave))]) {
    
    assign(sprintf("fe_%s", paste0(as.character(i),
                                   as.character(j))),
           ifelse(stacked$slow_zone_year == i & stacked$wave == j,
                  1,
                  0))
    stacked <- stacked %>% 
      cbind(as_tibble(get(sprintf("fe_%s", paste0(as.character(i),
                                                  as.character(j))))))
    
    rm(list = ls()[2])
    
  }
}

# Before moving on to define the FE for iris-wave, I have to rename variables
group_wave_combinations <- apply(expand.grid(unique(stacked$slow_zone_year)[order(unique(stacked$slow_zone_year))],
                                             unique(stacked$wave)[order(unique(stacked$wave))]),
                                 1,
                                 paste,
                                 collapse="") # yep, 88 combinations

# Re-order to make consistent with the underlying variables generated in the loop
# and add "fe_" at the beginning for tractability
group_wave_combinations <- group_wave_combinations[order(group_wave_combinations)]
group_wave_combinations <- paste("fe",
                                 group_wave_combinations,
                                 sep = "_")

colnames(stacked)[108:length(colnames(stacked))] <- group_wave_combinations

rm(list = c("i", "j", "group_wave_combinations"))

# Use a loop for arrondissement-by-year FE:
for (i in unique(stacked$arr)[order(unique(stacked$arr))]){
  for (j in unique(stacked$anneemut)[order(unique(stacked$anneemut))]) {
    
    assign(sprintf("fe_arr_%s", paste0(as.character(i),
                                       as.character(j))),
           ifelse(stacked$arr == i & stacked$anneemut == j,
                  1,
                  0))
    stacked <- stacked %>% 
      cbind(as_tibble(get(sprintf("fe_arr_%s", paste0(as.character(i),
                                                      as.character(j))))))
    
    rm(list = ls()[2])
    
  }
}

# Before moving on to define the FE for iris-wave, I have to rename variables
arr_wave_combinations <- apply(expand.grid(unique(stacked$arr)[order(unique(stacked$arr))],
                                           unique(stacked$anneemut)[order(unique(stacked$anneemut))]),
                               1,
                               paste,
                               collapse="") # yep, 88 combinations

# Re-order to make consistent with the underlying variables generated in the loop
# and add "fe_" at the beginning for tractability
arr_wave_combinations <- arr_wave_combinations[order(arr_wave_combinations)]
arr_wave_combinations <- paste("fe_arr",
                               arr_wave_combinations,
                               sep = "_")

colnames(stacked)[180:length(colnames(stacked))] <- arr_wave_combinations

rm(list = c("i", "j", "arr_wave_combinations"))



# Now, sum all the variables' and put into a tibble to select those with a sum of 0
stacked_sum <- lapply(stacked[,20:ncol(stacked)], function(x) sum(x, na.rm = TRUE))

# Set the format
stacked_sum1 <- stacked_sum %>%
  t() %>% 
  as_tibble()

# Extract so that I can get the non-zero summing variables and drop the rest of FE 
stacked_sum2 <- data.frame(variables = colnames(stacked_sum1),
                           sum = stacked_sum1 %>%
                             slice(1) %>% 
                             unlist(., use.names=FALSE))

stacked_sum2 <- stacked_sum2 %>% 
  filter(sum != 0)

stacked1 <- stacked %>% 
  select(unlist(unique(stacked_sum2$variables)))

stacked2 <- stacked[1:19]

rm(stacked_sum2)

stacked_clean <- cbind(stacked2, stacked1)

stacked_clean <- stacked_clean %>% 
  select(-wave)

write_csv(stacked_clean, "dv3f_new_variables/stacked/dv3f_stacked_2_2_window.csv")





#####################################
##### OLD VERSION:::::::::::::: 1. DV3F DATA: formatting ######
#####################################

# Taking the object "dv3f_out" declared in the file "4_dv3f_wrangling",
dv3f_out <- read_csv("dv3f_new_variables/dv3f_clean_no_outliers.csv")

# missing year of implementation of slow zone needs to be dropped
sum(is.na(dv3f_out[dv3f_out$slow_zone_d == 1,]$slow_zone_year))

dv3f_out <- dv3f_out %>% 
  filter(!(slow_zone_d == 1 & is.na(slow_zone_year))) # well-subsetted

# Also, slow zones implemented in 2007 and 2008 need to be changed to NA
dv3f_out <- dv3f_out %>% 
  mutate(slow_zone_year = case_when(slow_zone_year == 2007 | slow_zone_year == 2008 ~ NA,
                                    slow_zone_year == slow_zone_year ~ slow_zone_year))



# Create a loop selecting, for each implementation wave, the obs. in that group, the valid controls
# (i.e., those treated at least 2 years later AND never-treated) and the relevant observations.

# Also, I need to create the relevant treatment variables: 
# 1. post (i.e. anneemut >= wave)
# 2. in relevant zone (i.e. slow_zone_year == wave)
# 3. 

# NEW VERSION: FOLLOW BREHM ET AL. (2021) AND TAKE ALL THE RELEVANT OBS WHEN POSSIBLE
for (i in unique(dv3f_out$slow_zone_year)[2:length(unique(dv3f_out$slow_zone_year))]) {
  assign(sprintf("stacked_%s",
                 str_sub(as.character(i),
                         3,
                         4)),
         dv3f_out %>% 
           filter(slow_zone_year == i | is.na(slow_zone_year) | # just includes never-treated
                  (slow_zone_year > i & anneemut < slow_zone_year)) %>% 
           mutate(wave = i,
                  post = ifelse(anneemut >= wave,
                                1,
                                0),
                  zone = ifelse(slow_zone_year == wave & !is.na(slow_zone_year),
                                1,
                                ifelse(is.na(slow_zone_year),
                                       0,
                                       0)),
                  treated = ifelse(slow_zone_year == wave & anneemut >= wave & !is.na(slow_zone_year),
                                   1,
                                   ifelse(is.na(slow_zone_year),
                                          0,
                                          0))))
}













# OLD VERSION WITH TIME WINDOW
for (i in unique(dv3f_out$slow_zone_year)[2:length(unique(dv3f_out$slow_zone_year))]) {
  assign(sprintf("stacked_%s",
                 str_sub(as.character(i),
                         3,
                         4)),
         dv3f_out %>% 
           filter((slow_zone_year == i | slow_zone_year > i + 1 | is.na(slow_zone_year))
                  & anneemut %in% c((i-1):(i+1))) %>% 
           mutate(wave = i,
                  post = ifelse(anneemut >= wave,
                                1,
                                0),
                  zone = ifelse(slow_zone_year == wave & !is.na(slow_zone_year),
                                1,
                                ifelse(is.na(slow_zone_year),
                                       0,
                                       0)),
                  treated = ifelse(slow_zone_year == wave & anneemut >= wave & !is.na(slow_zone_year),
                                   1,
                                   ifelse(is.na(slow_zone_year),
                                          0,
                                          0))))
}



## Merge all in a single file and make some variables categorical so that dummies can be created
stacked <- rbind(stacked_10, stacked_13, stacked_14, stacked_15,
                 stacked_16, stacked_17, stacked_18, stacked_19) %>% 
  mutate(effective_treatment = treated*post) %>%  # literally equivalent to "treated" lol
  mutate(arr = as.character(arr),
         moismut = as.character(moismut))
  
         
# Drop the individual datasets to free up some memory
rm(list = c("stacked_10","stacked_13","stacked_14","stacked_15",
            "stacked_16","stacked_17", "stacked_18","stacked_19"))

# Now, add variables to act as fixed effects (following Gruhl et al., 2021)
# 1. Wave FE (wave).
# 2. IRIS * wave FE (zone) which equals n_g,j 
# the way I see it in Gruhl et al., 2021., it is the same as LEZ_i,g,j.,
# but specific to each implementation wave
# 3. year*implementation wave FE (weirdgreekletter_t,j).
# 4. IRIS FE.

# 1 and 4
stacked <- stacked %>% 
  mutate(wave_fe = as.character(wave),
         iris_fe = as.character(iris_code))


# Use a loop for n.2:
for (i in unique(stacked$iris_code)[order(unique(stacked$iris_code))]) {
  for (j in unique(stacked$wave)[order(unique(stacked$wave))]) {
    
    assign(sprintf("fe_%s", paste0(as.character(i),
                  as.character(j))),
           ifelse(stacked$iris_code == i & stacked$wave == j & stacked$zone == 1,
                  1,
                  0))
    stacked <- stacked %>% 
      cbind(as_tibble(get(sprintf("fe_%s", paste0(as.character(i),
                                        as.character(j))))))
    
    rm(list = ls()[2])
    
  }
}

# Take this option just in case
# stacked1 <- stacked[,1:46]


# Before moving on to define the FE for year-wave, I have to rename variables
iris_wave_combinations <- apply(expand.grid(unique(stacked$iris_code)[order(unique(stacked$iris_code))],
                                            unique(stacked$wave)[order(unique(stacked$wave))]),
                                1,
                                paste,
                                collapse="") # yep, 6824 combinations

# Re-order to make consistent with the underlying variables generated in the loop
# and add "fe_" at the beginning for tractability
iris_wave_combinations <- iris_wave_combinations[order(iris_wave_combinations)]
iris_wave_combinations <- paste("fe",
                                iris_wave_combinations,
                                sep = "_")

colnames(stacked)[47:length(colnames(stacked))] <- iris_wave_combinations

rm(list = c("i", "j", "iris_wave_combinations"))

# Now, sum all the variables' and put into a tibble to select those with a sum of 0
stacked_sum <- lapply(stacked %>% 
                                  select_if(is.numeric), function(x) sum(x, na.rm = TRUE))

# Set the format
stacked_sum1 <- stacked_sum %>%
  t() %>% 
  as_tibble()
 
# Extract so that I can get the non-zero summing variables and drop the rest of FE 
stacked_sum2 <- data.frame(variables = colnames(stacked_sum1),
                           sum = stacked_sum1 %>%
                             slice(1) %>% 
                             unlist(., use.names=FALSE))

# Most of the iris_wave FE are dropped, and nbvolmut as well (apparently it has just zeroes)
stacked1 <- stacked %>% 
  select(unlist(unique(stacked_sum2$variables)))

stacked2 <- stacked %>% 
  select_if(is.character)

stacked_clean <- cbind(stacked1, stacked2)

# Save until now to avoid memory crashes 
write_csv(stacked_clean, "dv3f_new_variables/stacked/stacked_step1.csv")


# Manual check to see if the variables are created the right way
# Take transactions inside the first three IRIS
stacked_check2 <- stacked %>% 
  filter(iris_code == 751062301) %>% 
  select(idmutation, iris_code, fe_7510623012010, wave, fe_7510623012013) #checked

###### ----- this was not used but leave it ------ ####
stacked_check1 <- stacked %>% 
  filter(`751010101` == 1)

iris13 <- stacked[stacked$iris_code == 751010101 & stacked$wave == 2013, "idmutation"]
iris14 <- stacked[stacked$iris_code == 751010101 & stacked$wave == 2014, "idmutation"]
iris15 <- stacked[stacked$iris_code == 751010101 & stacked$wave == 2015, "idmutation"]

cbind(iris13, iris14, iris15)



####### Memory explodes if done consecutively, so as I have them saved,
# I just dropped the previous iris_wave FE.

stacked <- stacked[1:46]

# Use a loop for event-time-by-wave FE:
for (i in unique(stacked$anneemut)[order(unique(stacked$anneemut))]) {
  for (j in unique(stacked$wave)[order(unique(stacked$wave))]) {
    
    assign(sprintf("fe_%s", paste0(as.character(i),
                                   as.character(j))),
           ifelse(stacked$anneemut == i & stacked$wave == j,
                  1,
                  0))
    stacked <- stacked %>% 
      cbind(as_tibble(get(sprintf("fe_%s", paste0(as.character(i),
                                                  as.character(j))))))
    
    rm(list = ls()[2])
    
  }
}


# Before moving on to define the FE for iris-wave, I have to rename variables
year_wave_combinations <- apply(expand.grid(unique(stacked$anneemut)[order(unique(stacked$anneemut))],
                                            unique(stacked$wave)[order(unique(stacked$wave))]),
                                1,
                                paste,
                                collapse="") # yep, 88 combinations

# Re-order to make consistent with the underlying variables generated in the loop
# and add "fe_" at the beginning for tractability
year_wave_combinations <- year_wave_combinations[order(year_wave_combinations)]
year_wave_combinations <- paste("fe",
                                year_wave_combinations,
                                sep = "_")

colnames(stacked)[47:length(colnames(stacked))] <- year_wave_combinations

rm(list = c("i", "j", "year_wave_combinations"))

# Use a loop for group-by-wave FE:
for (i in unique(stacked$e_i)[order(unique(stacked$e_i))]) {
  for (j in unique(stacked$wave)[order(unique(stacked$wave))]) {
    
    assign(sprintf("fe_%s", paste0(as.character(i),
                                   as.character(j))),
           ifelse(stacked$e_i == i & stacked$wave == j,
                  1,
                  0))
    stacked <- stacked %>% 
      cbind(as_tibble(get(sprintf("fe_%s", paste0(as.character(i),
                                                  as.character(j))))))
    
    rm(list = ls()[2])
    
  }
}

# Before moving on to define the FE for iris-wave, I have to rename variables
group_wave_combinations <- apply(expand.grid(unique(stacked$e_i)[order(unique(stacked$e_i))],
                                            unique(stacked$wave)[order(unique(stacked$wave))]),
                                1,
                                paste,
                                collapse="") # yep, 88 combinations

# Re-order to make consistent with the underlying variables generated in the loop
# and add "fe_" at the beginning for tractability
group_wave_combinations <- group_wave_combinations[order(group_wave_combinations)]
group_wave_combinations <- paste("fe",
                                group_wave_combinations,
                                sep = "_")

colnames(stacked)[135:length(colnames(stacked))] <- group_wave_combinations

rm(list = c("i", "j", "group_wave_combinations"))



# Use a loop for arrondissement-by-year FE:
for (i in unique(stacked$arr)[order(unique(stacked$arr))]) {
  for (j in unique(stacked$anneemut)[order(unique(stacked$anneemut))]) {
    
    assign(sprintf("fe_arr_%s", paste0(as.character(i),
                                   as.character(j))),
           ifelse(stacked$arr == i & stacked$anneemut == j,
                  1,
                  0))
    stacked <- stacked %>% 
      cbind(as_tibble(get(sprintf("fe_arr_%s", paste0(as.character(i),
                                                  as.character(j))))))
    
    rm(list = ls()[2])
    
  }
}

# Before moving on to define the FE for iris-wave, I have to rename variables
arr_wave_combinations <- apply(expand.grid(unique(stacked$arr)[order(unique(stacked$arr))],
                                             unique(stacked$anneemut)[order(unique(stacked$anneemut))]),
                                 1,
                                 paste,
                                 collapse="") # yep, 88 combinations

# Re-order to make consistent with the underlying variables generated in the loop
# and add "fe_" at the beginning for tractability
arr_wave_combinations <- arr_wave_combinations[order(arr_wave_combinations)]
arr_wave_combinations <- paste("fe_arr",
                                 arr_wave_combinations,
                                 sep = "_")

colnames(stacked)[207:length(colnames(stacked))] <- arr_wave_combinations

rm(list = c("i", "j", "arr_wave_combinations"))


# Now, sum all the variables' and put into a tibble to select those with a sum of 0
stacked_sum <- lapply(stacked %>% 
                        select_if(is.numeric), function(x) sum(x, na.rm = TRUE))

# Set the format
stacked_sum1 <- stacked_sum %>%
  t() %>% 
  as_tibble()

# Extract so that I can get the non-zero summing variables and drop the rest of FE 
stacked_sum2 <- data.frame(variables = colnames(stacked_sum1),
                           sum = stacked_sum1 %>%
                             slice(1) %>% 
                             unlist(., use.names=FALSE))

# Most of the iris_wave FE are dropped, and nbvolmut as well (apparently it has just zeroes)
stacked1 <- stacked %>% 
  select(unlist(unique(stacked_sum2$variables)))

stacked2 <- stacked %>% 
  select_if(is.character)

stacked_clean <- cbind(stacked1, stacked2)

# Save until now to avoid memory crashes
write_csv(stacked, "dv3f_new_variables/stacked/stacked_full_period.csv")




# Now, run a pooled OLS selecting the outcome, the treatment and the FEs
formula_stacked <- as.formula(paste("price_m2 ~ ", paste(c("treated",
                                                         "wave_fe",
                                                         "iris_fe",
                                                         "libtypbien",
                                                         "arr"),
                                                         collapse = "+")))


ols_stacked <- lm(formula_stacked, data = stacked)
summary(ols_stacked)













##########################################
##### 2. Accidents DATA: formatting ######
##########################################

# Import data without geospatial info
acc <- st_read("accidents/final/accidents_final_geo.geojson") %>% 
  st_drop_geometry() %>% 
  as_tibble()

# missing year of implementation of slow zone needs to be dropped
sum(is.na(acc[acc$slow_zone_d == 1,]$slow_zone_year)) # 4456 obs., but some could be inside
# the zone but before it's implemented, so we'd keep them as controls.

acc <- acc %>% 
  filter(!(slow_zone_d == 1 & is.na(slow_zone_year) & as.numeric(slow_zone_year) <= an)) # well-subsetted

# Also, slow zones implemented in 2007 and 2008 need to be changed to NA
acc <- acc %>% 
  mutate(slow_zone_year = case_when(slow_zone_year == 2007 | slow_zone_year == 2008 ~ NA,
                                    slow_zone_year == slow_zone_year ~ slow_zone_year))

# Visualize the number of observations in each slow zone year implementation date (which corresponds to the)
# total period. I thus drop the ones in the 2011 implementation date because we have too little variation
acc <- acc %>% 
  filter(!(slow_zone_year == "2011") | is.na(slow_zone_year))

table(acc$slow_zone_year)

###### For accidents, it makes more sense to group at the IRIS level

acc_g <- acc %>% 
  group_by(iris_code, an) %>% 
  summarise(n_accidents = n(),
            share_pedestrian = mean(has_pietons, na.rm = TRUE),
            share_bike = mean(has_velo, na.rm = TRUE),
            n_dead = sum(tue_nb, na.rm = TRUE),
            n_hospital = sum(hospitalise_nb, na.rm = TRUE),
            total_victims = sum(tue_nb + indemne_nb + hospitalise_nb + blesseleger_nb)) %>% 
  mutate(iris_year = paste0(as.character(iris_code),
                            as.character(an)),
         share_dead = n_dead/total_victims,
         share_hospital = n_hospital/total_victims) %>% 
  arrange(iris_year)

summary(acc_g)

# Import data with the classification of IRIS in slow zones
iris <- st_read("zones_30/iris_treatment_grouped.geojson") %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  rename(iris_code = iris) %>% 
  mutate(treatment = 1)

# Join both
acc_g <- acc_g %>% 
  left_join(iris, by = "iris_code")

acc_g <- acc_g %>% 
  mutate(treatment = ifelse(is.na(treatment),
                          0,
                          1))

sum(is.na(acc_g$earliest_year[acc_g$treatment == 1]))

# Drop the observations for which there is no info on year of implementations if they are treated
acc_g <- acc_g %>% 
  filter(!(is.na(earliest_year) & treatment == 1)) # correct

# write grouped version of acc (acc_g)
acc_g <- acc_g %>% 
  mutate(total_victims_per_accident = total_victims/n_accidents,
         dead_per_accident = n_dead/n_accidents)

write_csv(acc_g, "accidents/grouped/accidents_grouped.csv")

# What happens here is that there are some treated IRIS for which I do not have the info on the
# date of implementation of the slow zone they belong to. Thus, these observations are dropped, as they
# cannot be in any of the categories I define for this methodology, and would introduce a bias.


# In the next part, when I form the stacked datasets, some of these observations will ap


######### ---------- Now, stacked form ------------- ########

# Create a loop selecting, for each implementation wave, the obs. in that group, the valid controls
# (i.e., those treated at least 2 years later AND never-treated) and the relevant observations.

# Also, I need to create the relevant treatment variables: 
# 1. post (i.e. an >= wave)
# 2. in relevant zone (i.e. earliest_year == wave)
# 3. treated = post*zone

# NEW VERSION: FOLLOW BREHM ET AL. (2021) AND TAKE ALL THE RELEVANT OBS WHEN POSSIBLE
for (i in unique(acc_g$earliest_year)[2:length(unique(acc_g$earliest_year))]) {
  assign(sprintf("stacked_%s",
                 str_sub(as.character(i),
                         3,
                         4)),
         acc_g %>% 
           filter(earliest_year == i | is.na(earliest_year) | # just includes never-treated
                    (earliest_year > i & an < earliest_year)) %>% 
           mutate(wave = i,
                  post = ifelse(an >= wave,
                                1,
                                0),
                  zone = ifelse(earliest_year == wave & !is.na(earliest_year),
                                1,
                                ifelse(is.na(earliest_year),
                                       0,
                                       0)),
                  treated = ifelse(earliest_year == wave & an >= wave & !is.na(earliest_year),
                                   1,
                                   ifelse(is.na(earliest_year),
                                          0,
                                          0))))
}


## WITH TIME WINDOW
for (i in unique(acc_g$earliest_year)[2:length(unique(acc_g$earliest_year))]) {
  assign(sprintf("stacked_%s",
                 str_sub(as.character(i),
                         3,
                         4)),
         acc_g %>% 
           
           #### NEED TO CHANGE BOTH ARGUMENTS where it says +1
           filter((earliest_year == i | earliest_year > i + 1 | is.na(earliest_year))
                  & an %in% c((i-4):(i+1))) %>% 
           mutate(wave = i,
                  post = ifelse(an >= wave,
                                1,
                                0),
                  zone = ifelse(earliest_year == wave & !is.na(earliest_year),
                                1,
                                ifelse(is.na(earliest_year),
                                       0,
                                       0)),
                  treated = ifelse(earliest_year == wave & an >= wave & !is.na(earliest_year),
                                   1,
                                   ifelse(is.na(earliest_year),
                                          0,
                                          0))) %>% 
           select(-treatment))
}

# The variables treatment is not equivalent to zone or to treated, as it is coded in calendar time and
# not in relative time. Zone is the equivalent of treated for each implementation wave, and treated is effective
# treatment status. Thus, it is dropped right above.

## Merge all in a single file and make some variables categorical so that dummies can be created
stacked <- rbind(stacked_10, stacked_11, stacked_13, stacked_14, stacked_15,
                 stacked_16 , stacked_18, stacked_19) %>% 
  mutate(effective_treatment = treated*post) %>%  # literally equivalent to "treated" lol
  mutate(arr = str_sub(as.character(iris_code),
                       4,
                       5))


# Drop the individual datasets to free up some memory
rm(list = c("stacked_10","stacked_11","stacked_13","stacked_14","stacked_15",
            "stacked_16", "stacked_18","stacked_19"))

# Now, add variables to act as fixed effects (following Gruhl et al., 2021)
# 1. Wave FE (wave).
# 2. IRIS * wave FE (zone) which equals n_g,j 
# the way I see it in Gruhl et al., 2021., it is the same as LEZ_i,g,j.,
# but specific to each implementation wave
# 3. year*implementation wave FE (weirdgreekletter_t,j).
# 4. IRIS FE.

# 1 and 4
stacked <- stacked %>% 
  mutate(wave_fe = as.character(wave),
         iris_fe = as.character(iris_code),
         arr = as.character(arr))

# Use a loop for n.2:
for (i in unique(stacked$iris_code)[order(unique(stacked$iris_code))]) {
  for (j in unique(stacked$wave)[order(unique(stacked$wave))]) {
    
    assign(sprintf("fe_%s", paste0(as.character(i),
                                   as.character(j))),
           ifelse(stacked$iris_code == i & stacked$wave == j & stacked$zone == 1,
                  1,
                  0))
    stacked <- stacked %>% 
      cbind(as_tibble(get(sprintf("fe_%s", paste0(as.character(i),
                                                  as.character(j))))))
    
    rm(list = ls()[2])
    
  }
}

stacked[is.na(stacked$iris_code),]

# Before moving on to define the FE for year-wave, I have to rename variables
iris_wave_combinations <- apply(expand.grid(unique(stacked$iris_code)[order(unique(stacked$iris_code))],
                                            unique(stacked$wave)[order(unique(stacked$wave))]),
                                1,
                                paste,
                                collapse="") # yep, 7000 combinations

# Re-order to make consistent with the underlying variables generated in the loop
# and add "fe_" at the beginning for tractability
iris_wave_combinations <- iris_wave_combinations[order(iris_wave_combinations)]
iris_wave_combinations <- paste("fe",
                                iris_wave_combinations,
                                sep = "_")

colnames(stacked)[19:length(colnames(stacked))] <- iris_wave_combinations

rm(list = c("i", "j", "iris_wave_combinations"))

# Now, sum all the variables' and put into a tibble to select those with a sum of 0
stacked_sum <- lapply(stacked[,19:7018], function(x) sum(x, na.rm = TRUE))

# Set the format
stacked_sum1 <- stacked_sum %>%
  t() %>% 
  as_tibble()

# Extract so that I can get the non-zero summing variables and drop the rest of FE 
stacked_sum2 <- data.frame(variables = colnames(stacked_sum1),
                           sum = stacked_sum1 %>%
                             slice(1) %>% 
                             unlist(., use.names=FALSE))

stacked_sum2 <- stacked_sum2 %>% 
  filter(sum != 0)

# Most of the iris_wave FE are dropped, and nbvolmut as well (apparently it has just zeroes)
stacked1 <- stacked %>% 
  select(unlist(unique(stacked_sum2$variables)))

stacked2 <- stacked[1:18]

rm(stacked_sum)

stacked_clean <- cbind(stacked2, stacked1)

stacked_clean <- stacked_clean  %>% 
  select(c(1:18, 20:ncol(stacked_clean)))

stacked_clean <- stacked_clean %>% 
  rename(iris_code = iris_code...1)

# Join the new outcomes
s1 <- read_csv("accidents/stacked/accidents_stacked_step1_w_3.csv")
s1 <- s1 %>% 
  mutate(iris_code = as.character(iris_code)) %>% 
  left_join(acc_g %>% 
              select(iris_code, an, total_victims, share_dead, share_hospital),
            by = c("iris_code", "an"))

s1 <- s1 %>% 
  relocate(total_victims, share_dead, share_hospital,
           .after = n_accidents)


# Save the file
write_csv(s1, "accidents/stacked/accidents_stacked_step1_w_3.csv")


# Add the group variable
stacked <- stacked %>% 
  mutate(e_i = case_when(is.na(earliest_year) |  earliest_year == 2007 | earliest_year == 2008 ~ 0,
                         earliest_year == 2010 ~ 1,
                         earliest_year == 2011 ~ 2,
                         earliest_year == 2012 ~ 3,
                         earliest_year == 2013 ~ 4,
                         earliest_year == 2014 ~ 5,
                         earliest_year == 2015 ~ 6,
                         earliest_year == 2016 ~ 7,
                         earliest_year == 2017 ~ 8,
                         earliest_year == 2018 ~ 9,
                         earliest_year == 2019 ~ 10),
         .after = earliest_year)



# Use a loop for n.3:
for (i in unique(stacked$an)[order(unique(stacked$an))]) {
  for (j in unique(stacked$wave)[order(unique(stacked$wave))]) {
    
    assign(sprintf("fe_%s", paste0(as.character(i),
                                   as.character(j))),
           ifelse(stacked$an == i & stacked$wave == j,
                  1,
                  0))
    stacked <- stacked %>% 
      cbind(as_tibble(get(sprintf("fe_%s", paste0(as.character(i),
                                                  as.character(j))))))
    
    rm(list = ls()[2])
    
  }
}


# Before moving on to define the FE for iris-wave, I have to rename variables
year_wave_combinations <- apply(expand.grid(unique(stacked$an)[order(unique(stacked$an))],
                                            unique(stacked$wave)[order(unique(stacked$wave))]),
                                1,
                                paste,
                                collapse="") # yep, 88 combinations

# Re-order to make consistent with the underlying variables generated in the loop
# and add "fe_" at the beginning for tractability
year_wave_combinations <- year_wave_combinations[order(year_wave_combinations)]
year_wave_combinations <- paste("fe",
                                year_wave_combinations,
                                sep = "_")

colnames(stacked)[23:ncol(stacked)] <- year_wave_combinations

rm(list = c("i", "j", "year_wave_combinations"))


# Use a loop for group-by-wave FE:
for (i in unique(stacked$e_i)[order(unique(stacked$e_i))]) {
  for (j in unique(stacked$wave)[order(unique(stacked$wave))]) {
    
    assign(sprintf("fe_%s", paste0(as.character(i),
                                   as.character(j))),
           ifelse(stacked$e_i == i & stacked$wave == j,
                  1,
                  0))
    stacked <- stacked %>% 
      cbind(as_tibble(get(sprintf("fe_%s", paste0(as.character(i),
                                                  as.character(j))))))
    
    rm(list = ls()[2])
    
  }
}

# Before moving on to define the FE for iris-wave, I have to rename variables
group_wave_combinations <- apply(expand.grid(unique(stacked$e_i)[order(unique(stacked$e_i))],
                                             unique(stacked$wave)[order(unique(stacked$wave))]),
                                 1,
                                 paste,
                                 collapse="") # yep, 88 combinations

# Re-order to make consistent with the underlying variables generated in the loop
# and add "fe_" at the beginning for tractability
group_wave_combinations <- group_wave_combinations[order(group_wave_combinations)]
group_wave_combinations <- paste("fe",
                                 group_wave_combinations,
                                 sep = "_")

colnames(stacked)[152:length(colnames(stacked))] <- group_wave_combinations

rm(list = c("i", "j", "group_wave_combinations"))


# Drop obs. outside of arrondissement
stacked <- stacked %>% 
  filter(!is.na(arr))

# Use a loop for arrondissement-by-year FE:
for (i in unique(stacked$arr)[order(unique(stacked$arr))]){
  for (j in unique(stacked$an)[order(unique(stacked$an))]) {
    
    assign(sprintf("fe_arr_%s", paste0(as.character(i),
                                       as.character(j))),
           ifelse(stacked$arr == i & stacked$an == j,
                  1,
                  0))
    stacked <- stacked %>% 
      cbind(as_tibble(get(sprintf("fe_arr_%s", paste0(as.character(i),
                                                      as.character(j))))))
    
    rm(list = ls()[2])
    
  }
}

# Before moving on to define the FE for iris-wave, I have to rename variables
arr_wave_combinations <- apply(expand.grid(unique(stacked$arr)[order(unique(stacked$arr))],
                                           unique(stacked$an)[order(unique(stacked$an))]),
                               1,
                               paste,
                               collapse="") # yep, 88 combinations

# Re-order to make consistent with the underlying variables generated in the loop
# and add "fe_" at the beginning for tractability
arr_wave_combinations <- arr_wave_combinations[order(arr_wave_combinations)]
arr_wave_combinations <- paste("fe_arr",
                               arr_wave_combinations,
                               sep = "_")

colnames(stacked)[224:length(colnames(stacked))] <- arr_wave_combinations

rm(list = c("i", "j", "arr_wave_combinations"))



# Now, sum all the variables' and put into a tibble to select those with a sum of 0
stacked_sum <- lapply(stacked[,24:length(colnames(stacked))], function(x) sum(x, na.rm = TRUE))

# Set the format
stacked_sum1 <- stacked_sum %>%
  t() %>% 
  as_tibble()

# Extract so that I can get the non-zero summing variables and drop the rest of FE 
stacked_sum2 <- data.frame(variables = colnames(stacked_sum1),
                           sum = stacked_sum1 %>%
                             slice(1) %>% 
                             unlist(., use.names=FALSE))

stacked_sum2 <- stacked_sum2 %>% 
  filter(sum != 0)

stacked1 <- stacked %>% 
  select(unlist(unique(stacked_sum2$variables)))

stacked2 <- stacked[1:23]

rm(stacked_sum2)

stacked_clean <- cbind(stacked2, stacked1)

stacked_clean <- stacked_clean %>% 
  select(c(1:23, 25:ncol(stacked_clean)))

stacked_clean <- stacked_clean %>% 
  rename(iris_code = iris_code...1)

# Join the new outcomes
s2 <- read_csv("accidents/stacked/accidents_stacked_step2_w_3.csv")
s2 <- s2 %>% 
  mutate(iris_code = as.character(iris_code)) %>% 
  left_join(acc_g %>% 
              select(iris_code, an, total_victims, share_dead, share_hospital),
            by = c("iris_code", "an"))

s2 <- s2 %>% 
  relocate(total_victims, share_dead, share_hospital,
           .after = n_accidents)

  # Save until now to avoid memory crashes
write_csv(stacked_clean, "accidents/stacked/accidents_stacked_full_period.csv")


############################################
##### 3. REGRESSIONS WITH STACKED DID ######
############################################

# Import both parts of the data (dv3f)
stacked1 <- read_csv("dv3f_new_variables/stacked/stacked_step1.csv")

stacked2 <- read_csv("dv3f_new_variables/stacked/stacked_step2.csv")
rm(stacked)

# Import both parts of the data (accidents)
stacked1_acc <- read_csv("accidents/stacked/accidents_stacked_step1.csv")

stacked2_acc <- read_csv("accidents/stacked/accidents_stacked_step2.csv")


stacked_acc <- cbind(stacked1_acc, stacked2_acc[,19:ncol(stacked2_acc)])

stacked_acc <- stacked_acc %>% 
  filter(!is.na(iris_code))

# Format
stacked_acc <- stacked_acc %>% 
  mutate(wave_fe = as.character(wave_fe),
         iris_fe = as.character(iris_fe))




####### STANDARD DID
# define treatment
acc_g <- acc_g %>% 
  mutate(effective_treatment = ifelse(treatment == 1 & an >= earliest_year & !is.na(earliest_year),
                                      1,
                                      0),
         arr = str_sub(iris_code,
                       4,
                       5),
         year_fe = as.character(an)) %>% 
  filter(!is.na(iris_code))

summary(acc_g$effective_treatment)

acc_did <- plm(n_accidents ~ effective_treatment + arr,
              data = acc_g,
              index = c("iris_code", "an"),
              model = "within")

summary(acc_did)

# Goodman-Bacon decomposition

acc_did_bacon <- bacon(n_accidents ~ effective_treatment + arr,
                       data = acc_g,
                       id_var = "iris_code",
                       time_var = "an")

?bacon


### TRY TO RUN A SIMPLE REGRESSION
formula_acc <- as.formula(paste("n_accidents ~ ",
                                paste(c("treated", "wave_fe", "arr",
                                        colnames(stacked_acc)[19:ncol(stacked_acc)]),
                                      collapse = "+")))



acc_ols <- lm(formula_acc,
              data = stacked_acc)

summary(acc_ols)

# FIXED EFFECTS
acc_ols_fe <- plm(formula_acc,
                  data = stacked_acc,
                  index = c("iris_code", "an"),
                  model = "within")
  
  
summary(acc_ols_fe)
