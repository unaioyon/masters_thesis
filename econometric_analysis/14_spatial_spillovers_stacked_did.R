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
library(gridExtra) # arrange plots
library(ggpubr) # arrange plots
library(stargazer)
library(plotrix) # compute standard errors of mean
library(did) # CALLAWAY & SANT'ANNA
library(fixest) # SUN & ABRAHAM
library(bacondecomp)
library(plm) # fixed effects
library(estimatr) # cluster sd in regression specifications
library(DRDID) # twfe for repeated cross-sections
library(sandwich) # clustered se
library(lmtest) # clustered se

# memory problems
library(usethis) 
usethis::edit_r_environ()

# Set working directory
setwd("/Users/unaioyon/Desktop/masters_thesis/data/fra")

options(scipen = 999)


### Import the selection of treated accidents and the selection of untreated accidents
acc_t <- st_read("accidents/spillovers/treated_2010.geojson") %>% 
  st_drop_geometry() %>% 
  as_tibble()

acc_100 <- st_read("accidents/spillovers/buffer_100_2010.geojson") %>% 
  st_drop_geometry() %>% 
  as_tibble()

acc_200 <- st_read("accidents/spillovers/buffer_200_2010.geojson") %>% 
  st_drop_geometry() %>% 
  as_tibble()

acc_300 <- st_read("accidents/spillovers/buffer_300_2010.geojson") %>% 
  st_drop_geometry() %>% 
  as_tibble()

acc_all <- st_read("accidents/spillovers/buffer_all_2010.geojson") %>% 
  st_drop_geometry() %>% 
  as_tibble()


##### Now, join treated with each control
acc1 <- acc_t %>% 
  bind_rows(acc_100)

acc2 <- acc_t %>% 
  bind_rows(acc_200)

acc3 <- acc_t %>% 
  bind_rows(acc_300)

acc_all <- acc_t %>% 
  bind_rows(acc_all)


# Remove the non-valid observations (as in the 5_stacked_did_preparation.R file)
acc1 <- acc1 %>% 
  filter(!(slow_zone_d == 1 & is.na(slow_zone_year) & as.numeric(slow_zone_year) <= an)) %>% 
  mutate(slow_zone_year = case_when(slow_zone_year == 2007 | slow_zone_year == 2008 ~ NA,
                                    slow_zone_year == slow_zone_year ~ slow_zone_year)) %>% 
  filter(!(slow_zone_year == "2011") | is.na(slow_zone_year))

acc2 <- acc2 %>% 
  filter(!(slow_zone_d == 1 & is.na(slow_zone_year) & as.numeric(slow_zone_year) <= an)) %>% 
  mutate(slow_zone_year = case_when(slow_zone_year == 2007 | slow_zone_year == 2008 ~ NA,
                                    slow_zone_year == slow_zone_year ~ slow_zone_year)) %>% 
  filter(!(slow_zone_year == "2011") | is.na(slow_zone_year))

acc3 <- acc3 %>% 
  filter(!(slow_zone_d == 1 & is.na(slow_zone_year) & as.numeric(slow_zone_year) <= an)) %>% 
  mutate(slow_zone_year = case_when(slow_zone_year == 2007 | slow_zone_year == 2008 ~ NA,
                                    slow_zone_year == slow_zone_year ~ slow_zone_year)) %>% 
  filter(!(slow_zone_year == "2011") | is.na(slow_zone_year))

acc_all <- acc_all %>% 
  filter(!(slow_zone_d == 1 & is.na(slow_zone_year) & as.numeric(slow_zone_year) <= an)) %>% 
  mutate(slow_zone_year = case_when(slow_zone_year == 2007 | slow_zone_year == 2008 ~ NA,
                                    slow_zone_year == slow_zone_year ~ slow_zone_year)) %>% 
  filter(!(slow_zone_year == "2011") | is.na(slow_zone_year))



# now, group and get the relevant datasets at the IRIS level, obtaining the desired outcomes
acc1_g <- acc1 %>% 
  group_by(iris_code, an) %>% 
  summarise(n_accidents = n(),
            share_pedestrian = mean(has_pietons, na.rm = TRUE),
            share_bike = mean(has_velo, na.rm = TRUE),
            n_dead = sum(tue_nb, na.rm = TRUE),
            n_hospital = sum(hospitalise_nb, na.rm = TRUE),
            severe_victims = sum(tue_nb + hospitalise_nb),
            total_victims = sum(tue_nb + indemne_nb + hospitalise_nb + blesseleger_nb)) %>% 
  mutate(iris_year = paste0(as.character(iris_code),
                            as.character(an)),
         share_dead = n_dead/total_victims,
         share_hospital = n_hospital/total_victims) %>% 
  arrange(iris_year)


acc2_g <- acc2 %>% 
  group_by(iris_code, an) %>% 
  summarise(n_accidents = n(),
            share_pedestrian = mean(has_pietons, na.rm = TRUE),
            share_bike = mean(has_velo, na.rm = TRUE),
            n_dead = sum(tue_nb, na.rm = TRUE),
            n_hospital = sum(hospitalise_nb, na.rm = TRUE),
            severe_victims = sum(tue_nb + hospitalise_nb),
            total_victims = sum(tue_nb + indemne_nb + hospitalise_nb + blesseleger_nb)) %>% 
  mutate(iris_year = paste0(as.character(iris_code),
                            as.character(an)),
         share_dead = n_dead/total_victims,
         share_hospital = n_hospital/total_victims) %>% 
  arrange(iris_year)

acc3_g <- acc3 %>% 
  group_by(iris_code, an) %>% 
  summarise(n_accidents = n(),
            share_pedestrian = mean(has_pietons, na.rm = TRUE),
            share_bike = mean(has_velo, na.rm = TRUE),
            n_dead = sum(tue_nb, na.rm = TRUE),
            n_hospital = sum(hospitalise_nb, na.rm = TRUE),
            severe_victims = sum(tue_nb + hospitalise_nb),
            total_victims = sum(tue_nb + indemne_nb + hospitalise_nb + blesseleger_nb)) %>% 
  mutate(iris_year = paste0(as.character(iris_code),
                            as.character(an)),
         share_dead = n_dead/total_victims,
         share_hospital = n_hospital/total_victims) %>% 
  arrange(iris_year)

acc_all_g <- acc_all %>% 
  group_by(iris_code, an) %>% 
  summarise(n_accidents = n(),
            share_pedestrian = mean(has_pietons, na.rm = TRUE),
            share_bike = mean(has_velo, na.rm = TRUE),
            n_dead = sum(tue_nb, na.rm = TRUE),
            n_hospital = sum(hospitalise_nb, na.rm = TRUE),
            severe_victims = sum(tue_nb + hospitalise_nb),
            total_victims = sum(tue_nb + indemne_nb + hospitalise_nb + blesseleger_nb)) %>% 
  mutate(iris_year = paste0(as.character(iris_code),
                            as.character(an)),
         share_dead = n_dead/total_victims,
         share_hospital = n_hospital/total_victims) %>% 
  arrange(iris_year)



#### Now, add controls and treatment status
iris_controls <- read_csv("iris/iris_controls_accidents.csv") %>% 
  mutate(iris_code = as.character(iris_code)) %>% 
  rename(an = year)
  
iris_t <- st_read("zones_30/iris_treatment_grouped.geojson") %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  rename(iris_code = iris)

# Now, join treatment status first

acc1_g <- acc1_g %>% 
  left_join(iris_t %>% 
              dplyr::select(c(1, 2)), by = "iris_code") %>% 
  mutate(treated_zone = ifelse(is.na(earliest_year),
                               0,
                               1))

acc2_g <- acc2_g %>% 
  left_join(iris_t %>% 
              dplyr::select(c(1, 2)), by = "iris_code") %>% 
  mutate(treated_zone = ifelse(is.na(earliest_year),
                               0,
                               1))

acc3_g <- acc3_g %>% 
  left_join(iris_t %>% 
              dplyr::select(c(1, 2)), by = "iris_code") %>% 
  mutate(treated_zone = ifelse(is.na(earliest_year),
                               0,
                               1))

acc_all_g <- acc_all_g %>% 
  left_join(iris_t %>% 
              dplyr::select(c(1, 2)), by = "iris_code") %>% 
  mutate(treated_zone = ifelse(is.na(earliest_year),
                               0,
                               1))

# now, add controls
acc1_g <- acc1_g %>% 
  left_join(iris_controls %>% 
              dplyr::select(-id), by = c("iris_code", "an"))

acc2_g <- acc2_g %>% 
  left_join(iris_controls %>% 
              dplyr::select(-id), by = c("iris_code", "an"))

acc3_g <- acc3_g %>% 
  left_join(iris_controls %>% 
              dplyr::select(-id), by = c("iris_code", "an"))

acc_all_g <- acc_all_g %>% 
  left_join(iris_controls %>% 
              dplyr::select(-id), by = c("iris_code", "an"))


# Now, generate a stacked version
stacked10_1 <- acc1_g %>% 
  filter(earliest_year == 2010 | is.na(earliest_year) | # just includes never-treated
           (earliest_year > 2010 & an < earliest_year)) %>% 
  mutate(wave = 2010,
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
                                 0)))

stacked10_2 <- acc2_g %>% 
  filter(earliest_year == 2010 | is.na(earliest_year) | # just includes never-treated
           (earliest_year > 2010 & an < earliest_year)) %>% 
  mutate(wave = 2010,
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
                                 0)))

stacked10_3 <- acc3_g %>% 
  filter(earliest_year == 2010 | is.na(earliest_year) | # just includes never-treated
           (earliest_year > 2010 & an < earliest_year)) %>% 
  mutate(wave = 2010,
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
                                 0)))

stacked10_all <- acc_all_g %>% 
  filter(earliest_year == 2010 | is.na(earliest_year) | # just includes never-treated
           (earliest_year > 2010 & an < earliest_year)) %>% 
  mutate(wave = 2010,
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
                                 0)))



#### NOW ADD fixed effects
stacked10_1 <- stacked10_1 %>% 
  mutate(iris_fe = as.character(iris_code),
         arr = str_sub(iris_code,
                       4,
                       5),
         arr_fe = as.character(arr))

# Use a loop for arrondissement-by-year FE:
for (i in unique(stacked10_1$arr)[order(unique(stacked10_1$arr))]){
  for (j in unique(stacked10_1$an)[order(unique(stacked10_1$an))]) {
    
    assign(sprintf("fe_arr_%s", paste0(as.character(i),
                                       as.character(j))),
           ifelse(stacked10_1$arr == i & stacked10_1$an == j,
                  1,
                  0))
    stacked10_1 <- stacked10_1 %>% 
      cbind(as_tibble(get(sprintf("fe_arr_%s", paste0(as.character(i),
                                                      as.character(j))))))
    
    rm(list = ls()[2])
    
  }
}

# Before moving on to define the FE for iris-wave, I have to rename variables
arr_wave_combinations <- apply(expand.grid(unique(stacked10_1$arr)[order(unique(stacked10_1$arr))],
                                           unique(stacked10_1$an)[order(unique(stacked10_1$an))]),
                               1,
                               paste,
                               collapse="") # yep, 88 combinations

# Re-order to make consistent with the underlying variables generated in the loop
# and add "fe_" at the beginning for tractability
arr_wave_combinations <- arr_wave_combinations[order(arr_wave_combinations)]
arr_wave_combinations <- paste("fe_arr",
                               arr_wave_combinations,
                               sep = "_")

colnames(stacked10_1)[29:length(colnames(stacked10_1))] <- arr_wave_combinations

rm(list = c("i", "j", "arr_wave_combinations"))




stacked10_2 <- stacked10_2 %>% 
  mutate(iris_fe = as.character(iris_code),
         arr = str_sub(iris_code,
                       4,
                       5),
         arr_fe = as.character(arr))

# Use a loop for arrondissement-by-year FE:
for (i in unique(stacked10_2$arr)[order(unique(stacked10_2$arr))]){
  for (j in unique(stacked10_2$an)[order(unique(stacked10_2$an))]) {
    
    assign(sprintf("fe_arr_%s", paste0(as.character(i),
                                       as.character(j))),
           ifelse(stacked10_2$arr == i & stacked10_2$an == j,
                  1,
                  0))
    stacked10_2 <- stacked10_2 %>% 
      cbind(as_tibble(get(sprintf("fe_arr_%s", paste0(as.character(i),
                                                      as.character(j))))))
    
    rm(list = ls()[2])
    
  }
}

# Before moving on to define the FE for iris-wave, I have to rename variables
arr_wave_combinations <- apply(expand.grid(unique(stacked10_2$arr)[order(unique(stacked10_2$arr))],
                                           unique(stacked10_2$an)[order(unique(stacked10_2$an))]),
                               1,
                               paste,
                               collapse="") # yep, 88 combinations

# Re-order to make consistent with the underlying variables generated in the loop
# and add "fe_" at the beginning for tractability
arr_wave_combinations <- arr_wave_combinations[order(arr_wave_combinations)]
arr_wave_combinations <- paste("fe_arr",
                               arr_wave_combinations,
                               sep = "_")

colnames(stacked10_2)[29:length(colnames(stacked10_2))] <- arr_wave_combinations

rm(list = c("i", "j", "arr_wave_combinations"))





stacked10_3 <- stacked10_3 %>% 
  mutate(iris_fe = as.character(iris_code),
         arr = str_sub(iris_code,
                       4,
                       5),
         arr_fe = as.character(arr))

# Use a loop for arrondissement-by-year FE:
for (i in unique(stacked10_3$arr)[order(unique(stacked10_3$arr))]){
  for (j in unique(stacked10_3$an)[order(unique(stacked10_3$an))]) {
    
    assign(sprintf("fe_arr_%s", paste0(as.character(i),
                                       as.character(j))),
           ifelse(stacked10_3$arr == i & stacked10_3$an == j,
                  1,
                  0))
    stacked10_3 <- stacked10_3 %>% 
      cbind(as_tibble(get(sprintf("fe_arr_%s", paste0(as.character(i),
                                                      as.character(j))))))
    
    rm(list = ls()[2])
    
  }
}

# Before moving on to define the FE for iris-wave, I have to rename variables
arr_wave_combinations <- apply(expand.grid(unique(stacked10_3$arr)[order(unique(stacked10_3$arr))],
                                           unique(stacked10_3$an)[order(unique(stacked10_3$an))]),
                               1,
                               paste,
                               collapse="") # yep, 88 combinations

# Re-order to make consistent with the underlying variables generated in the loop
# and add "fe_" at the beginning for tractability
arr_wave_combinations <- arr_wave_combinations[order(arr_wave_combinations)]
arr_wave_combinations <- paste("fe_arr",
                               arr_wave_combinations,
                               sep = "_")

colnames(stacked10_3)[29:length(colnames(stacked10_3))] <- arr_wave_combinations

rm(list = c("i", "j", "arr_wave_combinations"))






stacked10_all <- stacked10_all %>% 
  mutate(iris_fe = as.character(iris_code),
         arr = str_sub(iris_code,
                       4,
                       5),
         arr_fe = as.character(arr))

# Use a loop for arrondissement-by-year FE:
for (i in unique(stacked10_all$arr)[order(unique(stacked10_all$arr))]){
  for (j in unique(stacked10_all$an)[order(unique(stacked10_all$an))]) {
    
    assign(sprintf("fe_arr_%s", paste0(as.character(i),
                                       as.character(j))),
           ifelse(stacked10_all$arr == i & stacked10_all$an == j,
                  1,
                  0))
    stacked10_all <- stacked10_all %>% 
      cbind(as_tibble(get(sprintf("fe_arr_%s", paste0(as.character(i),
                                                      as.character(j))))))
    
    rm(list = ls()[2])
    
  }
}

# Before moving on to define the FE for iris-wave, I have to rename variables
arr_wave_combinations <- apply(expand.grid(unique(stacked10_all$arr)[order(unique(stacked10_all$arr))],
                                           unique(stacked10_all$an)[order(unique(stacked10_all$an))]),
                               1,
                               paste,
                               collapse="") # yep, 88 combinations

# Re-order to make consistent with the underlying variables generated in the loop
# and add "fe_" at the beginning for tractability
arr_wave_combinations <- arr_wave_combinations[order(arr_wave_combinations)]
arr_wave_combinations <- paste("fe_arr",
                               arr_wave_combinations,
                               sep = "_")

colnames(stacked10_all)[29:length(colnames(stacked10_all))] <- arr_wave_combinations

rm(list = c("i", "j", "arr_wave_combinations"))





#### FINALLY, YEAR FE
stacked10_1 <- stacked10_1 %>% 
  mutate(year_fe = as.character(an))

stacked10_2 <- stacked10_2 %>% 
  mutate(year_fe = as.character(an))

stacked10_3 <- stacked10_3 %>% 
  mutate(year_fe = as.character(an))

stacked10_all <- stacked10_all %>% 
  mutate(year_fe = as.character(an))


# Remove columns with zeroes
# Now, sum all the variables' and put into a tibble to select those with a sum of 0
stacked_sum <- lapply(stacked10_3[,30:364], function(x) sum(x, na.rm = TRUE))

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

stacked1 <- stacked10_3 %>% 
  dplyr::select(unlist(unique(stacked_sum2$variables)))

stacked2 <- stacked10_3[1:29]

rm(stacked_sum2)

stacked_clean <- cbind(stacked2, stacked1)

stacked_clean <- stacked_clean %>% 
  dplyr::select(c(1:29, 31:ncol(stacked_clean)))

stacked_clean <- stacked_clean %>% 
  rename(iris_code = iris_code...1)

stacked1_3 <- stacked_clean




# save the stacked versions
# write_csv(stacked1_final, "accidents/spillovers/stacked/stacked1.csv")
# write_csv(stacked1_2, "accidents/spillovers/stacked/stacked2.csv")
# write_csv(stacked1_3, "accidents/spillovers/stacked/stacked3.csv")
# write_csv(stacked1_all, "accidents/spillovers/stacked/stackedall.csv")




##### Import data
st1 <- read_csv("accidents/spillovers/stacked/stacked1.csv")
st2 <- read_csv("accidents/spillovers/stacked/stacked2.csv")
st3 <- read_csv("accidents/spillovers/stacked/stacked3.csv")
stall <- read_csv("accidents/spillovers/stacked/stackedall.csv")

st1 <- st1 %>% 
  mutate(iris_fe = as.character(iris_fe),
         an_fe = as.character(an))

st2 <- st2 %>% 
  mutate(iris_fe = as.character(iris_fe),
         an_fe = as.character(an))

st3 <- st3 %>% 
  mutate(iris_fe = as.character(iris_fe),
         an_fe = as.character(an))

stall <- stall %>% 
  mutate(iris_fe = as.character(iris_fe),
         an_fe = as.character(an))

###############################             RUN STACKED DID ESTIMATION              ###################


f1 <- as.formula(paste("n_accidents ~ ",
                       paste(c("treated", colnames(st1)[c(15:21, 26, 28:ncol(st1))]),
                             collapse = "+")))

did1 <- lm(f1,
           data = st1)


coeftest(did1, vcov. = vcovCL, cluster = ~ iris_code)

f2 <- as.formula(paste("n_accidents ~ ",
                       paste(c("treated", colnames(st2)[c(15:21, 26, 28:ncol(st2))]),
                             collapse = "+")))

did2 <- lm(f2,
           data = st2)


coeftest(did2, vcov. = vcovCL, cluster = ~ iris_code)

f3 <- as.formula(paste("n_accidents ~ ",
                       paste(c("treated", colnames(st3)[c(15:21, 26, 28:ncol(st3))]),
                             collapse = "+")))

did3 <- lm(f3,
           data = st3)


coeftest(did3, vcov. = vcovCL, cluster = ~ iris_code)

fall <- as.formula(paste("n_accidents ~ ",
                       paste(c("treated", colnames(stall)[c(15:21, 26, 28:ncol(stall))]),
                             collapse = "+")))

didall <- lm(fall,
           data = stall)


coeftest(didall, vcov. = vcovCL, cluster = ~ iris_code)



# Import normal stacked to check
st <- read_csv("accidents/stacked/accidents_stacked_full_period.csv")


st <- st %>% 
  mutate(arr = as.character(arr),
         iris_fe = as.character(iris_code),
         an_fe = as.character(an)) %>% 
  filter(wave == 2010)


f_full <- as.formula(paste("n_accidents ~ ",
                           paste(c("treated", colnames(st)[c(21, 23, 196:516)]),
                                 collapse = "+")))


did_full = lm(f_full,
              data = st)

coeftest(did_full, vcov. = vcovCL, cluster = ~ iris_code)







##########                  EVENT STUDY DESIGN                


st <- st %>% 
  mutate(post_4 = ifelse(an == 2006,
                         1,
                         0),
         post_3 = ifelse(an == 2007,
                         1,
                         0),
         post_2 = ifelse(an == 2008,
                         1,
                         0),
         post_1 = ifelse(an == 2009,
                         1,
                         0),
         post0 = ifelse(an == 2010,
                        1,
                        0),
         post1 = ifelse(an == 2011,
                        1,
                        0),
         post2 = ifelse(an == 2012,
                        1,
                        0),
         post3 = ifelse(an == 2013,
                        1,
                        0),
         post4 = ifelse(an == 2014,
                        1,
                        0),
         post5 = ifelse(an == 2015,
                        1,
                        0),
         post6 = ifelse(an == 2016,
                        1,
                        0),
         post7 = ifelse(an == 2017,
                        1,
                        0),
         post8 = ifelse(an == 2018,
                        1,
                        0),
         .after = post)

# Create the interacted variables b/n the previous dummies and
# the zone dummy
st <- st %>%
  mutate_at(vars(starts_with("post")),  funs(mod = . * zone)) %>%
  rename_at(vars(ends_with("_mod")), funs(paste("treated", gsub("_mod", "", .), sep = "_"))) %>% 
  relocate(starts_with("treated_post"),
           .after = zone)




st1 <- st1 %>% 
  mutate(post_4 = ifelse(an == 2006,
                         1,
                         0),
         post_3 = ifelse(an == 2007,
                         1,
                         0),
         post_2 = ifelse(an == 2008,
                         1,
                         0),
         post_1 = ifelse(an == 2009,
                         1,
                         0),
         post0 = ifelse(an == 2010,
                        1,
                        0),
         post1 = ifelse(an == 2011,
                        1,
                        0),
         post2 = ifelse(an == 2012,
                        1,
                        0),
         post3 = ifelse(an == 2013,
                        1,
                        0),
         post4 = ifelse(an == 2014,
                        1,
                        0),
         post5 = ifelse(an == 2015,
                        1,
                        0),
         post6 = ifelse(an == 2016,
                        1,
                        0),
         post7 = ifelse(an == 2017,
                        1,
                        0),
         post8 = ifelse(an == 2018,
                        1,
                        0),
         .after = post)

# Create the interacted variables b/n the previous dummies and
# the zone dummy
st1 <- st1 %>%
  mutate_at(vars(starts_with("post")),  funs(mod = . * zone)) %>%
  rename_at(vars(ends_with("_mod")), funs(paste("treated", gsub("_mod", "", .), sep = "_"))) %>% 
  relocate(starts_with("treated_post"),
           .after = zone)


st2 <- st2 %>% 
  mutate(post_4 = ifelse(an == 2006,
                         1,
                         0),
         post_3 = ifelse(an == 2007,
                         1,
                         0),
         post_2 = ifelse(an == 2008,
                         1,
                         0),
         post_1 = ifelse(an == 2009,
                         1,
                         0),
         post0 = ifelse(an == 2010,
                        1,
                        0),
         post1 = ifelse(an == 2011,
                        1,
                        0),
         post2 = ifelse(an == 2012,
                        1,
                        0),
         post3 = ifelse(an == 2013,
                        1,
                        0),
         post4 = ifelse(an == 2014,
                        1,
                        0),
         post5 = ifelse(an == 2015,
                        1,
                        0),
         post6 = ifelse(an == 2016,
                        1,
                        0),
         post7 = ifelse(an == 2017,
                        1,
                        0),
         post8 = ifelse(an == 2018,
                        1,
                        0),
         .after = post)

# Create the interacted variables b/n the previous dummies and
# the zone dummy
st2 <- st2 %>%
  mutate_at(vars(starts_with("post")),  funs(mod = . * zone)) %>%
  rename_at(vars(ends_with("_mod")), funs(paste("treated", gsub("_mod", "", .), sep = "_"))) %>% 
  relocate(starts_with("treated_post"),
           .after = zone)


st3 <- st3 %>% 
  mutate(post_4 = ifelse(an == 2006,
                         1,
                         0),
         post_3 = ifelse(an == 2007,
                         1,
                         0),
         post_2 = ifelse(an == 2008,
                         1,
                         0),
         post_1 = ifelse(an == 2009,
                         1,
                         0),
         post0 = ifelse(an == 2010,
                        1,
                        0),
         post1 = ifelse(an == 2011,
                        1,
                        0),
         post2 = ifelse(an == 2012,
                        1,
                        0),
         post3 = ifelse(an == 2013,
                        1,
                        0),
         post4 = ifelse(an == 2014,
                        1,
                        0),
         post5 = ifelse(an == 2015,
                        1,
                        0),
         post6 = ifelse(an == 2016,
                        1,
                        0),
         post7 = ifelse(an == 2017,
                        1,
                        0),
         post8 = ifelse(an == 2018,
                        1,
                        0),
         .after = post)

# Create the interacted variables b/n the previous dummies and
# the zone dummy
st3 <- st3 %>%
  mutate_at(vars(starts_with("post")),  funs(mod = . * zone)) %>%
  rename_at(vars(ends_with("_mod")), funs(paste("treated", gsub("_mod", "", .), sep = "_"))) %>% 
  relocate(starts_with("treated_post"),
           .after = zone)



stall <- stall %>% 
  mutate(post_4 = ifelse(an == 2006,
                         1,
                         0),
         post_3 = ifelse(an == 2007,
                         1,
                         0),
         post_2 = ifelse(an == 2008,
                         1,
                         0),
         post_1 = ifelse(an == 2009,
                         1,
                         0),
         post0 = ifelse(an == 2010,
                        1,
                        0),
         post1 = ifelse(an == 2011,
                        1,
                        0),
         post2 = ifelse(an == 2012,
                        1,
                        0),
         post3 = ifelse(an == 2013,
                        1,
                        0),
         post4 = ifelse(an == 2014,
                        1,
                        0),
         post5 = ifelse(an == 2015,
                        1,
                        0),
         post6 = ifelse(an == 2016,
                        1,
                        0),
         post7 = ifelse(an == 2017,
                        1,
                        0),
         post8 = ifelse(an == 2018,
                        1,
                        0),
         .after = post)

# Create the interacted variables b/n the previous dummies and
# the zone dummy
stall <- stall %>%
  mutate_at(vars(starts_with("post")),  funs(mod = . * zone)) %>%
  rename_at(vars(ends_with("_mod")), funs(paste("treated", gsub("_mod", "", .), sep = "_"))) %>% 
  relocate(starts_with("treated_post"),
           .after = zone)






###### FULL
fes <- as.formula(paste("n_accidents ~ ",
                         paste(colnames(st)[c(33:45, 48, 50, 223:ncol(st))],
                               collapse = "+")))

es <- lm(fes,
          data = st)

sees <- coeftest(es, vcov. = vcovCL, cluster = ~ iris_code)


# Coerce to a dataframe
test_est_df <- sees[,] %>% 
  as_tibble() %>%
  mutate(variable = rownames(sees),
         .before = Estimate)

# Get confidence intervals
test_est_df <- test_est_df %>% 
  bind_cols(confint(sees) %>% 
              as_tibble()) %>% 
  slice(2:14) %>% 
  mutate(group = 1,
         variable1 = ifelse(variable == "treated_post_4",
                            "treated_apre_1",
                            ifelse(variable == "treated_post_3",
                                   "treated_apre_2",
                                   ifelse(variable == "treated_apost_2",
                                          "treated_pre_3",
                                          ifelse(variable == "treated_post_1",
                                                 "treated_apre_4",
                                                 variable)))))






######               PLOT               ######
ggplot(test_est_df, aes(x = variable1, y = Estimate, group = 1)) + 
geom_hline(yintercept = 0) + 
  stat_summary(fun = "mean", geom = "line", color = "green") + 
  geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`, width=0.2), color = "darkgrey") +
  geom_point(size = 2.5, color = "darkgreen") +
  labs(x = "Event time", y = "Estimate") + 
  scale_y_continuous(limits = c(-3.2, 0.7)) + 
  scale_x_discrete(labels = (-4:8)) + theme_bw() + 
  ggtitle("Number of accidents")
