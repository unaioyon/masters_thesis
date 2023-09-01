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
library(estimatr) # cluster sd in regression specifications
library(DRDID) # twfe for repeated cross-sections


# memory problems
library(usethis) 
usethis::edit_r_environ()

# Set working directory
setwd("/Users/unaioyon/Desktop/masters_thesis/data/fra")

options(scipen = 999)



# IRIS level pre-treatment controls
iris <- read_csv("delineation/iris_characteristics_full_data.csv")



############################################
##### 1. REGRESSIONS WITH STACKED DID ######
############################################


#### DV3F ####################################################
# Import both parts of the data (dv3f)

###### USING TIME WINDOW
# IRIS-by-wave fe + outcomes + controls
stacked1 <- read_csv("dv3f_new_variables/stacked/stacked_step1.csv")

# Year-by-wave fe
stacked2 <- read_csv("dv3f_new_variables/stacked/stacked_step2.csv")

stacked <- stacked1 %>% 
  bind_cols(stacked2[47:ncol(stacked2)]) 

# drop duplicated treatment variable
stacked <- stacked %>% 
  select(-effective_treatment)

rm(list = c("stacked1", "stacked2"))


###### USING FULL SAMPLE PERIOD
stacked_dv3f_full <- read_csv("dv3f_new_variables/stacked/stacked_full_period.csv")

# Import DV3F normal data
dv3f <- read_csv("dv3f_new_variables/dv3f_clean_no_outliers.csv")



####### 1. Standard DiD (TWFE)

# Modify the format and select the relevant variables
dv3f_sd_did <- dv3f %>% 
  mutate(ffancstmin_dif = 2019 - ffancstmin,
         ffancstmax_dif = 2019 - ffancstmax,
         arr = as.character(arr),
         iris_fe = as.character(iris_code),
         treatment = ifelse(slow_zone_d == 0,
                            0,
                            ifelse(slow_zone_d == 1 & is.na(slow_zone_year),
                                   NA,
                                   ifelse(anneemut >= slow_zone_year,
                                   1,
                                   0))),
         anneemut = as.character(anneemut)) %>%
  mutate(log_price_m2 = log(price_m2),
         .after = price_m2) %>% 
  select(log_price_m2, price_m2,  treatment, libnatmut, nblocmai, nblocapt,
         ffancstmin_dif, ffancstmax_dif, libtypbien, arr, anneemut, iris_fe)

rm(dv3f)


# Run the DiD to get pseudo-two-way-fe using repeated stationary cross-sections

twfe_did_rc(y = dv3f_sd_did$price_m2,
            )



# Run the DiD (9:48 to )
dv3f_did_formula <- as.formula(paste("price_m2 ~ ", paste(colnames(dv3f_sd_did)[c(10, 2:8)],
                                                       collapse = "+")))

dv3f_did <- lm(dv3f_did_formula,
               data = dv3f_sd_did)

summary(dv3f_did)




dv3f_did_cluster <- lm_robust(dv3f_formula,
               data = dv3f_sd_did,
               clusters = arr)





####### 2. Static Stacked DiD
stacked1 <- stacked_dv3f_full %>% 
  select(-effective_treatment)

stacked1 <- stacked1 %>% 
  rename(treatment = treated) %>% 
  mutate(ffancstmin_dif = 2019 - ffancstmin,
         ffancstmax_dif = 2019 - ffancstmax,
         arr = as.character(arr),
         wave_fe = as.character(wave),
         wave = wave + 2010,
         iris_code = as.character(iris_code),
         linear_trend = case_when(anneemut == 2010 ~ 1,
                                  anneemut == 2011 ~ 2,
                                  anneemut == 2012 ~ 3,
                                  anneemut == 2013 ~ 4,
                                  anneemut == 2014 ~ 5,
                                  anneemut == 2015 ~ 6,
                                  anneemut == 2016 ~ 7,
                                  anneemut == 2017 ~ 8,
                                  anneemut == 2018 ~ 9,
                                  anneemut == 2019 ~ 10,
                                  anneemut == 2020 ~ 11)) %>% 
  select(price_m2, libnatmut, nblocmai, nblocapt, ffancstmin_dif, ffancstmax_dif, libtypbien,
         arr, iris_code, treatment, linear_trend, wave_fe, wave, iris_code,
         colnames(stacked1)[c(46:ncol(stacked1))])

  
# Stacked formula following Brehm et al. (2022)
dv3f_stacked_formula <- as.formula(paste("price_m2 ~ ", paste(colnames(stacked1)[c(10, 2:9, 12:391)],
                                                          collapse = "+")))

dv3f_stacked_did <- lm(dv3f_stacked_formula,
                              data = stacked1)

did_p <- lm(price_m2 ~ fe_20202019 + fe_20202016,
            data = stacked1)


summary(dv3f_stacked_did)

# Stacked formula with  ET AL. FE

# Create a linear trend

dv3f_stacked_formula2 <- as.formula(paste("price_m2 ~ ", paste(colnames(stacked1)[c(9, 2:8, 10, 432:ncol(stacked1))],
                                                              collapse = "+")))

dv3f_stacked_did2 <- lm(dv3f_stacked_formula2,
                       data = stacked1)


summary(dv3f_stacked_did2)


did



########################### Import both parts of the data (accidents)
# With one lag
stacked1_acc <- read_csv("accidents/stacked/accidents_stacked_step1.csv")

stacked2_acc <- read_csv("accidents/stacked/accidents_stacked_step2.csv")


stacked_acc <- cbind(stacked1_acc, stacked2_acc[,19:ncol(stacked2_acc)])

stacked_acc <- stacked_acc %>% 
  filter(!is.na(iris_code))

# Format
stacked_acc <- stacked_acc %>% 
  mutate(wave_fe = as.character(wave_fe),
         iris_fe = as.character(iris_fe))

# With three lags & new outcomes
stacked1_acc_3 <- read_csv("accidents/stacked/accidents_stacked_step1_w_3.csv")

stacked2_acc_3 <- read_csv("accidents/stacked/accidents_stacked_step2_w_3.csv")

stacked_acc_3 <- cbind(stacked1_acc_3, stacked2_acc_3[,22:ncol(stacked2_acc_3)])

stacked_acc_3 <- stacked_acc_3 %>% 
  filter(!is.na(iris_code))

# Format
stacked_acc_3 <- stacked_acc_3 %>% 
  mutate(wave_fe = as.character(wave_fe),
         iris_fe = as.character(iris_fe))




###### with full sample
stacked_acc <- read_csv("accidents/stacked/accidents_stacked_full_period.csv")

## add IRIS-level controls
stacked_acc <- stacked_acc %>% 
  left_join(iris %>% 
              select(iris_code,pop, mob_voit1,dwe_prop,com_mar,
                     com_voit, com_tcom, median), by = "iris_code")



# Grouped version of accidents for the standard DiD procedure
acc_g <- read_csv("accidents/grouped/accidents_grouped.csv")

# IRIS level pre-treatment controls
iris <- read_csv("delineation/iris_characteristics_full_data.csv")

acc_g <- acc_g %>% 
  left_join(iris %>% 
              select(iris_code,pop, mob_voit1,dwe_prop,com_mar,
                     com_voit, com_tcom, median), by = "iris_code")



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

#### STANDARD DID

# Try to remove 2011, as using the bacon stuff, it looks like it is the 
# one driving positive results and there is so little variation it may be problematic
acc_g_no11 <- acc_g %>% 
  filter(earliest_year != 2011 | is.na(earliest_year))

acc_did <- lm_robust(n_accidents ~ effective_treatment,
               data = acc_g_no11,
               fixed_effects = ~ iris_code + an,
               clusters = iris_code)

summary(acc_did)


# Remember these elements are defined later on, so I have to go back and forth in the code
# to make sure everything works
iris_years_no11 <- iris_years %>% 
  filter(earliest_year != 2011 | is.na(earliest_year))

# Bacon unconditional
acc_did_bacon_no11 <- bacon(n_accidents ~ effective_treatment,
                       data = iris_years_no11,
                       id_var = "iris_code",
                       time_var = "an")

acc_did_bacon_uncond <- bacon(n_accidents ~ effective_treatment,
                            data = iris_years,
                            id_var = "iris_code",
                            time_var = "an")

ggplot(acc_did_bacon_no11) +
  aes(x = weight, y = estimate, shape = factor(type), color = factor(type)) +
  labs(x = "Weight", y = "Estimate", shape = "Type", color = "Type") +
  geom_point()  + theme_bw() +
  geom_hline(yintercept = 0) + geom_hline(yintercept = -0.67, linetype = "dashed")


############ STUDYING THE GOODMAN BACON STUFF
acc_did_bacon_no11_g <- acc_did_bacon_no11 %>% 
  group_by(type) %>% 
  mutate(weight_sum = sum(weight)) %>% 
  summarise(estimate = weighted.mean(estimate, weight/weight_sum),
            weight_sums = sum(weight))

acc_did_bacon_no11_g <- acc_did_bacon_no11_g %>% 
  group_by(type) %>% 
  summarise(estimate = weighted.mean(estimate, weight/weight_sum),
            weight_sums = sum(weight))


# Now, try standard did without 2011 and controls
acc_g_no11 <- acc_g_no11 %>% 
  mutate(an_fe = as.character(an),
         an = as.numeric(an),
         iris_fe = as.character(iris_code))

std_did_no11 <- lm(n_accidents ~ effective_treatment + pop + mob_voit1 + dwe_prop +
                           com_mar + com_voit + com_tcom + median +
                           arr + iris_fe,
                          data = acc_g_no11)

summary(std_did_no11)


# Goodman-Bacon decomposition

lapply(acc_g, function(x) sum(is.na(x)))

## Add the non-available observations to make a balanced panel
# Generate all possible combinations
iris_years <- as_tibble(apply(expand.grid(unique(iris$iris_code), as.character(c(2005:2020))),
                              1,
                              paste,
                              collapse=".")) %>% 
  rename(id = value) %>% 
  arrange(id) %>% 
  mutate(iris_code = as.numeric(str_sub(id,
                             1,
                             9)),
         an = as.numeric(str_sub(id,
                        11,
                        str_length(id))))


iris_years <- iris_years %>% 
  select(-id) %>% 
  left_join(acc_g, by = c("iris_code", "an"))

# Add treatment status
iris_treat <- st_read("zones_30/iris_treatment_grouped.geojson") %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  mutate(treat = 1) %>% 
  rename(iris_code = iris) %>% 
  mutate(iris_code = as.numeric(iris_code))

# Join with treatment
iris_years <- iris_years %>% 
  left_join(iris_treat %>% 
              select(iris_code, treat), by = "iris_code")

# As some IRIS which are included but have no data for accidents are treated and we impute them to be
# zero by construction, I need to do the following
iris_years <- iris_years %>% 
  group_by(iris_code) %>% 
  mutate(earliest_year = mean(earliest_year, na.rm = TRUE))

iris_years <- iris_years %>% 
  mutate(n_accidents = ifelse(is.na(n_accidents),
                              0,
                              n_accidents),
         effective_treatment = ifelse(is.na(treat),
                            0,
                            ifelse(!is.na(treat) & treat == 1 & !is.na(earliest_year) & earliest_year <= an,
                                   1,
                                   ifelse(!is.na(treat) & treat == 1 & !is.na(earliest_year) & earliest_year > an,
                                          0,
                                          0))))

# Impute controls
iris_years <- iris_years %>% 
  left_join(iris %>% 
               select(iris_code, pop, mob_voit1, dwe_prop, com_mar,
                      com_voit, com_tcom, median), by = "iris_code")

iris_years <- iris_years %>% 
  mutate(median.y = ifelse(is.na(median.y),
                         mean(iris_years$median.y, na.rm = TRUE),
                         median.y))

lapply(iris_years, function(x) sum(is.na(x)))


# Bacon conditional
acc_did_bacon_cond <- bacon(n_accidents ~ effective_treatment + pop.y + mob_voit1.y + dwe_prop.y +
                       com_mar.y + com_voit.y + com_tcom.y + median.y,
                       data = iris_years,
                       id_var = "iris_code",
                       time_var = "an")

# Bacon unconditional
acc_did_bacon <- bacon(n_accidents ~ effective_treatment,
                       data = iris_years,
                       id_var = "iris_code",
                       time_var = "an")

ggplot(acc_did_bacon$two_by_twos) +
  aes(x = weight, y = estimate, shape = factor(type), color = factor(type)) +
  labs(x = "Weight", y = "Estimate", shape = "Type", color = "Type") +
  geom_point()  + theme_bw() +
  geom_hline(yintercept = 0) + geom_hline(yintercept = -0.67, linetype = "dashed")




### TRY TO RUN A SIMPLE REGRESSION
formula_acc_3 <- as.formula(paste("n_accident ~ ",
                                paste(c("treated",  "treated*n_accidents", "n_accidents","wave_fe","arr",
                                        colnames(stacked_acc_3)[22:ncol(stacked_acc_3)]),
                                      collapse = "+")))



acc_ols <- lm(formula_acc_3,
              data = stacked_acc_3)

summary(acc_ols)

####### STACKED DID


# N. of accidents

# Basics
f_did1 <- as.formula(paste("n_accidents ~ ",
                               paste(c("treated",
                                       colnames(stacked_acc)[23:195]),
                                     collapse = "+")))

did1 <- lm_robust(f_did1,
                      data = stacked_acc,
                      clusters = iris_code)

summary(did1)

# Basics + arr_time
f_did1 <- as.formula(paste("n_accidents ~ ",
                           paste(c("treated",
                                   colnames(stacked_acc)[23:515]),
                                 collapse = "+")))

did1 <- lm_robust(f_did1,
                  data = stacked_acc,
                  clusters = iris_code)

summary(did1)

# Basics + arr_time + IRIS-level controls
f_did1 <- as.formula(paste("n_accidents ~ ",
                           paste(c("treated",
                                   colnames(stacked_acc)[23:ncol(stacked_acc)]),
                                 collapse = "+")))

did1 <- lm_robust(f_did1,
                  data = stacked_acc,
                  clusters = iris_code)

summary(did1)

# N. of victims per accidents
stacked_acc <- stacked_acc %>% 
  mutate(total_victims_per_accident = total_victims/n_accidents)

# Basics
f_did1 <- as.formula(paste("total_victims_per_accident ~ ",
                           paste(c("treated",
                                   colnames(stacked_acc)[23:195]),
                                 collapse = "+")))

did1 <- lm_robust(f_did1,
                  data = stacked_acc,
                  clusters = iris_code)

summary(did1)

# Basics + arr_time
f_did1 <- as.formula(paste("total_victims_per_accident ~ ",
                           paste(c("treated",
                                   colnames(stacked_acc)[23:515]),
                                 collapse = "+")))

did1 <- lm_robust(f_did1,
                  data = stacked_acc,
                  clusters = iris_code)

summary(did1)

# Basics + arr_time + IRIS-level controls
f_did1 <- as.formula(paste("total_victims_per_accident ~ ",
                           paste(c("treated",
                                   colnames(stacked_acc)[23:(ncol(stacked_acc) - 1)]),
                                 collapse = "+")))

did1 <- lm_robust(f_did1,
                  data = stacked_acc,
                  clusters = iris_code)

summary(did1)


# Share of dead per accident
stacked_acc <- stacked_acc %>% 
  mutate(dead_per_accident = n_dead/n_accidents)

# Basics
f_did1 <- as.formula(paste("dead_per_accident ~ ",
                           paste(c("treated",
                                   colnames(stacked_acc)[23:195]),
                                 collapse = "+")))

did1 <- lm_robust(f_did1,
                  data = stacked_acc,
                  clusters = iris_code)

summary(did1)

# Basics + arr_time
f_did1 <- as.formula(paste("dead_per_accident ~ ",
                           paste(c("treated",
                                   colnames(stacked_acc)[23:515]),
                                 collapse = "+")))

did1 <- lm_robust(f_did1,
                  data = stacked_acc,
                  clusters = iris_code)

summary(did1)

# Basics + arr_time + IRIS-level controls
f_did1 <- as.formula(paste("dead_per_accident ~ ",
                           paste(c("treated",
                                   colnames(stacked_acc)[23:(ncol(stacked_acc) - 2)]),
                                 collapse = "+")))

did1 <- lm_robust(f_did1,
                  data = stacked_acc,
                  clusters = iris_code)

summary(did1)



# Share of accidents involving bikes
stacked_acc <- stacked_acc %>% 
  mutate(iris_fe = as.character(iris_fe))

# Basics
f_did1 <- as.formula(paste("share_bike ~ ",
                           paste(c("treated",
                                   colnames(stacked_acc)[23:195]),
                                 collapse = "+")))

did1 <- lm_robust(f_did1,
                  data = stacked_acc,
                  clusters = iris_code)

summary(did1)

# Basics + arr_time
f_did1 <- as.formula(paste("share_bike ~ ",
                           paste(c("treated",
                                   colnames(stacked_acc)[23:515]),
                                 collapse = "+")))

did1 <- lm_robust(f_did1,
                  data = stacked_acc,
                  clusters = iris_code)

summary(did1)

# Basics + arr_time + IRIS-level controls
f_did1 <- as.formula(paste("share_bike ~ ",
                           paste(c("treated",
                                   colnames(stacked_acc)[23:(ncol(stacked_acc) - 2)]),
                                 collapse = "+")))

did1 <- lm_robust(f_did1,
                  data = stacked_acc,
                  clusters = iris_code)

summary(did1)




###################### ACCIDENT DATA

# Import all the iris data in a loop for all years:
for (i in 6:20) {
  if(i < 10){
    assign(sprintf("iris%s",
                   paste0("0",
                          as.character(i))),
           read_csv(sprintf("iris/data/20%s/paris/paris%s.csv",
                            paste0("0",
                                   as.character(i)),
                            paste0("0",
                                   as.character(i)))))
  }
  else{
    assign(sprintf("iris%s",
                   as.character(i)),
           read_csv(sprintf("iris/data/20%s/paris/paris%s.csv",
                            as.character(i),
                            as.character(i))))
  }
}


iris <- iris06 %>% 
  left_join(iris07, by = "IRIS") %>% 
  left_join(iris08, by = "IRIS") %>% 
  left_join(iris09, by = "IRIS") %>% 
  left_join(iris10, by = "IRIS") %>% 
  left_join(iris11, by = "IRIS") %>% 
  left_join(iris12, by = "IRIS") %>% 
  left_join(iris13, by = "IRIS") %>% 
  left_join(iris14, by = "IRIS") %>% 
  left_join(iris15, by = "IRIS") %>% 
  left_join(iris16, by = "IRIS") %>% 
  left_join(iris17, by = "IRIS") %>% 
  left_join(iris18, by = "IRIS") %>% 
  left_join(iris19, by = "IRIS")

# Now, subset the relevant variables

# pop, mob_voit1, dwe_prop, com_mar,
# com_tcom, median. CONTROLS FOR ACCIDENTS: IT IS DONE HERE

iris1 <- iris %>% 
  select(IRIS, ends_with("voit1"),
         ends_with("POP"),
         ends_with("RP_PROP"),
         starts_with("RFMQ2"),
         starts_with("DEC_MED"),
         ends_with("P_TCOM"),
         ends_with("P_VOIT"),
         ends_with("P_MAR"))

iris1 <- iris1 %>% 
  select(-c(contains("NPER"), contains("ANEM")))

iris_long <- tibble()
voit1 <- c()
# Now, create a new dataframe to store them in a long position
for (i in 1:992) {
  voit1 <- append(voit1, iris1 %>% 
    slice(i) %>% 
    select(ends_with("voit1")) %>% 
    `colnames<-`(c(2006:2019)) %>% 
    t()
  )
}

pop <- c()
for (i in 1:992) {
  pop <- append(pop, iris1 %>% 
                    slice(i) %>% 
                    select(ends_with("POP")) %>% 
                    `colnames<-`(c(2006:2019)) %>% 
                    t()
  )
}

prop <- c()
for (i in 1:992) {
  prop <- append(prop, iris1 %>% 
                  slice(i) %>% 
                  select(ends_with("PROP")) %>% 
                  `colnames<-`(c(2006:2019)) %>% 
                  t()
  )
}

median <- c()
for (i in 1:992) {
  median <- append(median, iris1 %>% 
                   slice(i) %>% 
                   select(starts_with("RFMQ2"),
                          starts_with("DEC_MED")) %>% 
                   `colnames<-`(c(2006:2019)) %>% 
                   t()
  )
}

tcom <- c()
for (i in 1:992) {
  tcom <- append(tcom, iris1 %>% 
                     slice(i) %>% 
                     select(ends_with("P_TCOM")) %>% 
                     `colnames<-`(c(2006:2019)) %>% 
                     t()
  )
}

voit <- c()
for (i in 1:992) {
  voit <- append(voit, iris1 %>% 
                     slice(i) %>% 
                     select(ends_with("P_VOIT")) %>% 
                     `colnames<-`(c(2006:2019)) %>% 
                     t()
  )
}

mar <- c()
for (i in 1:992) {
  mar <- append(mar, iris1 %>% 
                     slice(i) %>% 
                     select(ends_with("P_MAR")) %>% 
                     `colnames<-`(c(2006:2019)) %>% 
                     t()
  )
}

# Generate all possible combinations of IRIS-YEARS
iris_years <- as_tibble(apply(expand.grid(unique(iris$IRIS), as.character(c(2006:2019))),
                              1,
                              paste,
                              collapse=".")) %>% 
  rename(id = value) %>% 
  arrange(id) %>% 
  mutate(iris_code = str_sub(id,
                         1,
                         9),
         year = str_sub(id,
                        11,
                        14))

# Add the variables
iris_years <- iris_years %>% 
  mutate(pop = pop,
         median = median,
         com_mar = mar,
         com_voit = voit,
         com_tcom = tcom,
         dwe_prop = prop,
         mob_voit1 = voit1)

# Check for missing values (some missing values but it's okay)
lapply(iris_years, function(x) sum(is.na(x)))

# Now, save it
write_csv(iris_years, "iris/iris_controls_accidents.csv")
