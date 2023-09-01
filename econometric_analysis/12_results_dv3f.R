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
library(stargazer) # tables
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


#### DV3F ####################################################
###### USING the -2, +2 time window
stacked <- read_csv("dv3f_new_variables/stacked/dv3f_stacked_2_2_window.csv") %>% 
  mutate(iris_fe = as.character(iris_fe),
         anneemut = as.character(anneemut),
         arr = as.character(arr))

# Import DV3F normal data
dv3f <- read_csv("dv3f_new_variables/dv3f_clean_no_outliers.csv")

dv3f <- dv3f %>% 
  filter(!(slow_zone_d == 1 & is.na(slow_zone_year))) # well-subsetted

# Also, slow zones implemented in 2007 and 2008 need to be changed to NA
dv3f <- dv3f %>% 
  mutate(slow_zone_year = case_when(slow_zone_year == 2007 | slow_zone_year == 2008 ~ NA,
                                    slow_zone_year == slow_zone_year ~ slow_zone_year))


dv3f <- dv3f %>% 
  mutate(ffancstmin_dif = 2019 - ffancstmin,
         ffancstmax_dif = 2019 - ffancstmax,
         arr = as.character(arr),
         iris_fe = as.character(iris_code),
         treatment = ifelse(slow_zone_d == 0,
                            0,
                            ifelse(slow_zone_d == 1 & anneemut >= slow_zone_year,
                                          1,
                                          0)),
         anneemut = as.character(anneemut)) %>%
  mutate(log_price_m2 = log(price_m2),
         .after = price_m2) %>% 
  select(log_price_m2, price_m2,  treatment, libnatmut, nblocmai, nblocapt,
         ffancstmin_dif, ffancstmax_dif, libtypbien, arr, anneemut,
         iris_fe, slow_zone_year, slow_zone_d)


## ADD IRIS CONTROLS TO BOTH
iris_control <- read_csv("iris/iris_controls_accidents.csv") %>% 
  rename(iris_fe = iris_code,
         anneemut = year) %>% 
  mutate(iris_fe = as.character(iris_fe),
         anneemut = as.character(anneemut)) 


# Add to standard
dv3f <- dv3f %>% 
  left_join(iris_control %>% 
              select(-id), by = c("iris_fe", "anneemut"))

# Add to stacked
stacked <- stacked %>% 
  left_join(iris_control %>% 
              select(-id), by = c("iris_fe", "anneemut"))



rm(iris_control)


################# BEFORE I START DOING ANY ESTIMATIO PROCEDURE,
#### I NEED TO JUSTIFY WHICH ONE OF THE FOLLOWING HOLDS:
#### PARALLEL TREND on LOGS or in LEVELS
dv3f_g <- dv3f %>% 
  group_by(slow_zone_d, anneemut) %>% 
  summarise(price_m2 = mean(price_m2),
            log_price_m2 = mean(log_price_m2),
            n_transactions = n()) %>% 
  mutate(anneemut = as.numeric(anneemut),
         slow_zone_d = as.character(slow_zone_d))

# Plot

dv3f_g <- dv3f_g %>% 
  mutate(slow_zone = ifelse(slow_zone_d == "0",
                            "Outside LSZ",
                            "In area of (future) LSZ"))

# Plotting the parallel trends
p1 <- ggplot(dv3f_g) + 
  geom_line(aes(x = anneemut, y = price_m2, color = slow_zone)) +
  labs(x = "Year",
       y = "Average for each year",
       color = "") +
  scale_x_continuous(limits = c(2010, 2020), breaks = seq(2010, 2020, by = 1)) +
  theme_bw() +
  scale_color_manual(values = c("darkgreen", "lightgreen")) + 
  guides(color = "none") + 
  ggtitle("Price per square meter") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p1
p2 <- ggplot(dv3f_g) + 
  geom_line(aes(x = anneemut, y = log_price_m2, color = slow_zone)) +
  labs(x = "Year",
       y = "",
       color = "") +
  scale_x_continuous(limits = c(2010, 2020), breaks = seq(2010, 2020, by = 1)) +
  theme_bw() +
  scale_color_manual(values = c("darkgreen", "lightgreen")) + 
  ggtitle("Log. of price per square meter") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p2


ggarrange(p1, p2, ncol = 2, common.legend = TRUE, legend="bottom")
                                                                                                         

###### STANDARD TWFE DID
# Controls

formula_twfe_1 <- as.formula(paste("price_m2 ~ ",
                                   paste(colnames(dv3f[c(3, 4:12, 15:ncol(dv3f))]),
                                         collapse = "+")))

twfe_did_1 <- lm(formula_twfe_1,
                 data = dv3f)

coeftest(twfe_did_1, vcov = vcovCL, cluster = ~ iris_fe)


# no controls

formula_twfe_2 <- as.formula(paste("price_m2 ~ ",
                                   paste(colnames(dv3f[c(3, 4:12)]),
                                                                  collapse = "+")))

twfe_did_2 <- lm(formula_twfe_2,
                 data = dv3f)

coeftest(twfe_did_2, vcov = vcovCL, cluster = ~ iris_fe)
                             

# NO 2010
dv3f_no10 <- dv3f %>% 
  filter(anneemut != 2010)


formula_twfe_3 <- as.formula(paste("price_m2 ~ ",
                                   paste(colnames(dv3f[c(3, 4:12, 15:ncol(dv3f))]),
                                         collapse = "+")))

twfe_did_3 <- lm(formula_twfe_3,
                 data = dv3f_no10)

coeftest(twfe_did_3, vcov = vcovCL, cluster = ~ iris_fe)


# no controls

formula_twfe_4 <- as.formula(paste("price_m2 ~ ",
                                   paste(colnames(dv3f[c(3, 4:12)]),
                                         collapse = "+")))

twfe_did_4 <- lm(formula_twfe_4,
                 data = dv3f_no10)

coeftest(twfe_did_4, vcov = vcovCL, cluster = ~ iris_fe)


####### STACKED DID


# No controls
formula_stk_1 <- as.formula(paste("price_m2 ~ ",
                                  paste(colnames(stacked)[c(17, 4:10, 12, 19:(ncol(stacked) - 7))],
                                        collapse = "+")))

stk_did_1 <- lm(formula_stk_1,
                data = stacked)

# Clustered SE
coeftest(stk_did_1, vcov = vcovCL, cluster = ~ iris_fe)



# No controls
formula_stk_2 <- as.formula(paste("price_m2 ~ ",
                                  paste(colnames(stacked)[c(17, 4:10, 12, 19:(ncol(stacked)))],
                                        collapse = "+")))


stk_did_2 <- lm(formula_stk_2,
                data = stacked)

# Clustered SE
coeftest(stk_did_2, vcov = vcovCL, cluster = ~ iris_fe)


#### NOW, WITHOUT 2010
stacked_no10 <- stacked %>% 
  filter(wave_fe != "2010")

# No controls
formula_stk_1 <- as.formula(paste("price_m2 ~ ",
                                  paste(colnames(stacked)[c(17, 4:10, 12, 19:(ncol(stacked) - 7))],
                                        collapse = "+")))

stk_did_3 <- lm(formula_stk_1,
                data = stacked_no10)

# Clustered SE
coeftest(stk_did_3, vcov = vcovCL, cluster = ~ iris_fe)



# No controls
formula_stk_2 <- as.formula(paste("price_m2 ~ ",
                                  paste(colnames(stacked)[c(17, 4:10, 12, 19:(ncol(stacked)))],
                                        collapse = "+")))


stk_did_4 <- lm(formula_stk_2,
                data = stacked_no10)

# Clustered SE
coeftest(stk_did_4, vcov = vcovCL, cluster = ~ iris_fe)



#######            EVENT STUDY            ##############



