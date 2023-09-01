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
library(MASS) # negative binomial

# memory problems
library(usethis) 
usethis::edit_r_environ()

# Set working directory
setwd("/Users/unaioyon/Desktop/masters_thesis/data/fra")

options(scipen = 999)



################################## IMPORT THE DATA

# Stacked version with full sample
stacked_acc <- read_csv("accidents/stacked/accidents_stacked_full_period.csv")

# Grouped version of accidents for the standard DiD procedure
acc_g <- read_csv("accidents/grouped/accidents_grouped.csv")



# IRIS controls
iris_acc <- read_csv("iris/iris_controls_accidents.csv") %>% 
  rename(an = year)



##################################
#### 1. STANDARD DIFF IN DIFF ####
##################################


# Define treatment correctly
acc_g <- acc_g %>% 
  mutate(effective_treatment = ifelse(treatment == 1 & an >= earliest_year & !is.na(earliest_year),
                                      1,
                                      0),
         arr = str_sub(iris_code,
                       4,
                       5),
         year_fe = as.character(an)) %>% 
  filter(!is.na(iris_code)) %>% 
  rename(treat_location = treatment) %>% 
  relocate(effective_treatment, treat_location, earliest_year,
           .after = iris_code)

# Add controls
acc_g <- acc_g %>% 
  left_join(iris_acc, by = c("iris_code", "an"))

# By adding controls, some missing values are introduced: keep that in mind for Bacon!


###### GOODMAN-BACON

# Choose the balanced panel:
iris_list <- unique(iris_acc$iris_code)

iris_years <- as_tibble(apply(expand.grid(iris_list, as.character(c(2006:2019))),
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
                        14)))

# JOIN
iris_balanced <- iris_years %>% 
  select(-id) %>% 
  left_join(acc_g, by =  c("iris_code", "an"))

# Add treatment status to those treated zone iris-years which had no accidents
iris_treat <- st_read("zones_30/iris_treatment_grouped.geojson") %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  mutate(treat = 1) %>% 
  rename(iris_code = iris) %>% 
  mutate(iris_code = as.numeric(iris_code))
# Join with treatment
iris_balanced <- iris_balanced %>% 
  left_join(iris_treat %>% 
              select(iris_code, treat), by = "iris_code")

# As some IRIS which are included but have no data for accidents are treated and we impute them to be
# zero by construction, I need to do the following
iris_balanced <- iris_balanced %>% 
  group_by(iris_code) %>% 
  mutate(earliest_year = mean(earliest_year, na.rm = TRUE))


iris_balanced <- iris_balanced %>% 
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

iris_balanced <- iris_balanced %>% 
  group_by(iris_code) %>% 
  mutate(n_accidents_imp = ifelse(is.na(n_accidents),
                                  mean(n_accidents, na.rm = TRUE),
                                  n_accidents))


######################

# Now, set MISSING VALUES for controls
mean_pop <- mean(iris_balanced$pop, na.rm = TRUE)
mean_dwe_prop <- mean(iris_balanced$dwe_prop, na.rm = TRUE)
mean_com_mar <- mean(iris_balanced$com_mar, na.rm = TRUE)
mean_com_voit <- mean(iris_balanced$com_voit, na.rm = TRUE)
mean_com_tcom <- mean(iris_balanced$com_tcom, na.rm = TRUE)
mean_mob_voit1 <- mean(iris_balanced$mob_voit1, na.rm = TRUE)
mean_median <- mean(iris_balanced$median, na.rm = TRUE)

iris_balanced <- iris_balanced %>% 
  mutate(pop = ifelse(is.na(pop),
                      mean_pop,
                      pop),
         mob_voit1 = ifelse(is.na(mob_voit1),
                      mean_mob_voit1,
                      mob_voit1),
         median = ifelse(is.na(median),
                      mean_median,
                      median),
         dwe_prop = ifelse(is.na(dwe_prop),
                      mean_dwe_prop,
                      dwe_prop),
         com_mar = ifelse(is.na(com_mar),
                      mean_com_mar,
                      com_mar),
         com_voit = ifelse(is.na(com_voit),
                      mean_com_voit,
                      com_voit),
         com_tcom = ifelse(is.na(com_tcom),
                      mean_com_tcom,
                      com_tcom))

rm(list = c("acc"))

# STANDARD DID ######################################################################
acc_g <- acc_g %>% 
  filter(earliest_year != 2011 | is.na(earliest_year)) %>% 
  mutate(iris_fe = as.character(iris_code))

acc_g <- acc_g %>% 
  mutate(iris_fe = as.character(iris_code))


# DIDs
# Controls
acc_g <- acc_g %>% 
  mutate(severe_victims_per_accident = (n_dead + n_hospital)/n_accidents,
         severe_victims = n_dead + n_hospital)
  
####### DIDS HERE
did1 <- lm(share_bike ~ effective_treatment + pop + mob_voit1 + median + dwe_prop +
             com_mar + com_voit + com_tcom + arr + year_fe + iris_fe,
           data = acc_g)

# Clustered SE
coeftest(did1, vcov = vcovCL, cluster = ~ iris_code)

summary(did1)

did2 <- lm_robust(n_accidents ~ effective_treatment + arr,
                  fixed_effects = ~ iris_code + an,
                  data = acc_g)

did3 <- lm(n_accidents ~ effective_treatment + pop + mob_voit1 + dwe_prop +
             com_mar + com_voit + com_tcom + median +
             arr + iris_fe + year_fe,
           data = acc_g)

summary(did3)

# GOODMAN-BACON ##################################################################
iris_balanced <- iris_balanced %>% 
  filter(earliest_year != 2011 | is.na(earliest_year))

# Comparing with iris_years from 10_:::.R

iris_balanced1 <- iris_balanced %>% 
  select(n_accidents_imp, effective_treatment, iris_code, an, pop,
           median, com_mar, com_voit, com_tcom, 
           dwe_prop, mob_voit1)

bacon_no11 <- bacon(n_accidents_imp ~ effective_treatment,
                    data = iris_balanced1,
                    id_var = "iris_code",
                    time_var = "an")



bacon_no11_controls <- bacon(n_accidents_imp ~ effective_treatment + pop  +
                               median + com_mar + com_voit + com_tcom +
                               dwe_prop + mob_voit1,
                             data = iris_balanced1,
                             id_var = "iris_code",
                             time_var = "an")

# Visualize the decomposition
ggplot(bacon_no11) +
  aes(x = weight, y = estimate, shape = factor(type), color = factor(type)) +
  labs(x = "Weight", y = "Estimate", shape = "Type", color = "Type") +
  geom_point()  + theme_bw() +
  geom_hline(yintercept = 0) + geom_hline(yintercept = -0.72, linetype = "dashed")


# Add a within version of it and adjust the weights
bacon_no_11_controls_df <- bacon_no11_controls$two_by_twos
bacon_no_11_controls_df_within <- tibble(weight = bacon_no11_controls$Omega,
                                         estimate = bacon_no11_controls$beta_hat_w)

bacon_no_11_controls_df <- bacon_no_11_controls_df %>% 
  bind_rows(bacon_no_11_controls_df_within)

rm(bacon_no_11_controls_df_within)

# Reweight
bacon_no_11_controls_df <- bacon_no_11_controls_df %>% 
  mutate(weight_scaled = weight/sum(weight)) %>% 
  mutate(type = ifelse(is.na(type),
                       "Within",
                       ifelse(type == "Both Treated",
                              "Earlier vs. Later & Later vs. Earlier",
                              type)))


ggplot(bacon_no_11_controls_df) +
  aes(x = weight, y = estimate, shape = factor(type), color = factor(type)) +
  labs(x = "Weight", y = "Estimate", shape = "Type", color = "Type") +
  geom_point()  + theme_bw() +
  geom_hline(yintercept = 0) + geom_hline(yintercept = -0.505, linetype = "dashed")


# Summarizing the goodman-bacon stuff
bacon_no11_g <- bacon_no11 %>% 
  group_by(type) %>% 
  mutate(weight_sum = sum(weight)) %>% 
  summarise(estimate = weighted.mean(estimate, weight/weight_sum),
            weight_sums = sum(weight))




iris_balanced1_2 <- iris_balanced  %>% 
  filter(earliest_year != 2015 | is.na(earliest_year)) %>% 
  select(n_accidents_imp, effective_treatment, iris_code, an)

bacon_no11_15 <- bacon(n_accidents_imp ~ effective_treatment,
                    data = iris_balanced1_2,
                    id_var = "iris_code",
                    time_var = "an")

ggplot(bacon_no11_15) +
  aes(x = weight, y = estimate, shape = factor(type), color = factor(type)) +
  labs(x = "Weight", y = "Estimate", shape = "Type", color = "Type") +
  geom_point()  + theme_bw() +
  geom_hline(yintercept = 0) + geom_hline(yintercept = -0.37, linetype = "dashed")

bacon_no11_old <- bacon(n_accidents ~ effective_treatment,
                       data = iris_years,
                       id_var = "iris_code",
                       time_var = "an")














# STACKED DID ######################################################################


# Add controls
stacked_acc <- stacked_acc %>% 
  dplyr::select(1:515) %>% 
  left_join(iris_acc %>% 
              dplyr::select(-id, an), by = c("iris_code", "an")) %>% 
  mutate(iris_fe = as.character(iris_fe))

# Create the new outcomes
stacked_acc <- stacked_acc %>% 
  mutate(total_victims_per_accident = total_victims/n_accidents,
         dead_per_accident = n_dead/n_accidents,
         severe_victims_per_accident = (n_dead + n_hospital)/n_accidents,
         severe_victims = n_dead + n_hospital,
         n_bike = share_bike * n_accidents,
         n_non_bike = n_accidents - n_bike)


# n accidents
f_did1 <- as.formula(paste("n_accidents ~ ",
                           paste(c("treated",
                                   colnames(stacked_acc)[c(23, 516:(ncol(stacked_acc)-6), 24:(ncol(stacked_acc)-13))]),
                                 collapse = "+")))

f_did2 <- as.formula(paste("total_victims ~ ",
                           paste(c("treated",
                                   colnames(stacked_acc)[c(23, 516:(ncol(stacked_acc)-6), 24:(ncol(stacked_acc)-13))]),
                                 collapse = "+")))

f_did3 <- as.formula(paste("severe_victims ~ ",
                           paste(c("treated",
                                   colnames(stacked_acc)[c(23, 516:(ncol(stacked_acc)-6), 24:(ncol(stacked_acc)-13))]),
                                 collapse = "+")))

f_did4 <- as.formula(paste("n_dead ~ ",
                           paste(c("treated",
                                   colnames(stacked_acc)[c(23, 516:(ncol(stacked_acc)-6), 24:(ncol(stacked_acc)-13))]),
                                 collapse = "+")))



# COUNT DATA ######################################################################

#make this example reproducible
set.seed(1)

#create dataset
data <- data.frame(offers = c(rep(0, 700), rep(1, 100), rep(2, 100),
                              rep(3, 70), rep(4, 30)),
                   division = sample(c('A', 'B', 'C'), 100, replace = TRUE),
                   exam = c(runif(700, 60, 90), runif(100, 65, 95),
                            runif(200, 75, 95)))

#view first six rows of dataset
head(data)

#fit Poisson regression model
p_model <- glm(offers ~ division + exam, family = 'poisson', data = data)


st <- stacked_acc %>% 
  slice(1:50) %>% 
  mutate(n_accidents = ifelse(n_accidents > 18,
                              0,
                              n_accidents))
# N. of accidents
poi1 <- glm(f_did1,
           data = stacked_acc,
           family = "poisson")

nb1 <- glm.nb(f_did1,
            data = stacked_acc)

# Clustered SE
st11 <- coeftest(poi1, vcov = vcovCL, cluster = ~ iris_code)
st12 <- coeftest(nb1, vcov = vcovCL, cluster = ~ iris_code)



# Study which model is better
#Residual plot for Poisson regression
p_res <- resid(poi1)
plot(fitted(poi1), p_res, col='steelblue', pch=16,
     xlab='Predicted Offers', ylab='Standardized Residuals', main='Poisson')
abline(0,0)

poi1_df <- tibble(residuals = p_res,
                 predicted = fitted(poi1))

poi1_gg <- ggplot(poi1_df) + 
  geom_point(aes(predicted, residuals), color = "darkgreen",
             fill = "green", pch = 21) + 
  geom_hline(yintercept = 0) + theme_bw() + 
  labs(x = "Predicted Values", y = "Standardized Residuals") + 
  ggtitle("Poisson regression") + 
  scale_y_continuous(limits = c(-7, 8))

poi1_gg


nb1_df <- tibble(residuals = nb_res,
                  predicted = fitted(nb1))

nb1_gg <- ggplot(nb1_df) + 
  geom_point(aes(predicted, residuals), color = "darkgreen",
             fill = "green", pch = 21) + 
  geom_hline(yintercept = 0) + theme_bw() + 
  labs(x = "Predicted Values", y = "Standardized Residuals") + 
  ggtitle("Negative binomial regression") + 
  scale_y_continuous(limits = c(-7, 8))

nb1_gg


ggarrange(poi1_gg, nb1_gg)



## Plot the distribution of the number of accidents
set.seed(2022)  # for reproducibility 
pois_dist = tibble(pois = rpois(20000, 6.213328))
summary(pois_dist)


ggplot() + 
  geom_histogram(data = stacked_acc,
                 aes(x = n_accidents, y= after_stat(density)),
                 color = "darkgreen", fill = "lightgreen") + 
  geom_density(data = pois_dist, aes(x = pois), color = "darkgreen") + theme_bw()
  

mean(stacked_acc$n_accidents)
var(stacked_acc$n_accidents)

stacked_acc %>% 
  dplyr::filter(dplyr::between(n_accidents, 1, quantile(n_accidents, 0.98))) %>% 
  dplyr::select(n_accidents) %>% 
  var()

#Residual plot for negative binomial regression 
nb_res <- resid(nb1)
plot(fitted(nb1), nb_res, col='steelblue', pch=16,
     xlab='Predicted Offers', ylab='Standardized Residuals', main='Negative Binomial')
abline(0,0)

pchisq(2 * (logLik(nb1) - logLik(poi1)), df = 1, lower.tail = FALSE)


# N. of total victims
poi2 <- glm(f_did2,
            data = stacked_acc,
            family = "poisson")

nb2 <- glm.nb(f_did2,
              data = stacked_acc)

# Clustered SE
st21 <- coeftest(poi2, vcov = vcovCL, cluster = ~ iris_code)
st22 <- coeftest(nb2, vcov = vcovCL, cluster = ~ iris_code)

# N. of KSI victims
poi3 <- glm(f_did3,
            data = stacked_acc,
            family = "poisson")

nb3 <- glm.nb(f_did3,
              data = stacked_acc)

# Clustered SE
st31 <- coeftest(poi3, vcov = vcovCL, cluster = ~ iris_code)
st32 <- coeftest(nb3, vcov = vcovCL, cluster = ~ iris_code)

# N. of killed victims
poi4 <- glm(f_did4,
            data = stacked_acc,
            family = "poisson")

nb4 <- glm.nb(f_did4,
              data = stacked_acc)

# Clustered SE
st41 <- coeftest(poi4, vcov = vcovCL, cluster = ~ iris_code)
st42 <- coeftest(nb4, vcov = vcovCL, cluster = ~ iris_code)

summary(poi1)
summary(nb1)

summary(stacked_acc$treated)


# Normal
stk_did1 <- lm(f_did1,
               data = stacked_acc)

# Poisson

# Clustered SE
coeftest(stk_did1, vcov = vcovCL, cluster = ~ iris_code)


# Trimmed data
stacked_acc_trim <- stacked_acc %>% 
  filter(dplyr::between(total_victims, 0, quantile(total_victims, 0.98)))
# n accidents
f_did1 <- as.formula(paste("total_victims ~ ",
                           paste(c("treated",
                                   colnames(stacked_acc_trim)[c(23, 516:(ncol(stacked_acc_trim)-6), 24:(ncol(stacked_acc_trim)-13))]),
                                 collapse = "+")))

stk_did1 <- lm(f_did1,
               data = stacked_acc_trim)

# Clustered SE
coeftest(stk_did1, vcov = vcovCL, cluster = ~ iris_code)


###################### NORMALIZE ACCIDENT RESULTS BY PRE-TREATMENT MEANS (2006-09)
mean_acc <- mean(acc_g[acc_g$an %in% c(2006:2009),]$n_accidents, na.rm = TRUE)
mean_total <- mean(acc_g[acc_g$an %in% c(2006:2009),]$total_victims, na.rm = TRUE)
mean_dead <- mean(acc_g[acc_g$an %in% c(2006:2009),]$n_dead, na.rm = TRUE)
mean_severe <- mean(acc_g[acc_g$an %in% c(2006:2009),]$severe_victims, na.rm = TRUE)

mean_acc1 <- mean(acc_g$n_accidents, na.rm = TRUE)
mean_total1 <- mean(acc_g$total_victims, na.rm = TRUE)
mean_dead1 <- mean(acc_g$n_dead, na.rm = TRUE)
mean_severe1 <- mean(acc_g$severe_victims, na.rm = TRUE)




########################      EVENT STUDY       ######################

# Create indicators for each relative time. There are, at most, 8, as data
# is constrained to be in 2019 to have controls. Thus:

stacked_acc1 <- stacked_acc %>% 
  mutate(post_4 = ifelse(an == wave - 4,
                        1,
                        0),
         post_3 = ifelse(an == wave - 3,
                        1,
                        0),
         post_2 = ifelse(an == wave - 2,
                        1,
                        0),
         post_1 = ifelse(an == wave - 1,
                        1,
                        0),
         post0 = ifelse(an == wave,
                        1,
                        0),
         post1 = ifelse(an == wave + 1,
                        1,
                        0),
         post2 = ifelse(an == wave + 2,
                        1,
                        0),
         post3 = ifelse(an == wave + 3,
                        1,
                        0),
         post4 = ifelse(an == wave + 4,
                        1,
                        0),
         post5 = ifelse(an == wave + 5,
                        1,
                        0),
         post6 = ifelse(an == wave + 6,
                        1,
                        0),
         post7 = ifelse(an == wave + 7,
                        1,
                        0),
         post8 = ifelse(an == wave + 8,
                        1,
                        0),
         .after = post)

# Create the interacted variables b/n the previous dummies and
# the zone dummy
stacked_acc1 <- stacked_acc1 %>%
  mutate_at(vars(starts_with("post")),  funs(mod = . * zone)) %>%
  rename_at(vars(ends_with("_mod")), funs(paste("treated", gsub("_mod", "", .), sep = "_"))) %>% 
  relocate(starts_with("treated_post"),
           .after = zone)


# Now, run the stacked event study design

# n accidents
did_est1 <- as.formula(paste("n_accidents ~ ",
                           paste(c(colnames(stacked_acc1)[c(33:45,
                                                             543:(ncol(stacked_acc1)-6), 48, 50:(ncol(stacked_acc)-13))]),
                                 collapse = "+")))

stk_did1 <- lm(did_est1,
               data = stacked_acc1)

# Clustered SE
test_est1 <- coeftest(stk_did1, vcov = vcovCL, cluster = ~ iris_code)

# Coerce to a dataframe
test_est1_df <- test_est1[,] %>% 
  as_tibble() %>%
  mutate(variable = rownames(test_est1),
         .before = Estimate)

# Get confidence intervals
test_est1_df <- test_est1_df %>% 
  bind_cols(confint(test_est1) %>% 
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

# total victims
did_est2 <- as.formula(paste("total_victims ~ ",
                             paste(c(colnames(stacked_acc1)[c(33:45,
                                                              543:(ncol(stacked_acc1)-6), 48, 50:(ncol(stacked_acc)-13))]),
                                   collapse = "+")))

stk_did2 <- lm(did_est2,
               data = stacked_acc1)

# Clustered SE
test_est2 <- coeftest(stk_did2, vcov = vcovCL, cluster = ~ iris_code)

# Coerce to a dataframe
test_est2_df <- test_est2[,] %>% 
  as_tibble() %>%
  mutate(variable = rownames(test_est2),
         .before = Estimate)

# Get confidence intervals
test_est2_df <- test_est2_df %>% 
  bind_cols(confint(test_est2) %>% 
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

# KSI
did_est3 <- as.formula(paste("severe_victims ~ ",
                             paste(c(colnames(stacked_acc1)[c(33:45,
                                                              543:(ncol(stacked_acc1)-6), 48, 50:(ncol(stacked_acc)-13))]),
                                   collapse = "+")))

stk_did3 <- lm(did_est3,
               data = stacked_acc1)

# Clustered SE
test_est3 <- coeftest(stk_did3, vcov = vcovCL, cluster = ~ iris_code)

# Coerce to a dataframe
test_est3_df <- test_est3[,] %>% 
  as_tibble() %>%
  mutate(variable = rownames(test_est3),
         .before = Estimate)

# Get confidence intervals
test_est3_df <- test_est3_df %>% 
  bind_cols(confint(test_est3) %>% 
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

# dead
did_est4 <- as.formula(paste("n_dead ~ ",
                             paste(c(colnames(stacked_acc1)[c(33:45,
                                                              543:(ncol(stacked_acc1)-6), 48, 50:(ncol(stacked_acc)-13))]),
                                   collapse = "+")))

stk_did4 <- lm(did_est4,
               data = stacked_acc1)

# Clustered SE
test_est4 <- coeftest(stk_did4, vcov = vcovCL, cluster = ~ iris_code)

# Coerce to a dataframe
test_est4_df <- test_est4[,] %>% 
  as_tibble() %>%
  mutate(variable = rownames(test_est4),
         .before = Estimate)

# Get confidence intervals
test_est4_df <- test_est4_df %>% 
  bind_cols(confint(test_est4) %>% 
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

# Plot them
e1 <- ggplot(test_est1_df, aes(x = variable1, y = Estimate, group = 1)) + 
  geom_hline(yintercept = 0) + 
  stat_summary(fun = "mean", geom = "line", color = "green") + 
  geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`, width=0.2), color = "darkgrey") +
  geom_point(size = 2.5, color = "darkgreen") +
  labs(x = "Event time", y = "Estimate") + 
  scale_y_continuous(limits = c(-3.2, 0.7)) + 
  scale_x_discrete(labels = (-4:8)) + theme_bw() + 
  ggtitle("Number of accidents")

e2 <- ggplot(test_est2_df, aes(x = variable1, y = Estimate, group = 1)) + 
  geom_hline(yintercept = 0) + 
  stat_summary(fun = "mean", geom = "line", color = "green") + 
  geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`, width=0.2), color = "darkgrey") +
  geom_point(size = 2.5, color = "darkgreen") +
  labs(x = "Event time", y = "Estimate") + 
  scale_y_continuous(limits = c(-6, 1.2)) + 
  scale_x_discrete(labels = (-4:8)) + theme_bw() + 
  ggtitle("Number of total victims")

e3 <- ggplot(test_est3_df, aes(x = variable1, y = Estimate, group = 1)) + 
  geom_hline(yintercept = 0) + 
  stat_summary(fun = "mean", geom = "line", color = "green") + 
  geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`, width=0.2), color = "darkgrey") +
  geom_point(size = 2.5, color = "darkgreen") +
  labs(x = "Event time", y = "Estimate") + 
  scale_y_continuous(limits = c(-4, 1)) + 
  scale_x_discrete(labels = (-4:8)) + theme_bw() + 
  ggtitle("Number of KSI")

e4 <- ggplot(test_est4_df, aes(x = variable1, y = Estimate, group = 1)) + 
  geom_hline(yintercept = 0) + 
  stat_summary(fun = "mean", geom = "line", color = "green") + 
  geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`, width=0.2), color = "darkgrey") +
  geom_point(size = 2.5, color = "darkgreen") +
  labs(x = "Event time", y = "Estimate") + 
  scale_y_continuous(limits = c(-4, 0.8)) + 
  scale_x_discrete(labels = (-4:8)) + theme_bw() + 
  ggtitle("Number of dead victims")

e1

e2

e3

e4


# Plot them in a grid
ggarrange(e1, e2, e3, e4, nrow = 2, ncol = 2, common.legend = TRUE, legend="bottom")
  

# NOW REMOVE 2 PERIODS TO AVOID MULTICOLINEARITY
# n accidents
did_est2 <- as.formula(paste("n_accidents ~ ",
                             paste(c(colnames(stacked_acc1)[c(33:43,
                                                              543:(ncol(stacked_acc1)-6), 48, 50:(ncol(stacked_acc)-13))]),
                                   collapse = "+")))

stk_did2 <- lm(did_est2,
               data = stacked_acc1)

# Clustered SE
test_est2 <- coeftest(stk_did2, vcov = vcovCL, cluster = ~ iris_code)

