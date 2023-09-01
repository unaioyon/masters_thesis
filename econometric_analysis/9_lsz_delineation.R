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
library(glm) # generalized linear models
library(fastDummies) # generate dummies from categorical variables
library(SuperLearner) # ML
library(MASS)
library(glmnet)
library(gsubfn) # clean strings of accented characters


# Set working directory
setwd("/Users/unaioyon/Desktop/masters_thesis/data/fra")

options(scipen = 999)



#############################################
#### IMPORT THE DATA ########################
#############################################
# Transports
metro_geo <- st_read("delineation/metro_stations.geojson")
metro <- read_delim("delineation/metro_traffic_2013.csv",
                  delim = ";")

# Remove hyphens
metro_geo <- metro_geo %>% 
  mutate(station = gsub("-", "", nom_gares))

# Remove apostrophes
metro_geo <- metro_geo %>% 
  mutate(station = gsub("'", "", station))

# Remove spaces
metro_geo <- metro_geo %>% 
        mutate(station = gsub(" ", "", station))

# Convert to lower case
metro_geo <- metro_geo %>% 
  mutate(station = tolower(station))

# Remove accented characters
unwanted_array = list(    'Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                          'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                          'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                          'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                          'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ü'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )


metro_geo <- metro_geo %>% 
  mutate(station = gsubfn(paste(names(unwanted_array),collapse='|'),
                          unwanted_array,
                          station))
# Now, same for metro

# Remove hyphens
metro <- metro %>% 
  mutate(station = gsub("-", "", Station))

# Remove apostrophes
metro <- metro %>% 
  mutate(station = gsub("'", "", station))

# Remove spaces
metro <- metro %>% 
  mutate(station = gsub(" ", "", station))

# Convert to lower case
metro <- metro %>% 
  mutate(station = tolower(station))

# Remove accented characters

metro <- metro %>% 
  mutate(station = gsubfn(paste(names(unwanted_array),collapse='|'),
                          unwanted_array,
                          station))

# Remove asterisks
metro <- metro %>% 
  mutate(station = gsub("*", "", station))


# Now, they should be joinable. The geo version has duplicates for different lines, so I just wanna keep
# the other one. 
metro_geo <- metro_geo %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(27:(ncol(metro_geo)-1))

# Now, do the left join, then drop the ones outside of Paris intramuros
metro <- metro %>% 
  mutate(correspondance = ifelse(!is.na(Correspondance_1) |
                                   !is.na(Correspondance_2) |
                                   !is.na(Correspondance_3) |
                                   !is.na(Correspondance_4) |
                                   !is.na(Correspondance_5),
                                 1,
                                 0)) %>% 
  select(station, Trafic, Réseau)

metro_final <- metro_geo %>% 
  left_join(metro, by = "station")

# Drop outside of Paris
metro_final <- metro_final %>% 
  filter(!is.na(iris_code) & !is.na(Trafic))

rm(metro)
rm(metro_geo)
rm(unwanted_array)

#write it to csv
#write_csv(metro_final, "delineation/metro_final.csv")





# IRIS
iris <- st_read("iris/geo/iris_paris_2022/1_DONNEES_LIVRAISON_2022-05-00266/IRIS-GE_2-0_SHP_LAMB93_D075-2022/IRIS_GE.shp") 
iris_treated <- st_read("zones_30/iris_treatment_grouped.geojson") %>% 
  rename(CODE_IRIS = iris) %>% 
  mutate(treated = 1)

iris <- iris %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  full_join(iris_treated %>% 
              st_drop_geometry() %>% 
              as_tibble(), by = "CODE_IRIS") %>% 
  select(CODE_IRIS, INSEE_COM, TYP_IRIS, treated) %>% 
  mutate(treated = ifelse(is.na(treated),
                          0,
                          1))

# Transport
metro <- read_csv("delineation/metro_final.csv")

# Schools
schools <- st_read("delineation/schools.geojson")

# Vote
vote <- st_read("delineation/iris_bureaux_vote_corrected.geojson")

# DV3F
dv3f <- read_csv("dv3f_new_variables/dv3f_modified.csv")

# Accidents
acc <- st_read("accidents/final/accidents_final_geo.geojson")






###############################
###     IRIS CENSUS DATA    ###
###############################

# Variables have a common format that includes the last two digits of the year, so it is quite convenient
# to select them. Also, I have created a single file for each year, so I just need to import it.

for (i in 6:9) {
  assign(sprintf("iris%s",
                 paste0("0",
                        as.character(i))),
         read_csv(sprintf("iris/data/20%s/paris/paris%s.csv",
                          paste0("0",
                                 as.character(i)),
                          paste0("0",
                                 as.character(i)))))
}

# Select variables

iris06 <- iris06 %>% 
  select(IRIS, ARR, P06_POP, P06_POP0014, P06_POP6579,P06_POP_ETR, P06_POP_IMM,
         P06_POP80P_PSEUL, C06_MENCOUPAENF, C06_MENFAMMONO,
         P06_RP_VOIT1P, P06_RP_VOIT1, P06_RP_VOIT2P, P06_RP_GARL, P06_RP_PROP, P06_RP_LOC,
         C06_ACTOCC15P, C06_ACTOCC15P_PAS, C06_ACTOCC15P_MAR, C06_ACTOCC15P_DROU,
         C06_ACTOCC15P_VOIT, C06_ACTOCC15P_TCOM, P06_CHOM1564,
         RFMQ206, RFMRD06, RFMMO06, RFMGI06,
         P06_NSCOL15P_DIPL0, P06_NSCOL15P_SUP)

iris07 <- iris07 %>% 
  select(IRIS, ARR, P07_POP, P07_POP0014, P07_POP6579,P07_POP_ETR, P07_POP_IMM,
         P07_POP80P_PSEUL, C07_MENCOUPAENF, C07_MENFAMMONO,
         P07_RP_VOIT1P, P07_RP_VOIT1, P07_RP_VOIT2P, P07_RP_GARL, P07_RP_PROP, P07_RP_LOC,
         C07_ACTOCC15P, C07_ACTOCC15P_PAS, C07_ACTOCC15P_MAR, C07_ACTOCC15P_DROU,
         C07_ACTOCC15P_VOIT, C07_ACTOCC15P_TCOM, P07_CHOM1564,
         RFMQ207, RFMRD07, RFMMO07, RFMGI07,
         P07_NSCOL15P_DIPL0, P07_NSCOL15P_SUP)

iris08 <- iris08 %>% 
  select(IRIS, ARR, P08_POP, P08_POP0014, P08_POP6579,P08_POP_ETR, P08_POP_IMM,
         P08_POP80P_PSEUL, C08_MENCOUPAENF, C08_MENFAMMONO,
         P08_RP_VOIT1P, P08_RP_VOIT1, P08_RP_VOIT2P, P08_RP_GARL, P08_RP_PROP, P08_RP_LOC,
         C08_ACTOCC15P, C08_ACTOCC15P_PAS, C08_ACTOCC15P_MAR, C08_ACTOCC15P_DROU,
         C08_ACTOCC15P_VOIT, C08_ACTOCC15P_TCOM, P08_CHOM1564,
         RFMQ208, RFMRD08, RFMMO08, RFMGI08,
         P08_NSCOL15P_DIPL0, P08_NSCOL15P_SUP)

iris09 <- iris09 %>% 
  select(IRIS, ARR, P09_POP, P09_POP0014, P09_POP6579,P09_POP_ETR, P09_POP_IMM,
         P09_POP80P_PSEUL, C09_MENCOUPAENF, C09_MENFAMMONO,
         P09_RP_VOIT1P, P09_RP_VOIT1, P09_RP_VOIT2P, P09_RP_GARL, P09_RP_PROP, P09_RP_LOC,
         C09_ACTOCC15P, C09_ACTOCC15P_PAS, C09_ACTOCC15P_MAR, C09_ACTOCC15P_DROU,
         C09_ACTOCC15P_VOIT, C09_ACTOCC15P_TCOM, P09_CHOM1564,
         RFMQ209, RFMRD09, RFMMO09,
         P09_NSCOL15P_DIPL0, P09_NSCOL15P_SUP) # NO GINI



# Aggregate them for the 06-09 period using averages
iris_data <- iris06 %>% 
  full_join(iris07, by = "IRIS") %>% 
  full_join(iris08, by = "IRIS") %>% 
  full_join(iris09, by = "IRIS")

iris_data <- iris_data %>% 
  mutate(pop = (P06_POP + P07_POP + P08_POP +P09_POP)/4,
         pop_0014 = (P06_POP0014 + P07_POP0014 + P08_POP0014 +P09_POP0014)/4,
         pop_65_79 = (P06_POP6579 + P07_POP6579 + P08_POP6579 +P09_POP6579)/4,
         pop_etr = (P06_POP_ETR + P07_POP_ETR + P08_POP_ETR + P09_POP_ETR)/4,
         pop_imm = (P06_POP_IMM + P07_POP_IMM + P08_POP_IMM + P09_POP_IMM)/4,
         pop_80seul = (P06_POP80P_PSEUL + P07_POP80P_PSEUL + P08_POP80P_PSEUL + P09_POP80P_PSEUL)/4,
         fam_coupaenf = (C06_MENCOUPAENF + C07_MENCOUPAENF + C08_MENCOUPAENF + C09_MENCOUPAENF)/4,
         fam_monoenf = (C06_MENFAMMONO + C07_MENFAMMONO + C08_MENFAMMONO + C09_MENFAMMONO)/4,
         mob_voit1p = (P06_RP_VOIT1P + P07_RP_VOIT1P + P08_RP_VOIT1P + P09_RP_VOIT1P)/4,
         mob_voit1 = (P06_RP_VOIT1 + P07_RP_VOIT1 + P08_RP_VOIT1 + P09_RP_VOIT1)/4,
         mob_voit2p = (P06_RP_VOIT2P + P07_RP_VOIT2P + P08_RP_VOIT2P + P09_RP_VOIT2P)/4,
         mob_garage = (P06_RP_GARL + P07_RP_GARL + P08_RP_GARL + P09_RP_GARL)/4,
         dwe_prop = (P06_RP_PROP + P07_RP_PROP + P08_RP_PROP + P09_RP_PROP)/4,
         dew_loc = (P06_RP_LOC + P07_RP_LOC + P08_RP_LOC + P09_RP_LOC)/4,
         com_p = (C06_ACTOCC15P + C07_ACTOCC15P + C08_ACTOCC15P + C09_ACTOCC15P)/4,
         com_pas = (C06_ACTOCC15P_PAS + C07_ACTOCC15P_PAS + C08_ACTOCC15P_PAS + C09_ACTOCC15P_PAS)/4,
         com_mar = (C06_ACTOCC15P_MAR + C07_ACTOCC15P_MAR + C08_ACTOCC15P_MAR + C09_ACTOCC15P_MAR)/4,
         com_drou = (C06_ACTOCC15P_DROU + C07_ACTOCC15P_DROU + C08_ACTOCC15P_DROU + C09_ACTOCC15P_DROU)/4,
         com_voit = (C06_ACTOCC15P_VOIT + C07_ACTOCC15P_VOIT + C08_ACTOCC15P_VOIT + C09_ACTOCC15P_VOIT)/4,
         com_tcom = (C06_ACTOCC15P_TCOM + C07_ACTOCC15P_TCOM + C08_ACTOCC15P_TCOM + C09_ACTOCC15P_TCOM)/4,
         pop_chom1564 = (P06_CHOM1564 + P07_CHOM1564 + P08_CHOM1564 + P09_CHOM1564)/4,
         median = (RFMQ206 + RFMQ207 + RFMQ208 + RFMQ209)/4,
         iqrd = (RFMRD06 + RFMRD07 + RFMRD08 + RFMRD09)/4,
         mean = (RFMMO06 + RFMMO07 + RFMMO08 + RFMMO09)/4,
         gini = (RFMQ206 + RFMQ207 + RFMQ208)/3,
         for_none = (P06_NSCOL15P_DIPL0 + P07_NSCOL15P_DIPL0 + P08_NSCOL15P_DIPL0 + P09_NSCOL15P_DIPL0)/4,
         for_sup = (P06_NSCOL15P_SUP + P07_NSCOL15P_SUP + P08_NSCOL15P_SUP + P09_NSCOL15P_SUP)/4,
       )

iris_data <- iris_data %>% 
  select(c(1,2,113:ncol(iris_data))) %>% 
  rename(arr = ARR.x)

# Check for missing data
lapply(iris_data, function(x) sum(is.na(x)))

# Just the revenues data have NAs

# Now, no need to round up all the variables since they are averages


#################################
### MAKING THE FINAL DATABASE ###
#################################

iris_data <- iris_data %>% 
  rename(iris_code = IRIS)

iris <- iris %>% 
  rename(iris_code = CODE_IRIS)

# Merge everything
del <- iris %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  mutate(arr = str_sub(INSEE_COM,
                       4,
                       5)) %>% 
  select(iris_code, arr, TYP_IRIS, treated)

del <- del %>%  
  left_join(iris_data %>% 
              select(-arr) %>% 
              mutate(iris_code = as.character(iris_code)), by = "iris_code")


# The rest of the data needs to be aggregated

# Vote:
vote_g <- vote %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  group_by(iris) %>% 
  summarise(gauche_share = weighted.mean(gauche_share, weights),
            droite_share = weighted.mean(droite_share, weights)) %>% 
  rename(iris_code = iris)

del <- del %>% 
  left_join(vote_g, by = "iris_code")

# Schools:
schools_g <- schools %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  group_by(iris_code) %>% 
  summarise(n_college = sum(type == "college"),
            n_maternelle = sum(type == "maternelle"),
            n_elementaire = sum(type == "elementaire"))
del <- del %>% 
  left_join(schools_g, by = "iris_code") %>% 
  mutate(n_college = ifelse(is.na(n_college),
                            0,
                            n_college),
         n_maternelle = ifelse(is.na(n_maternelle),
                            0,
                            n_maternelle),
         n_elementaire = ifelse(is.na(n_elementaire),
                            0,
                            n_elementaire))

# Metro:
metro_g <- metro %>% 
  group_by(iris_code) %>% 
  summarise(traffic = sum(Trafic, na.rm = TRUE)) %>% 
  mutate(metro_station = 1,
         iris_code = as.character(iris_code))

del <- del %>% 
  left_join(metro_g, by = "iris_code") %>% 
  mutate(metro_station = ifelse(is.na(metro_station),
                                0,
                                metro_station),
         traffic = ifelse(is.na(traffic),
                          0,
                          traffic))

# Accidents
acc_g <- acc %>% 
  filter(an %in% 2005:2009) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  group_by(iris_code) %>% 
  summarise(n_accidents = n(),
            n_dead = sum(tue_nb, na.rm = TRUE),
            has_pietons = mean(has_pietons, na.rm = TRUE),
            has_bike = mean(has_velo, na.rm = TRUE))

del <- del %>% 
  left_join(acc_g, by = "iris_code") %>% 
  mutate(n_accidents = ifelse(is.na(n_accidents),
                              0,
                              n_accidents),
         n_dead = ifelse(is.na(n_dead),
                              0,
                              n_dead),
         has_pietons = ifelse(is.na(has_pietons),
                              0,
                              has_pietons),
         has_bike = ifelse(is.na(has_bike),
                              0,
                              has_bike))


#write_csv(del, "delineation/iris_characteristics_full_data.csv")

#rm(list = ls())

######################### COMPUTING STUFF

#### BINARY TREATMENT
# All the variables
formula1 <- as.formula(paste("treated ~ ", paste(colnames(del %>% 
                                                               dplyr::select(-c(iris_code,treated,arr,
                                                                      mob_voit1p, mob_voit1, mob_voit2p,
                                                                      com_p, treated_lag, earliest_year,
                                                                      TYP_IRIS))),
                                                 collapse = "+")))

ols1 <- lm(formula1,
           data = del)

summary(ols1)


# Now, without 2010
ols1_1 <- lm(formula1,
           data = del %>% 
             filter(earliest_year != 2010))

summary(ols1_1)

# Now, just 2010
ols1_2 <- lm(formula1,
             data = del %>% 
               filter(earliest_year == 2010))

summary(ols1_2)



#### NOW, CHECK WHO WON INSTEAD OF SHARE OF VOTES
del <- del %>% 
  mutate(left_win = ifelse(gauche_share >= 0.5 & !is.na(gauche_share),
                           1,
                           ifelse(is.na(gauche_share),
                                  NA,
                                  0)))


formula1_1 <- as.formula(paste("treated ~ ", paste(colnames(del %>% 
                                                            dplyr::select(-c(iris_code,treated,arr,
                                                                             mob_voit1p, mob_voit1, mob_voit2p,
                                                                             com_p, treated_lag, earliest_year,
                                                                             TYP_IRIS, gauche_share, droite_share))),
                                                 collapse = "+")))

ols1_win <- lm(formula1_1,
           data = del)

summary(ols1_win)


#### NOW, CHECK WHO WON INSTEAD OF SHARE OF VOTES but USE THE EVENTUAL RESULTS INSTEAD OF AGGREGATING VOTES
del <- del %>% 
  mutate(left_win_final = ifelse(arr %in% c(1, 5:8, 15:17),
                                 0,
                                 1))


formula1_2 <- as.formula(paste("treated ~ ", paste(colnames(del %>% 
                                                              dplyr::select(-c(iris_code,treated,arr,
                                                                               mob_voit1p, mob_voit1, mob_voit2p,
                                                                               com_p, treated_lag, earliest_year,
                                                                               TYP_IRIS, droite_share, left_win))),
                                                   collapse = "+")))

ols2_win <- lm(formula1_2,
               data = del)

summary(ols2_win)


# Logit model
logit1 <- glm(formula1, 
              data = del,
              family = binomial(link = "logit"))

summary(logit1)

# Lasso

# Using glmnet
lasso2 <- glmnet(del_lasso_x, del_lasso$treated,
                 alpha = 1,)

beta <- coef(lasso2)

tmp <- as.data.frame(as.matrix(beta))
tmp$coef <- row.names(tmp)
tmp <- reshape::melt(tmp, id = "coef")
tmp$variable <- as.numeric(gsub("s", "", tmp$variable))
tmp$lambda <- lasso2$lambda[tmp$variable+1] # extract the lambda values
tmp$norm <- apply(abs(beta[-1,]), 2, sum)[tmp$variable+1] # compute L1 norm

tmp <- tmp %>% 
  mutate(coef = ifelse(coef == "com_tcom",
                       "# of public transport users",
                       ifelse(coef == "gauche_share",
                              "Share of left-wing votes",
                              ifelse(coef == "has_bike",
                                     "Share of accidents with bikes",
                                     coef))))

# x11(width = 13/2.54, height = 9/2.54)
ggplot(tmp[tmp$coef != "(Intercept)",] %>% 
         filter(coef %in% c("# of public transport users",
                            "Share of left-wing votes",
                            "Share of accidents with bikes"))) + 
  geom_line(
            aes(lambda, value, color = coef, linetype = coef)) + 
  scale_x_log10() + 
  guides(color = guide_legend(title = ""), 
         linetype = guide_legend(title = "")) +
  theme(legend.key.width = unit(3,"lines")) + 
  labs(x = "Lambda (log scale)", y = "Coefficient value") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                                panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


plot(lasso2, label = 12)

# Using SuperLearner
sl_lasso <- create.Learner("SL.glmnet", params = list(alpha = 1), name_prefix="lasso")

del_lasso <- del %>% 
  relocate(treated, .before = arr) %>% 
  select(-c(iris_code, arr, TYP_IRIS,mob_voit1p, mob_voit1, mob_voit2p,
            com_p, droite_share)) %>% 
  filter(!(if_any(everything(), is.na))) # 628 obs.

del_lasso_x <- del_lasso %>% 
  select(-treated)
  
lasso1 <- SuperLearner(del_lasso$treated,
                       del_lasso_x,
                       family = gaussian(),
                       SL.library = sl_lasso$names)

# Get the LASSO coefficients
lasso_coeffs <- data.frame(lambda = lasso1$fitLibrary$lasso_1_All$object$lambda)

coef(lasso1$fitLibrary$lasso_1_All$object) # first 20 OLS coeffs
std


##### LAG WHEN RECEIVING TREATMENT
iris_treated <- iris_treated %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  dplyr::select(earliest_year, iris_code) %>% 
  rename(iris_code = "CODE_IRIS")

del <- del %>% 
  left_join(iris_treated %>% 
              st_drop_geometry() %>% 
              as_tibble() %>% 
              rename(iris_code = "CODE_IRIS") %>% 
              dplyr::select(earliest_year, iris_code), by = "iris_code")

del <- del %>% 
  mutate(treated_lag = ifelse(!is.na(earliest_year),
                              earliest_year - 2010,
                              earliest_year))

del_lag <- del %>% 
  relocate(treated, treated_lag, .before = iris_code) %>% 
  filter(treated == 1) %>% 
  dplyr::select(-earliest_year)

#### fit models
formula2 <- as.formula(paste("treated_lag ~ ", paste(colnames(del_lag %>% 
                                                            dplyr::select(-c(iris_code,treated,treated_lag,arr,
                                                                      mob_voit1p, mob_voit1, mob_voit2p,
                                                                      com_p, TYP_IRIS))),
                                                 
                                                 collapse = "+")))

ols2 <- lm(formula2,
           data = del_lag)

summary(ols2)


# Now, I want a stargazer with all the estimations
stargazer(ols1, logit1, ols1_1, ols2, notes = "\\parbox[t]{\\textwidth}{Logistic regression. Dependent variable: an indicator varible ... AND Some very long and interesting comment.}",
          notes.append = FALSE)


#### Stargazer on the number of slow zones implemented each year
zones <- st_read("zones_30/zones-30.shp") %>% 
  st_drop_geometry() %>% 
  as_tibble()

zones_g <- zones %>% 
  group_by(year) %>% 
  summarise(n = n())

stargazer(zones_g, summary.stat = "mean",
          digits = 2, summary = FALSE, align = TRUE, rownames = FALSE)
