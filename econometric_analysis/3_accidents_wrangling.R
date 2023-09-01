# Import packages
library(tidyverse)
library(textclean) # clean texts
library(sf) # geo
library(ggridges) # plotting
library(viridis) # plotting
library(hrbrthemes) # plotting
library(RColorBrewer) # color palette
library(stargazer)
library(plotrix) # compute standard errors of mean


# Set working directory
setwd("/Users/unaioyon/Desktop/masters_thesis/data/fra")

options(scipen = 999)


#################################
####  1. Importing the data  ####
#################################

####### 2011 AS AN EXAMPLE ########

acc11 <- read_delim("accidents/2011/2011.csv",
                    delim = ";")

acc11_car <- read_csv("accidents/2011/caracteristiques_2011.csv")

acc11_lie <- read_csv("accidents/2011/lieux_2011.csv")

acc11_veh <- read_csv("accidents/2011/vehicules_2011.csv")

acc11_usa <- read_csv("accidents/2011/usagers_2011.csv")

acc20 <- read_csv("accidents/2020/2020.csv")

# Is the identifier a primary key? Yes for caracteristiques and lieux, but not for
# véhicules and usagers, and it makes perfect sense.

# Check for missing values
lapply(acc11_car, function(x) sum(is.na(x)))

# Use a loop to import all the years (2009 done later bc the delimiter is " ")
for (i in c(5:8,10:20)) {
  if (i < 19){
    if (i < 10){
      # Assign each of the 4 datasets to an object with the type of data and the year
      # Then do the same for Paris
      
      # characteristiques
      assign(sprintf("acc%s_car",
                     paste0("0",as.character(i))),
             read_csv(sprintf("accidents/20%s/caracteristiques_20%s.csv",
                              paste0("0",as.character(i)),
                      paste0("0",as.character(i)))))
      
      assign(sprintf("acc%s_car_paris",
                     paste0("0",as.character(i))),
             read_csv(sprintf("accidents/20%s/caracteristiques_20%s.csv",
                              paste0("0",as.character(i)),
                              paste0("0",as.character(i)))) %>% 
               filter(dep == "750"))
      
      # As "dep" does not appear in the other datasets, subset by identifier
      # This is newly assigned for each year
      paris_id <- read_csv(sprintf("accidents/20%s/caracteristiques_20%s.csv",
                                   paste0("0",as.character(i)),
                                   paste0("0",as.character(i)))) %>% 
        filter(dep == "750") %>% 
        mutate(Num_Acc = as.character(Num_Acc)) %>% 
        select(Num_Acc) # accident identifier
      
      paris_id <- as.list(paris_id)[[1]]
      
      # lieux
      assign(sprintf("acc%s_lie",
                     paste0("0",as.character(i))),
             read_csv(sprintf("accidents/20%s/lieux_20%s.csv",
                              paste0("0",as.character(i)),
                              paste0("0",as.character(i)))))
      
      assign(sprintf("acc%s_lie_paris",
                     paste0("0",as.character(i))),
             read_csv(sprintf("accidents/20%s/lieux_20%s.csv",
                              paste0("0",as.character(i)),
                              paste0("0",as.character(i)))) %>% 
               filter(Num_Acc %in% paris_id))
      # véhicules
      assign(sprintf("acc%s_veh",
                     paste0("0",as.character(i))),
             read_csv(sprintf("accidents/20%s/vehicules_20%s.csv",
                              paste0("0",as.character(i)),
                              paste0("0",as.character(i)))))
      
      assign(sprintf("acc%s_veh_paris",
                     paste0("0",as.character(i))),
             read_csv(sprintf("accidents/20%s/vehicules_20%s.csv",
                              paste0("0",as.character(i)),
                              paste0("0",as.character(i)))) %>% 
               filter(as.character(Num_Acc) %in% paris_id))
      # usagers
      assign(sprintf("acc%s_usa",
                     paste0("0",as.character(i))),
             read_csv(sprintf("accidents/20%s/usagers_20%s.csv",
                              paste0("0",as.character(i)),
                              paste0("0",as.character(i)))))
      assign(sprintf("acc%s_usa_paris",
                     paste0("0",as.character(i))),
             read_csv(sprintf("accidents/20%s/usagers_20%s.csv",
                              paste0("0",as.character(i)),
                              paste0("0",as.character(i))))  %>% 
               filter(Num_Acc %in% paris_id))
      
      
    }
    else{
  # Assign each of the 4 datasets to an object with the type of data and the year
  # Then do the same for Paris
    
  # characteristiques
  assign(sprintf("acc%s_car",
                 as.character(i)),
         read_csv(sprintf("accidents/20%s/caracteristiques_20%s.csv",
                          as.character(i),
                          as.character(i))))
    
    assign(sprintf("acc%s_car_paris",
                   as.character(i)),
           read_csv(sprintf("accidents/20%s/caracteristiques_20%s.csv",
                            as.character(i),
                            as.character(i))) %>% 
             filter(dep == "750"))
    
    # As "dep" does not appear in the other datasets, subset by identifier
    # This is newly assigned for each year
    paris_id <- read_csv(sprintf("accidents/20%s/caracteristiques_20%s.csv",
                            as.character(i),
                            as.character(i))) %>% 
             filter(dep == "750") %>% 
             mutate(Num_Acc = as.character(Num_Acc)) %>% 
             select(Num_Acc) # accident identifier
    
    paris_id <- as.list(paris_id)[[1]]
             
  # lieux
  assign(sprintf("acc%s_lie",
                 as.character(i)),
         read_csv(sprintf("accidents/20%s/lieux_20%s.csv",
                          as.character(i),
                          as.character(i))))
  
  assign(sprintf("acc%s_lie_paris",
                 as.character(i)),
         read_csv(sprintf("accidents/20%s/lieux_20%s.csv",
                          as.character(i),
                          as.character(i))) %>% 
           filter(Num_Acc %in% paris_id))
  # véhicules
  assign(sprintf("acc%s_veh",
                 as.character(i)),
         read_csv(sprintf("accidents/20%s/vehicules_20%s.csv",
                          as.character(i),
                          as.character(i))))
  
  assign(sprintf("acc%s_veh_paris",
                 as.character(i)),
         read_csv(sprintf("accidents/20%s/vehicules_20%s.csv",
                          as.character(i),
                          as.character(i))) %>% 
           filter(as.character(Num_Acc) %in% paris_id))
  # usagers
  assign(sprintf("acc%s_usa",
                 as.character(i)),
         read_csv(sprintf("accidents/20%s/usagers_20%s.csv",
                          as.character(i),
                          as.character(i))))
  assign(sprintf("acc%s_usa_paris",
                 as.character(i)),
         read_csv(sprintf("accidents/20%s/usagers_20%s.csv",
                          as.character(i),
                          as.character(i)))  %>% 
           filter(Num_Acc %in% paris_id))

    }
  }
  
  # as 2019 and 2020 are delimited by ; instead of ,
  # and dep only consits of two figures, i.e. "75"
  else {
    # characteristiques
    assign(sprintf("acc%s_car",
                   as.character(i)),
           read_delim(sprintf("accidents/20%s/caracteristiques_20%s.csv",
                            as.character(i),
                            as.character(i)),
                      delim = ";"))
    
    assign(sprintf("acc%s_car_paris",
                   as.character(i)),
           read_delim(sprintf("accidents/20%s/caracteristiques_20%s.csv",
                            as.character(i),
                            as.character(i)),
                      delim = ";") %>% 
             filter(dep == "75"))
    
    # As "dep" does not appear in the other datasets, subset by identifier
    # This is newly assigned for each year
    paris_id <- read_delim(sprintf("accidents/20%s/caracteristiques_20%s.csv",
                                 as.character(i),
                                 as.character(i)),
                           delim = ";") %>% 
      filter(dep == "75") %>% 
      select(Num_Acc) # accident identifier
    
    paris_id <- as.list(paris_id)[[1]]
    
    # lieux
    assign(sprintf("acc%s_lie",
                   as.character(i)),
           read_delim(sprintf("accidents/20%s/lieux_20%s.csv",
                            as.character(i),
                            as.character(i)),
                      delim = ";"))
    
    assign(sprintf("acc%s_lie_paris",
                   as.character(i)),
           read_delim(sprintf("accidents/20%s/lieux_20%s.csv",
                            as.character(i),
                            as.character(i)),
                      delim = ";") %>% 
             filter(Num_Acc %in% paris_id))
    # véhicules
    assign(sprintf("acc%s_veh",
                   as.character(i)),
           read_delim(sprintf("accidents/20%s/vehicules_20%s.csv",
                            as.character(i),
                            as.character(i)),
                      delim = ";"))
    
    assign(sprintf("acc%s_veh_paris",
                   as.character(i)),
           read_delim(sprintf("accidents/20%s/vehicules_20%s.csv",
                            as.character(i),
                            as.character(i)),
                      delim = ";") %>% 
             filter(Num_Acc %in% paris_id))
    # usagers
    assign(sprintf("acc%s_usa",
                   as.character(i)),
           read_delim(sprintf("accidents/20%s/usagers_20%s.csv",
                            as.character(i),
                            as.character(i)),
                      delim = ";"))
    assign(sprintf("acc%s_usa_paris",
                   as.character(i)),
           read_delim(sprintf("accidents/20%s/usagers_20%s.csv",
                            as.character(i),
                            as.character(i)),
                      delim = ";")  %>% 
             filter(Num_Acc %in% paris_id))
    
  }
  
}

# 2009
acc09_car <- read_delim("accidents/2009/caracteristiques_2009.csv")
acc09_lie <- read_delim("accidents/2009/lieux_2009.csv")
acc09_veh <- read_delim("accidents/2009/vehicules_2009.csv")
acc09_usa <- read_delim("accidents/2009/usagers_2009.csv")

acc09_car_paris <- read_delim("accidents/2009/caracteristiques_2009.csv") %>% 
  filter(dep == "750")

paris_id <- acc09_car_paris$Num_Acc

acc09_lie_paris <- read_delim("accidents/2009/lieux_2009.csv") %>% 
  filter(Num_Acc %in% paris_id)
acc09_veh_paris <- read_delim("accidents/2009/vehicules_2009.csv") %>% 
  filter(Num_Acc %in% paris_id)
acc09_usa_paris <- read_delim("accidents/2009/usagers_2009.csv") %>% 
  filter(Num_Acc %in% paris_id)

rm(paris_id)


#################################
####  2. Exploring the data  ####
#################################

# 2019 and 2020 are delimited by ";" instead of ",". Also, their column names are in
# different order and some of them are different.

# 2011-2018 poses problems when it comes to parsing the dates. Thus, this is performed
# manually.




#####################################
####  3. Descriptive statistics  ####
#####################################

# Creating a table with the number of accidents per year in France and Paris
number_acc <- vector()
number_acc_paris <- vector()
for (i in 5:20) {
  if(i < 10){
    # France
    number_acc <- append(number_acc,
                         nrow(get(sprintf("acc%s_car",
                                          paste0("0",
                                                 as.character(i))))))
    
    # Paris
    number_acc_paris <- append(number_acc_paris,
                               nrow(get(sprintf("acc%s_car_paris",
                                                paste0("0",
                                                       as.character(i))))))
  }
  
  else{
    # France
    number_acc <- append(number_acc,
                         nrow(get(sprintf("acc%s_car",
                                          as.character(i)))))
    
    # Paris
    number_acc_paris <- append(number_acc_paris,
                               nrow(get(sprintf("acc%s_car_paris",
                                                as.character(i)))))
  }
  
}


# Check for NAs in Paris
# Vector with number of missing coordinates
na_geom <- vector()
na_address <- vector()

for (i in 5:20) {
  if (i < 10){
    assign(sprintf("na_paris%s",
                   paste0("0", as.character(i))),
           tibble(lapply(get(sprintf("acc%s_car_paris",
                                     paste0("0", as.character(i)))),
                         function(x) sum(is.na(x)))) %>% 
      rename(na = "lapply(...)"))
    
    na_geom <- append(na_geom,
                      get(sprintf("na_paris%s",
                                  paste0("0", as.character(i))))$na$lat[[1]])
    
    na_address <- append(na_address,
                         get(sprintf("na_paris%s",
                                     paste0("0", as.character(i))))$na$adr[[1]])
  }
  else{
    assign(sprintf("na_paris%s",
                   as.character(i)),
           tibble(lapply(get(sprintf("acc%s_car_paris",
                                     as.character(i))),
                         function(x) sum(is.na(x)))) %>% 
             rename(na = "lapply(get(sprintf(\"acc%s_car_paris\", as.character(i))), function(x) sum(is.na(x)))"))
    
    
    na_geom <- append(na_geom,
                      get(sprintf("na_paris%s",
                                  as.character(i)))$na$lat[[1]])
    
    na_address <- append(na_address,
                         get(sprintf("na_paris%s",
                                     as.character(i)))$na$adr[[1]])
  }
}


# Create a joint dataframe with number of accidents, number of NAs & shares
number_accidents <- tibble(2005:2020,
                           number_acc,
                           number_acc_paris,
                           na_geom,
                           na_address) %>% 
  mutate(share_acc_paris = round(number_acc_paris/number_acc*100,2),
         .after = number_acc_paris) %>% 
  rename(year = "2005:2020") %>% 
  mutate(share_na_geom_paris = round(na_geom/number_acc_paris*100,2),
         .after = na_geom)

#################################
####  4. Geocoding the data  ####
#################################

# Using the geocoded database by Tristram Gräbener
# import it
acc_geo <- read_delim("accidents/geolocated_tristramgrabener/accidents.csv",
                      delim = ";")

# Check the covered timespan
summary(acc_geo$ANNEE) # From 2005 # Until 2016

# Check for NAs to subset Paris
sum(is.na(acc_geo$dep))


# Select accidents occurring in Paris
sum(number_accidents[number_accidents$year < 2017,]$number_acc_paris)
acc_geo_paris <- acc_geo %>% 
  filter(dep == "75") # Not the same as the number selected by me
rm(acc_geo)

# Save the Paris data
# write_delim(acc_geo_paris, "accidents/geolocated_tristramgrabener/paris/accidents_paris.csv",
           # delim = ";")

########### DESCRIPTIVE STATS
# Check by year
acc_grouped <- acc_geo_paris %>% 
  group_by(ANNEE) %>% 
  summarise(n = n(),
            n_non_na = sum(!is.na(longitude)))

acc_geo_paris05 <- acc_geo_paris %>% 
  filter(ANNEE == 2005)

acc_geo_paris16 <- acc_geo_paris %>% 
  filter(ANNEE == 2016)

# write the difference to see how many observations are dropped
number_accidents <- number_accidents %>% 
  mutate(share_geocoded_paris = ifelse(year < 2017,
                                       round(acc_grouped$n_non_na/number_acc_paris*100, 2),
                                       round((number_acc_paris - na_geom)/number_acc_paris*100, 2)))

# Create a table
stargazer(number_accidents, header = FALSE, summary = FALSE, rownames = FALSE)

iris19 <- st_read("iris/geo/CONTOURS-IRIS_2-1__SHP__FRA_2019-01-01/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2020-01-00139/CONTOURS-IRIS_2-1_SHP_LAMB93_FXX-2019/CONTOURS-IRIS.SHP")





##########################################
####  5. Creating the final database  ####
##########################################

# Impor the data for 2017-2020
# Use a loop to import all the years (2009 done later bc the delimiter is " ")
for (i in c(17:20)) {
  if (i < 19){
      # Assign each of the 4 datasets to an object with the type of data and the year
      # Then do the same for Paris
      
      # characteristiques
      assign(sprintf("acc%s_car",
                     as.character(i)),
             read_csv(sprintf("accidents/20%s/caracteristiques_20%s.csv",
                              as.character(i),
                              as.character(i))))
      
      assign(sprintf("acc%s_car_paris",
                     as.character(i)),
             read_csv(sprintf("accidents/20%s/caracteristiques_20%s.csv",
                              as.character(i),
                              as.character(i))) %>% 
               filter(dep == "750"))
      
      # As "dep" does not appear in the other datasets, subset by identifier
      # This is newly assigned for each year
      paris_id <- read_csv(sprintf("accidents/20%s/caracteristiques_20%s.csv",
                                   as.character(i),
                                   as.character(i))) %>% 
        filter(dep == "750") %>% 
        mutate(Num_Acc = as.character(Num_Acc)) %>% 
        select(Num_Acc) # accident identifier
      
      paris_id <- as.list(paris_id)[[1]]
      
      # lieux
      assign(sprintf("acc%s_lie",
                     as.character(i)),
             read_csv(sprintf("accidents/20%s/lieux_20%s.csv",
                              as.character(i),
                              as.character(i))))
      
      assign(sprintf("acc%s_lie_paris",
                     as.character(i)),
             read_csv(sprintf("accidents/20%s/lieux_20%s.csv",
                              as.character(i),
                              as.character(i))) %>% 
               filter(Num_Acc %in% paris_id))
      # véhicules
      assign(sprintf("acc%s_veh",
                     as.character(i)),
             read_csv(sprintf("accidents/20%s/vehicules_20%s.csv",
                              as.character(i),
                              as.character(i))))
      
      assign(sprintf("acc%s_veh_paris",
                     as.character(i)),
             read_csv(sprintf("accidents/20%s/vehicules_20%s.csv",
                              as.character(i),
                              as.character(i))) %>% 
               filter(as.character(Num_Acc) %in% paris_id))
      # usagers
      assign(sprintf("acc%s_usa",
                     as.character(i)),
             read_csv(sprintf("accidents/20%s/usagers_20%s.csv",
                              as.character(i),
                              as.character(i))))
      assign(sprintf("acc%s_usa_paris",
                     as.character(i)),
             read_csv(sprintf("accidents/20%s/usagers_20%s.csv",
                              as.character(i),
                              as.character(i)))  %>% 
               filter(Num_Acc %in% paris_id))
      
  }
  
  # as 2019 and 2020 are delimited by ; instead of ,
  # and dep only consits of two figures, i.e. "75"
  else {
    # characteristiques
    assign(sprintf("acc%s_car",
                   as.character(i)),
           read_delim(sprintf("accidents/20%s/caracteristiques_20%s.csv",
                              as.character(i),
                              as.character(i)),
                      delim = ";"))
    
    assign(sprintf("acc%s_car_paris",
                   as.character(i)),
           read_delim(sprintf("accidents/20%s/caracteristiques_20%s.csv",
                              as.character(i),
                              as.character(i)),
                      delim = ";") %>% 
             filter(dep == "75"))
    
    # As "dep" does not appear in the other datasets, subset by identifier
    # This is newly assigned for each year
    paris_id <- read_delim(sprintf("accidents/20%s/caracteristiques_20%s.csv",
                                   as.character(i),
                                   as.character(i)),
                           delim = ";") %>% 
      filter(dep == "75") %>% 
      select(Num_Acc) # accident identifier
    
    paris_id <- as.list(paris_id)[[1]]
    
    # lieux
    assign(sprintf("acc%s_lie",
                   as.character(i)),
           read_delim(sprintf("accidents/20%s/lieux_20%s.csv",
                              as.character(i),
                              as.character(i)),
                      delim = ";"))
    
    assign(sprintf("acc%s_lie_paris",
                   as.character(i)),
           read_delim(sprintf("accidents/20%s/lieux_20%s.csv",
                              as.character(i),
                              as.character(i)),
                      delim = ";") %>% 
             filter(Num_Acc %in% paris_id))
    # véhicules
    assign(sprintf("acc%s_veh",
                   as.character(i)),
           read_delim(sprintf("accidents/20%s/vehicules_20%s.csv",
                              as.character(i),
                              as.character(i)),
                      delim = ";"))
    
    assign(sprintf("acc%s_veh_paris",
                   as.character(i)),
           read_delim(sprintf("accidents/20%s/vehicules_20%s.csv",
                              as.character(i),
                              as.character(i)),
                      delim = ";") %>% 
             filter(Num_Acc %in% paris_id))
    # usagers
    assign(sprintf("acc%s_usa",
                   as.character(i)),
           read_delim(sprintf("accidents/20%s/usagers_20%s.csv",
                              as.character(i),
                              as.character(i)),
                      delim = ";"))
    assign(sprintf("acc%s_usa_paris",
                   as.character(i)),
           read_delim(sprintf("accidents/20%s/usagers_20%s.csv",
                              as.character(i),
                              as.character(i)),
                      delim = ";")  %>% 
             filter(Num_Acc %in% paris_id))
    
  }
  
}
unused
# Drop unused objects
unused <- as_tibble(ls()) %>% 
  filter(!(str_sub(value, 10, 10) == "_")) %>% 
  as_vector()
rm(list = c("i", "paris_id", "unused", unused[2:length(unused)]))

# Using the metadata... change the numeric values associated to categories
# to the underlying categories to make comparable to the
# geocoded 2005-2016 version.

# After further inspection, however, there are some abbreviations and
# conventions used in the geocoded database that do not exactly
# correspond with the metadata (e.g. "intersection à plus de
# 4 branches" [META] vs. "intersection +4 branches" [GEO]).

# Thus, I STICK TO THE TRISTRAM GRABENER VERSION TO AVOID ANY
# INCOMPATIBILITY BETWEEN YEARS/VERSIONS

# Bear in mind some variables do not appear exactly as in the metadata
# so I need to check for each specific dataset. Also, when there is a -1
# in these categorical variables, it is equivalent to a NA.

# ----------- CARACTERISTIQUES ------------ #

# list of variables
car_col <- colnames(acc17_car_paris)
car_col[which(car_col %in% colnames(acc_geo_paris))]

# list of variables with categories that I want:
# "lum", "agg", "int", "atm", "col"
car_col <- car_col[c(6:10)]

# All values appear for all variables
for (i in car_col) {
  print(acc_geo_paris %>% 
    select(i) %>% 
    unique())
}

# Assign the new values to the original datasets
for(i in c(17:20)){
  assign(sprintf("acc%s_car_paris",
                 as.character(i)),
         get(sprintf("acc%s_car_paris",
                     as.character(i))) %>% mutate(col = ifelse(col == 1,
                                                              "2 véhicules frontal",
                                                              ifelse(col == 2,
                                                                     "2 véhicules par l'arrière",
                                                                     ifelse(col == 3,
                                                                            "2 véhicules côté",
                                                                            ifelse(col == 4,
                                                                                   "3+ véhicules en chaine",
                                                                                   ifelse(col == 5,
                                                                                          "3+ véhicules autre",
                                                                                          ifelse(col == 6,
                                                                                                 "autre collision",
                                                                                                 ifelse(col == 7,
                                                                                                        "Sans collision", NA))))))),
                                                 atm = ifelse(atm == 1,
                                                              "météo normale",
                                                              ifelse(atm == 2,
                                                                     "pluie légère",
                                                                     ifelse(atm == 3,
                                                                            "pluie forte",
                                                                            ifelse(atm == 4,
                                                                                   "neige - grêle",
                                                                                   ifelse(atm == 5,
                                                                                          "brouillard - fumée",
                                                                                          ifelse(atm == 6,
                                                                                                 "vent fort - tempête",
                                                                                                 ifelse(atm == 7,
                                                                                                        "temps éblouissant",
                                                                                                        ifelse(atm == 8,
                                                                                                               "temps couvert",
                                                                                                               ifelse(atm == 9,
                                                                                                                      "autre météo", NA))))))))),
                                                 int = ifelse(int == 1,
                                                              "hors intersection",
                                                              ifelse(int == 2,
                                                                     "intersection en X",
                                                                     ifelse(int == 3,
                                                                            "intersection en T",
                                                                            ifelse(int == 4,
                                                                                   "intersection en Y",
                                                                                   ifelse(int == 5,
                                                                                          "intersection +4 branches",
                                                                                          ifelse(int == 6,
                                                                                                 "giratoire",
                                                                                                 ifelse(int == 7,
                                                                                                        "place",
                                                                                                        ifelse(int == 8,
                                                                                                               "passage à niveau",
                                                                                                               ifelse(int == 9,
                                                                                                                      "autre intersection", NA))))))))),
                                                 agg = ifelse(agg == 1,
                                                              "hors agglomération",
                                                              ifelse(agg == 2,
                                                                     "en agglomération",
                                                                     NA)),
                                                 lum = ifelse(lum == 1,
                                                              "plein jour",
                                                              ifelse(lum == 2,
                                                                     "crépuscule ou aube",
                                                                     ifelse(lum == 3,
                                                                            "nuit sans éclairage public",
                                                                            ifelse(lum == 4,
                                                                                   "nuit avec éclairage public non allumé",
                                                                                   ifelse(lum == 5,
                                                                                          "nuit avec éclairage public allumé", NA)))))))
}
 
# Check for missing values
lapply(acc17_car_paris, function(x) sum(is.na(x))) #no NAs
lapply(acc18, function(x) sum(is.na(x))) #no NAs
lapply(acc19, function(x) sum(is.na(x))) #no NAs
lapply(acc20, function(x) sum(is.na(x))) #no NAs

# ----------- LIEUX ------------ #

# list of variables
lie_col <- colnames(acc17_lie_paris)
lie_col[which(lie_col %in% colnames(acc_geo_paris))]

# list of variables with categories that I want:
# "catr", "circ", "vosp", "prof", "plan", "surf", "infra", "situ"
lie_col <- lie_col[c(2, 6, 10:12, 15:17)]

# Not all values appear for all variables
for (i in lie_col) {
  print(acc_geo_paris %>% 
          select(i) %>% 
          unique())
}

# After running the loop one time, I realize the data is inconsistent with the metadata, because there are no
# -1 in any variable (where -1 is meant to be the coding for NA), while 0s appear even when they are not
# classified as responding to any of the categories for certain variables. Thus, I need to check, for each variable
# whether a 0 is simply "other" or an NA, as this will make an impact on which observations are kept for regressions.


for(i in c(17:20)){
  assign(sprintf("acc%s_lie_paris",
                 as.character(i)),
         get(sprintf("acc%s_lie_paris",
                     as.character(i))) %>% 
  mutate(catr = ifelse(catr == 1,
                       "autoroute",
                       ifelse(catr == 2,
                              "route nationale",
                              ifelse(catr == 3,
                                     "route départamentale",
                                     ifelse(catr == 4,
                                            "voie communale",
                                            ifelse(catr == 5,
                                                   "hors réseau public",
                                                   ifelse(catr == 6,
                                                          "parking",
                                                          ifelse(catr == 7,
                                                                 "routes métropole urbaine",
                                                                 ifelse(catr == 9,
                                                                        "autre route", NA)))))))),
         circ = ifelse(circ == 1,
                       "sens unique",
                       ifelse(circ == 2,
                              "bidirectionnel",
                              ifelse(circ == 3,
                                     "chaussées séparées",
                                     ifelse(circ == 4,
                                            "affectation variable", NA)))), # 0s interpreted as NAs
         vosp = ifelse(vosp == 0,
                       "sans objet",
                       ifelse(vosp == 1,
                              "piste cyclable",
                              ifelse(vosp == 2,
                                     "bande cyclable",
                                     ifelse(vosp == 3,
                                            "voie réservée", NA)))), #0s interpreted as 0.
         prof = ifelse(prof == 1,
                       "plat",
                       ifelse(prof == 2,
                              "pente",
                              ifelse(prof == 3,
                                     "sommet de côte",
                                     ifelse(prof == 4,
                                            "bas de côte", NA)))), # 0s interpreted as NAs
         plan = ifelse(plan == 1,
                       "rectiligne",
                       ifelse(plan == 2,
                              "courbe à gauche",
                              ifelse(plan == 3,
                                     "courbe à droite",
                                     ifelse(plan == 4,
                                            "en S", NA)))), # 0s interpreted as NAs
         surf = ifelse(surf == 1,
                       "surface normale",
                       ifelse(surf == 2,
                              "surface mouillée",
                              ifelse(surf == 3,
                                     "flaques",
                                     ifelse(surf == 4,
                                            "surface innondée",
                                            ifelse(surf == 5,
                                                   "surface enneigée",
                                                   ifelse(surf == 6,
                                                          "boue",
                                                          ifelse(surf == 7,
                                                                 "surface verglacée",
                                                                 ifelse(surf == 8,
                                                                        "corps gras-huile",
                                                                        ifelse(surf == 9,
                                                                               "autre surface", NA))))))))), # 0s interpreted as NAs
         infra = ifelse(infra == 1,
                        "souterrain ou tunnel",
                        ifelse(infra == 2,
                               "pont",
                               ifelse(infra == 3,
                                      "échangeur",
                                      ifelse(infra == 4,
                                             "rails",
                                             ifelse(infra == 5,
                                                    "carrefour aménagé",
                                                    ifelse(infra == 6,
                                                           "zone poétonne",
                                                           ifelse(infra == 7,
                                                                  "zone de péage",
                                                                  ifelse(infra == 8,
                                                                         "chantier",
                                                                         ifelse(infra == 9,
                                                                                "autre",
                                                                                ifelse(infra == 0,
                                                                                       "none", NA)))))))))), # 0s as 0s
         situ = ifelse(situ == 1,
                       "chaussée",
                       ifelse(situ == 2,
                              "bande d'arrêt d'urgence",
                              ifelse(situ == 3,
                                     "accotement",
                                     ifelse(situ == 4,
                                            "trottoir",
                                            ifelse(situ == 5,
                                                   "piste cyclable",
                                                   ifelse(situ == 6,
                                                          "autre voie spéciale", NA)))))))) # 0s as NAs
  
}

# There is a typo in "vosp" which I correct, as Tristram says "banque cyclable"
# for the 2nd category, while the metadata says "bande cyclable".
acc_geo_paris <- acc_geo_paris %>% 
  mutate(vosp = ifelse(vosp == "banque cyclable",
                       "bande cyclable",
                       vosp))

# Check for missing values. In this case there are missing values
lapply(acc17_lie_paris, function(x) sum(is.na(x))) 
lapply(acc18_lie_paris, function(x) sum(is.na(x))) 
lapply(acc19_lie_paris, function(x) sum(is.na(x))) 
lapply(acc20_lie_paris, function(x) sum(is.na(x))) 


# ----------- USAGERS ------------ #

# list of variables
usa_col <- colnames(acc17_usa_paris)
usa_col[which(usa_col %in% colnames(acc_geo_paris))]

# The security variables are not included until 2019. These are not extremely relevant for this case so they
# will be dropped later on

# As there is no direct overlapping (because in the 2005-2016 database these variables have been
# modified after aggregation, as Num_Acc is not a primary key of the usagers database), I need to construct these
# variables differently

# Create these variables: n. of pedestrians, n. of non hurt, n.  of lightly hurt, n. of hospitalized,
# n. of killed, maximum gravity  -------- following Gräbener

# Also, create variables that are relevant for the community sphere aspect of accidents, related to the
# use of space: an_nais, sexe, locp, actp

# not all values appear for all variables
for (i in usa_col) {
  print(acc_geo_paris %>% 
          select(i) %>% 
          unique())
}

# Here, missing values are ignored as they do not make an impact in the aggregated measures
for(i in c(17:20)){
  assign(sprintf("acc%s_usa_paris",
                 as.character(i)),
         get(sprintf("acc%s_usa_paris",
                     as.character(i))) %>% 
  group_by(Num_Acc) %>% 
  summarise(pietons_nb = sum(catu == 3),
            has_pietons = ifelse(max(catu) == 3,
                                 1, 0),
            indemne_nb = sum(grav == 1),
            blesseleger_nb = sum(grav == 2),
            hospitalise_nb = sum(grav == 3),
            tue_nb = sum(grav == 4),
            gravite_accident = max(grav),
            traversant = ifelse(max(actp, na.rm = FALSE) == 3,
                                1,
                                ifelse(is.na(max(actp)),
                                       NA,
                                       0)),
            jouant = ifelse(max(actp, na.rm = FALSE) == 5,
                            1,
                            ifelse(is.na(max(actp)),
                                   NA,
                                   0))) %>% 
  mutate(gravite_accident = ifelse(gravite_accident == 2,
                                   "blessé léger",
                                   ifelse(gravite_accident == 3,
                                          "hospitalisé",
                                          ifelse(gravite_accident == 4,
                                                 "tué", NA)))))
}


# Exploring the feasibility of using "community-related variables" I eventually just make two dummies:
# one for the pedestrian involved crossing the road and another one for him playing.

# When subsetting just those users who are pedestrians (for which the community-related variables
# are available), we get that Num_Acc is not a primary key, meaning there are some accidents where 
# more than one 


# ----------- VÉHICULES ------------ #

acc_geo_paris$catr %>% 
  unique()

# list of variables
veh_col <- colnames(acc17_veh_paris)
veh_col[which(veh_col %in% colnames(acc_geo_paris))] # again, no direct overlapping

# list of variables created with them by Gräbener:
# "voiture_nb", "has_voiture", "deuxrouesmotorises_nb", "has_deuxrouesmotorises", "velo_nb",
# "has_velo", "poidslourd_nb", "has_poidslourd", "vehiculeautre_nb", "has_vehiculeautre"


# list of variables with categories that I want in addition:
# 


# All values appear for all variables
for (i in veh_col) {
  print(acc_geo_paris %>% 
          select(i) %>% 
          unique())
}


# No missing values in catv so no need to take decisions discretionarily.
for(i in c(17:20)){
  assign(sprintf("acc%s_veh_paris",
                 as.character(i)),
         get(sprintf("acc%s_veh_paris",
                     as.character(i))) %>% 
           group_by(Num_Acc) %>% 
           summarise(voiture_nb = sum(catv == "07" | catv == "10"),
                     has_voiture = ifelse(voiture_nb > 0,
                                          1,
                                          0),
                     deuxrouesmotorises_nb = sum(catv == "02" | catv == "30" |
                                                   catv == "31" |catv == "32" |
                                                   catv == "33" |catv == "34"),
                     has_deuxrouesmotorises = ifelse(deuxrouesmotorises_nb > 0,
                                                    1,
                                                    0),
                     velo_nb = sum(catv == "01"),
                     has_velo = ifelse(velo_nb > 0,
                                       1,
                                       0),
                     poidslourd_nb = sum(catv == "13" |catv == "14" |
                                           catv == "15" |catv == "16" |
                                           catv == "17" |catv == "37" |
                                           catv == "38"),
                     has_poidslourd = ifelse(poidslourd_nb > 0,
                                             1,
                                             0),
                     vehiculeautre_nb = sum(catv == "99"),
                     has_vehiculeautre = ifelse(vehiculeautre_nb > 0,
                                                1,
                                                0)))
}

################################
### 6. MERGING THE DATABASES ###
################################

# Now, all 4 databases are uniquely identified by "Num_Acc", have the same number of observations
# (as usagers and véhicules have been grouped at the accident level) and can thus be aggregated.

acc17 <- acc17_car_paris %>% 
  full_join(acc17_lie_paris, by = "Num_Acc") %>% 
  full_join(acc17_usa_paris, by = "Num_Acc") %>% 
  full_join(acc17_veh_paris, by = "Num_Acc")

acc18 <- acc18_car_paris %>% 
  full_join(acc18_lie_paris, by = "Num_Acc") %>% 
  full_join(acc18_usa_paris, by = "Num_Acc") %>% 
  full_join(acc18_veh_paris, by = "Num_Acc")

acc19 <- acc19_car_paris %>% 
  full_join(acc19_lie_paris, by = "Num_Acc") %>% 
  full_join(acc19_usa_paris, by = "Num_Acc") %>% 
  full_join(acc19_veh_paris, by = "Num_Acc")

acc20 <- acc20_car_paris %>% 
  full_join(acc20_lie_paris, by = "Num_Acc") %>% 
  full_join(acc20_usa_paris, by = "Num_Acc") %>% 
  full_join(acc20_veh_paris, by = "Num_Acc")


###### NOW, check column names for all of the databases to see if there is any incompatibility
unique(colnames(acc17) == colnames(acc18)) # 2017 and 2018 are equal initially
unique(sort(colnames(acc19))  == sort(colnames(acc20))) # 2019 and 2020 are equal initially

colnames(acc19)
colnames(acc17)
colnames(acc17)[which(!(colnames(acc17) %in% colnames(acc20)))] # just gps and env1
colnames(acc20)[which(!(colnames(acc20) %in% colnames(acc17)))] # just vma

# As none of the three non-overlapping columns are relevant, I decide to drop all of them
acc17 <- acc17 %>% 
  select(-c("gps", "env1", "dep"))
acc18 <- acc18 %>% 
  select(-c("gps", "env1", "dep"))
acc19 <- acc19 %>% 
  select(-c("vma", "dep"))
acc20 <- acc20 %>% 
  select(-c("vma", "dep"))

# Now all of them are equal 
unique(sort(colnames(acc17))  == sort(colnames(acc19)))


# Now clean the geocoded data for 2005-2016 as well
colnames(acc17_20)
# Original variable names are "an", "mois", "jour", "hrmn"
colnames(acc_geo_paris)
# Gräbener names are "mois" (done), "date" (without hour but whatever), "date_formated" (done), "heures_minutes" (done),
# "ANNEE" (done), "SEMAINE" (done), "LIBELLE_JOUR" (nope), "HEURE" (done), "LIBELLE_PLAGE_HORAIRE" (removed)

# Start by creating the relevant date variables in 2017 and 2020
acc17 <- acc17 %>% 
  mutate(heure = ifelse(hrmn < 1000,
                        as.integer(str_sub(as.character(hrmn), 1, 1)),
                        as.integer(str_sub(as.character(hrmn), 1, 2))),
         date_formated = paste(ifelse(jour < 10,
                                      paste0("0", as.character(jour)),
                                      as.character(jour)),
                                ifelse(mois < 10,
                                       paste0("0", as.character(mois)),
                                       as.character(mois)),
                               paste0("20",as.character(an)),
                                sep = "/"),
         
         date = as.Date(date_formated, "%d/%m/%Y"),
         heures_minutes = ifelse(hrmn < 10,
                                 paste("00",
                                       paste0("0",str_sub(hrmn, 1, 1)),
                                       "00",
                                       sep = ":"),
                                 ifelse(hrmn >= 10 & hrmn < 100,
                                        paste("00",
                                              str_sub(hrmn, 1, 2),
                                              "00",
                                              sep = ":"),
                                        ifelse(hrmn >= 100 & hrmn < 1000,
                                               paste(paste0("0", str_sub(hrmn, 1, 1)),
                                                     str_sub(hrmn, 2, 3),
                                                     "00",
                                                     sep = ":"),
                                               paste(str_sub(as.character(hrmn), 1, 2),
                                                     str_sub(as.character(hrmn), 3, 4),
                                                     "00",
                                                     sep = ":")))),
         semaine = as.integer(strftime(date, format = "%W")),
         jour_semaine = weekdays(as.Date(date, "%Y/%m/%d")),
         .after = hrmn) %>% 
  select(-hrmn)

acc18 <- acc18 %>% 
  mutate(heure = ifelse(hrmn < 1000,
                        as.integer(str_sub(as.character(hrmn), 1, 1)),
                        as.integer(str_sub(as.character(hrmn), 1, 2))),
         date_formated = paste(ifelse(jour < 10,
                                      paste0("0", as.character(jour)),
                                      as.character(jour)),
                               ifelse(mois < 10,
                                      paste0("0", as.character(mois)),
                                      as.character(mois)),
                               paste0("20",as.character(an)),
                               sep = "/"),
         
         date = as.Date(date_formated, "%d/%m/%Y"),
         heures_minutes = ifelse(hrmn < 10,
                                 paste("00",
                                       paste0("0",str_sub(hrmn, 1, 1)),
                                       "00",
                                       sep = ":"),
                                 ifelse(hrmn >= 10 & hrmn < 100,
                                 paste("00",
                                       str_sub(hrmn, 1, 2),
                                       "00",
                                       sep = ":"),
                                 ifelse(hrmn >= 100 & hrmn < 1000,
                                 paste(paste0("0", str_sub(hrmn, 1, 1)),
                                       str_sub(hrmn, 2, 3),
                                       "00",
                                       sep = ":"),
                                 paste(str_sub(as.character(hrmn), 1, 2),
                                       str_sub(as.character(hrmn), 3, 4),
                                       "00",
                                       sep = ":")))),
         semaine = as.integer(strftime(date, format = "%W")),
         jour_semaine = weekdays(as.Date(date, "%Y/%m/%d")),
         .after = hrmn) %>% 
  select(-hrmn)


# 2019 and 2020 have the right format for the hour
acc19 <- acc19 %>% 
  mutate(heure = as.integer(str_sub(as.character(hrmn),
                                    1,
                                    2)),
         date_formated = paste(ifelse(jour < 10,
                                      paste0("0", as.character(jour)),
                                      as.character(jour)),
                               ifelse(mois < 10,
                                      paste0("0", as.character(mois)),
                                      as.character(mois)),
                                      as.character(an),
                               sep = "/"),
         date = as.Date(date_formated, "%d/%m/%Y"),
         semaine = as.integer(strftime(date,format = "%W")),
         jour_semaine = weekdays(as.Date(date, "%Y/%m/%d")),
         .after = hrmn) %>% 
  rename(heures_minutes = hrmn)

acc20 <- acc20 %>% 
  mutate(heure = as.integer(str_sub(as.character(hrmn),
                                    1,
                                    2)),
         date_formated = paste(ifelse(jour < 10,
                                      paste0("0", as.character(jour)),
                                      as.character(jour)),
                               ifelse(mois < 10,
                                      paste0("0", as.character(mois)),
                                      as.character(mois)),
                               as.character(an),
                               sep = "/"),
         date = as.Date(date_formated, "%d/%m/%Y"),
         semaine = as.integer(strftime(date,format = "%W")),
         jour_semaine = weekdays(as.Date(date, "%Y/%m/%d")),
         .after = hrmn) %>% 
  rename(heures_minutes = hrmn)


# The dataset columns are identical now
sort(unique(colnames(acc17))) == sort(unique(colnames(acc20)))

# Save the files
write_csv(acc17, "accidents/2017/final17.csv")
write_csv(acc18, "accidents/2018/final18.csv")
write_csv(acc19, "accidents/2019/final19.csv")
write_csv(acc20, "accidents/2020/final20.csv")
 
# Create & save the joint dataframe for all years 2017-2020
acc17_20 <- do.call("rbind", list(acc17, acc18, acc19, acc20)) %>% 
  select(-c(com))
write_csv(acc17_20, "accidents/final/final17_20.csv")

############################ FINAL MERGE ########################

# Rename variables in Gräbener's version
acc_geo_paris <- acc_geo_paris %>% 
  rename(an = ANNEE,
         semaine = SEMAINE,
         jour_semaine = LIBELLE_JOUR,
         heure = HEURE,
         lat = latitude,
         long = longitude) %>% 
  mutate(jour = as.integer(str_sub(date_formated,
                                   1,
                                   2)),
         heures_minutes = as.character(heures_minutes),
         date = as.Date(date),
         .after = mois) 
  
acc_geo_paris <- acc_geo_paris  %>% 
  select(colnames(acc_geo_paris)[which(colnames(acc_geo_paris) %in% colnames(acc17_20))])

colnames(acc17_20)[which(!(colnames(acc17_20) %in% colnames(acc_geo_paris)))]


acc05_16 <- acc_geo_paris
rm(acc_geo_paris)
# Save the file
write_csv(acc05_16, "accidents/final/final05_16.csv")

# Just two variables do not appear in Gräbener's version and appear in mine
# traversant" and "jouant" (which I created as community-related variables)

# Thus, I remove them for the join
acc17_20 <- acc17_20 %>% 
  select(-c(traversant, jouant))



################ JOIN EVERYTHING ################
acc <- rbind(acc05_16, acc17_20)

# Some final problems, mostly related to FORMAT AND COORDINATES

# Year format
acc <- acc %>% 
  mutate(an = ifelse(an == 17,
                     2017,
                     ifelse(an == 18,
                            2018,
                            an)))

# And most importantly, coordinates

# Replace the dots by "_", then split the data that still has inconsistencies

acc <- acc %>% 
  mutate(latitude1 = str_replace_all(lat,
                                     "[.]",
                                     "\\_"),
         longitude1 = str_replace_all(long,
                                      "[.]",
                                      "\\_"))

# Split 2020
acc1 <- acc %>% 
  filter(str_detect(acc$longitude1,","))

# The rest
acc2 <- acc %>% 
  filter(!(str_detect(acc$longitude1,",")))


# Have a view of them
view(acc1[,c("latitude1", "longitude1")])
view(acc2[,c("latitude1", "longitude1")])

# Like this, missing values are left out
sum(is.na(acc1$longitude1))
sum(is.na(acc2$longitude1))
dim(acc1)[1] + dim(acc2)[1] # 85,538, the number of non-missing coordinates


# Start with acc1
acc1 <- acc1 %>% 
  mutate(latitude1 = str_replace_all(latitude1,
                                     "[,]",
                                     "\\_"),
         longitude1 = str_replace_all(longitude1,
                                      "[,]",
                                      "\\_"))

# Check - every observation is now set to have a "_"
sum(str_detect(acc1$longitude1, "_")) # yep
sum(str_detect(acc1$latitude1, "_")) # same

# Now, create the right value
acc1 <- acc1 %>% 
  mutate(longitude = paste0(str_sub(longitude1,
                                    1,
                                    1),
                            ".",
                            str_sub(longitude1,
                                    3,
                                    str_length(longitude1))),
         latitude = paste0(str_sub(latitude1,
                                    1,
                                    2),
                            ".",
                            str_sub(latitude1,
                                    4,
                                    str_length(latitude1))))

view(acc1[,c("latitude1", "longitude1",
             "latitude", "longitude")])

# Drop the intermediate variables
acc1 <- acc1 %>% 
  select(-c(longitude1, latitude1))

# 2020 IS SET __________________________

# Now, back to acc2

# First, get rid of the zeroes at the beginning of longitude
acc2 <- acc2 %>% 
  mutate(longitude2 = ifelse(str_sub(longitude1,
                                     1,
                                     1) == "0" & str_sub(longitude1,
                                                         2,
                                                         2) != "0",
                             str_sub(longitude1,
                                     2,
                                     str_length(longitude1)),
                             ifelse(str_sub(longitude1,
                                            1,
                                            2) == "00",
                                    str_sub(longitude1,
                                            3,
                                            str_length(longitude1)),
                                    longitude1)))

view(acc2[,c("latitude1", "longitude1", "longitude2")])

# CHECKED
sum(str_sub(acc2$longitude2, 1, 1) == "0") # 0 obs.

# Split another time acc2
# With "_"
acc2_1 <- acc2 %>% 
  filter(str_detect(longitude2, "\\_")) 

# Without "_"
acc2_2 <- acc2 %>% 
  filter(!(str_detect(longitude2, "\\_")) )

# Using the ones with "_" first
acc2_1 <- acc2_1 %>% 
  mutate(longitude = str_replace(longitude2,
                                 "\\_",
                                 "."),
         latitude = str_replace(latitude1,
                                 "\\_",
                                 "."))

view(acc2_1[,c("latitude1", "longitude1",
             "latitude", "longitude")])

# acc2_1 is set ____________________-

view(acc2_2[,c("latitude1", "longitude2")])

# Now, back to the ones without "_"
acc2_2 <- acc2_2 %>% 
  mutate(longitude = paste0(str_sub(longitude2,
                                    1,
                                    1),
                            ".",
                            str_sub(longitude2,
                                    2,
                                    str_length(longitude2))),
         latitude = paste0(str_sub(latitude1,
                                   1,
                                   2),
                           ".",
                           str_sub(latitude1,
                                   3,
                                   str_length(latitude1))))

view(acc2_2[,c("latitude1", "longitude2",
               "latitude", "longitude")])
# acc2_2 is set ____________________________-

acc2_1 <- acc2_1 %>% 
  select(-c(latitude1, longitude1, longitude2))

acc2_2 <- acc2_2 %>% 
  select(-c(latitude1, longitude1, longitude2))

# Now, do a rbind() and eventually add the missing values
acc_nonna <- rbind(acc1, acc2_1, acc2_2)
colnames(acc) # intermediate steps are effectively removed
view(acc[,c("lat", "long",
               "latitude", "longitude")])

# Now, add those accidents which have missing coordinates
# Create two (empty) variables
acc <- acc %>% 
  mutate(latitude = NA,
         longitude = NA)

acc_final <- rbind(acc_nonna, acc[!(acc$Num_Acc %in% acc_nonna$Num_Acc),])

view(acc_final[,c("lat", "long", "latitude", "longitude")])

# Finally, sort the data by date
acc_final <- acc_final %>% 
  arrange(date)

### FINALLY, SAVE THE DATABASE
write_csv(acc_final, "accidents/final/accidents_final.csv")



