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


# Set working directory
setwd("/Users/unaioyon/Desktop/masters_thesis/data/fra")

options(scipen = 999)


##############################
##### NON-SPATIAL DATA  ######    ----- DATA FORMATTING, CAN BE IGNORED NOW
##############################

# 2005
rev05 <- read_xlsx("iris/data/2005/revenus.xlsx",
                  sheet = 2) # identifier: "IRIS"

rev05paris <- rev05 %>% 
  filter(DEP == "75")

# Save Paris
write_csv(rev05paris, "iris/data/2005/revenus_paris.csv")

# 2006
# Revenus
rev06 <- read_xlsx("iris/data/2006/revenus.xlsx",
                   sheet = 2) # identifier: "IRIS"

rev06paris <- rev06 %>% 
  filter(DEP == "75")

# Activités
act06 <- read_xlsx("iris/data/2006/activite.xlsx",
                   sheet = 1) # identifier: "IRIS" 

act06paris <- act06 %>% 
  filter(DEP == "75") # 992

# Logements
log06 <- read_xls("iris/data/2006/logement.xls",
                   sheet = 1)# identifier: "IRIS"

log06 <- log06[c(5:nrow(log06)),] %>% 
  row_to_names(row_number = 1)
  
log06paris <- log06 %>% 
  filter(DEP == "75") # 992

# Formation
for06 <- read_xls("iris/data/2006/formation.xls",
                  sheet = 1)# identifier: "IRIS"

for06 <- for06[c(5:nrow(for06)),] %>% 
  row_to_names(row_number = 1)

for06paris <- for06 %>% 
  filter(DEP == "75") # 992

# Population
pop06 <- read_xls("iris/data/2006/population.xls",
                  sheet = 1)# identifier: "IRIS"

pop06 <- pop06[c(5:nrow(pop06)),] %>% 
  row_to_names(row_number = 1)

pop06paris <- pop06 %>% 
  filter(DEP == "75") # 992

# Familles
fam06 <- read_xls("iris/data/2006/familles.xls",
                  sheet = 1)# identifier: "IRIS"

fam06 <- fam06[c(5:nrow(fam06)),] %>% 
  row_to_names(row_number = 1)

fam06paris <- fam06 %>% 
  filter(DEP == "75") # 992

# Merge into a single file
paris06 <- rev06paris %>% 
  full_join(act06paris, by = "IRIS") %>% 
  full_join(log06paris, by = "IRIS") %>%
  full_join(for06paris, by = "IRIS") %>%
  full_join(pop06paris, by = "IRIS") %>%
  full_join(fam06paris, by = "IRIS") # 992


# Drop the previous datasets to save memory
rm(list = c("rev06", "act06", "log06", "for06", "pop06", "fam06"))

# Save Paris
write_csv(rev06paris, "iris/data/2006/revenus_paris.csv")
write_csv(act06paris, "iris/data/2006/activite_paris.csv")
write_csv(log06paris, "iris/data/2006/logement_paris.csv")
write_csv(for06paris, "iris/data/2006/formation_paris.csv")
write_csv(pop06paris, "iris/data/2006/population_paris.csv")
write_csv(fam06paris, "iris/data/2006/familles_paris.csv")

# Save joint Paris
write_csv(paris06, "iris/data/2006/paris06.csv")

# Drop the Paris datasets as well
rm(list = c("rev06paris", "act06paris", "log06paris",
            "for06paris", "pop06paris", "fam06paris",
            "paris06"))


# 2007
# Revenus
rev07 <- read_xls("iris/data/2007/revenus.xls",
                   sheet = 2) # identifier: "IRIS"

rev07 <- rev07[c(6:nrow(rev07)),] %>% 
  row_to_names(row_number = 1)

rev07paris <- rev07 %>% 
  filter(DEP == "75")

# Activités
act07 <- read_xls("iris/data/2007/activite.xls",
                   sheet = 1) # identifier: "IRIS" 

act07 <- act07[c(5:nrow(act07)),] %>% 
  row_to_names(row_number = 1)

act07paris <- act07 %>% 
  filter(DEP == "75") # 992

# Logements
log07 <- read_xls("iris/data/2007/logement.xls",
                  sheet = 1)# identifier: "IRIS"

log07 <- log07[c(5:nrow(log07)),] %>% 
  row_to_names(row_number = 1)

log07paris <- log07 %>% 
  filter(DEP == "75") # 992

# Formation
for07 <- read_xls("iris/data/2007/formation.xls",
                  sheet = 1)# identifier: "IRIS"

for07 <- for07[c(5:nrow(for07)),] %>% 
  row_to_names(row_number = 1)

for07paris <- for07 %>% 
  filter(DEP == "75") # 992

# Population
pop07 <- read_xls("iris/data/2007/population.xls",
                  sheet = 1)# identifier: "IRIS"

pop07 <- pop07[c(5:nrow(pop07)),] %>% 
  row_to_names(row_number = 1)

pop07paris <- pop07 %>% 
  filter(DEP == "75") # 992

# Familles
fam07 <- read_xls("iris/data/2007/familles.xls",
                  sheet = 1)# identifier: "IRIS"

fam07 <- fam07[c(5:nrow(fam07)),] %>% 
  row_to_names(row_number = 1)

fam07paris <- fam07 %>% 
  filter(DEP == "75") # 992

# Merge into a single file
paris07 <- rev07paris %>% 
  full_join(act07paris, by = "IRIS") %>% 
  full_join(log07paris, by = "IRIS") %>%
  full_join(for07paris, by = "IRIS") %>%
  full_join(pop07paris, by = "IRIS") %>%
  full_join(fam07paris, by = "IRIS") # 992


# Drop the previous datasets to save memory
rm(list = c("rev07", "act07", "log07", "for07", "pop07", "fam07"))

# Save Paris
write_csv(rev07paris, "iris/data/2007/revenus_paris.csv")
write_csv(act07paris, "iris/data/2007/activite_paris.csv")
write_csv(log07paris, "iris/data/2007/logement_paris.csv")
write_csv(for07paris, "iris/data/2007/formation_paris.csv")
write_csv(pop07paris, "iris/data/2007/population_paris.csv")
write_csv(fam07paris, "iris/data/2007/familles_paris.csv")

# Save joint Paris
write_csv(paris07, "iris/data/2007/paris07.csv")

# Drop the Paris datasets as well
rm(list = c("rev07paris", "act07paris", "log07paris",
            "for07paris", "pop07paris", "fam07paris",
            "paris07"))


# 2008
# Revenus
rev08 <- read_xls("iris/data/2008/revenus.xls",
                  sheet = 2) # identifier: "IRIS"

rev08 <- rev08[c(6:nrow(rev08)),] %>% 
  row_to_names(row_number = 1)

rev08paris <- rev08 %>% 
  filter(DEP == "75")

# Activités
act08 <- read_xls("iris/data/2008/activite.xls",
                  sheet = 1) # identifier: "IRIS" 

act08 <- act08[c(5:nrow(act08)),] %>% 
  row_to_names(row_number = 1)

act08paris <- act08 %>% 
  filter(DEP == "75") # 992

# Logements
log08 <- read_xls("iris/data/2008/logement.xls",
                  sheet = 1)# identifier: "IRIS"

log08 <- log08[c(5:nrow(log08)),] %>% 
  row_to_names(row_number = 1)

log08paris <- log08 %>% 
  filter(DEP == "75") # 992

# Formation
for08 <- read_xls("iris/data/2008/formation.xls",
                  sheet = 1)# identifier: "IRIS"

for08 <- for08[c(5:nrow(for08)),] %>% 
  row_to_names(row_number = 1)

for08paris <- for08 %>% 
  filter(DEP == "75") # 992

# Population
pop08 <- read_xls("iris/data/2008/population.xls",
                  sheet = 1)# identifier: "IRIS"

pop08 <- pop08[c(5:nrow(pop08)),] %>% 
  row_to_names(row_number = 1)

pop08paris <- pop08 %>% 
  filter(DEP == "75") # 992

# Familles
fam08 <- read_xls("iris/data/2008/familles.xls",
                  sheet = 1)# identifier: "IRIS"

fam08 <- fam08[c(5:nrow(fam08)),] %>% 
  row_to_names(row_number = 1)

fam08paris <- fam08 %>% 
  filter(DEP == "75") # 992

# Merge into a single file
paris08 <- rev08paris %>% 
  full_join(act08paris, by = "IRIS") %>% 
  full_join(log08paris, by = "IRIS") %>%
  full_join(for08paris, by = "IRIS") %>%
  full_join(pop08paris, by = "IRIS") %>%
  full_join(fam08paris, by = "IRIS") # 992


# Drop the previous datasets to save memory
rm(list = c("rev08", "act08", "log08", "for08", "pop08", "fam08"))

# Save Paris
write_csv(rev08paris, "iris/data/2008/revenus_paris.csv")
write_csv(act08paris, "iris/data/2008/activite_paris.csv")
write_csv(log08paris, "iris/data/2008/logement_paris.csv")
write_csv(for08paris, "iris/data/2008/formation_paris.csv")
write_csv(pop08paris, "iris/data/2008/population_paris.csv")
write_csv(fam08paris, "iris/data/2008/familles_paris.csv")

# Save joint Paris
write_csv(paris08, "iris/data/2008/paris08.csv")

# Drop the Paris datasets as well
rm(list = c("rev08paris", "act08paris", "log08paris",
            "for08paris", "pop08paris", "fam08paris",
            "paris08"))


# 2009
# Revenus
rev09 <- read_xls("iris/data/2009/revenus.xls",
                  sheet = 2) # identifier: "IRIS"

rev09 <- rev09[c(6:nrow(rev09)),] %>% 
  row_to_names(row_number = 1)

rev09paris <- rev09 %>% 
  filter(DEP == "75")

# Activités
act09 <- read_xls("iris/data/2009/activite.xls",
                  sheet = 1) # identifier: "IRIS" 

act09 <- act09[c(5:nrow(act09)),] %>% 
  row_to_names(row_number = 1)

act09paris <- act09 %>% 
  filter(DEP == "75") # 992

# Logements
log09 <- read_xls("iris/data/2009/logement.xls",
                  sheet = 1)# identifier: "IRIS"

log09 <- log09[c(5:nrow(log09)),] %>% 
  row_to_names(row_number = 1)

log09paris <- log09 %>% 
  filter(DEP == "75") # 992

# Formation
for09 <- read_xls("iris/data/2009/formation.xls",
                  sheet = 1)# identifier: "IRIS"

for09 <- for09[c(5:nrow(for09)),] %>% 
  row_to_names(row_number = 1)

for09paris <- for09 %>% 
  filter(DEP == "75") # 992

# Population
pop09 <- read_xls("iris/data/2009/population.xls",
                  sheet = 1)# identifier: "IRIS"

pop09 <- pop09[c(5:nrow(pop09)),] %>% 
  row_to_names(row_number = 1)

pop09paris <- pop09 %>% 
  filter(DEP == "75") # 992

# Familles
fam09 <- read_xls("iris/data/2009/familles.xls",
                  sheet = 1)# identifier: "IRIS"

fam09 <- fam09[c(5:nrow(fam09)),] %>% 
  row_to_names(row_number = 1)

fam09paris <- fam09 %>% 
  filter(DEP == "75") # 992

# Merge into a single file
paris09 <- rev09paris %>% 
  full_join(act09paris, by = "IRIS") %>% 
  full_join(log09paris, by = "IRIS") %>%
  full_join(for09paris, by = "IRIS") %>%
  full_join(pop09paris, by = "IRIS") %>%
  full_join(fam09paris, by = "IRIS") # 992


# Drop the previous datasets to save memory
rm(list = c("rev09", "act09", "log09", "for09", "pop09", "fam09"))

# Save Paris
write_csv(rev09paris, "iris/data/2009/revenus_paris.csv")
write_csv(act09paris, "iris/data/2009/activite_paris.csv")
write_csv(log09paris, "iris/data/2009/logement_paris.csv")
write_csv(for09paris, "iris/data/2009/formation_paris.csv")
write_csv(pop09paris, "iris/data/2009/population_paris.csv")
write_csv(fam09paris, "iris/data/2009/familles_paris.csv")

# Save joint Paris
write_csv(paris09, "iris/data/2009/paris09.csv")

# Drop the Paris datasets as well
rm(list = c("rev09paris", "act09paris", "log09paris",
            "for09paris", "pop09paris", "fam09paris",
            "paris09"))


# 2010
# Revenus
rev10 <- read_xls("iris/data/2010/revenus.xls",
                  sheet = 2) # identifier: "IRIS"

rev10 <- rev10[c(6:nrow(rev10)),] %>% 
  row_to_names(row_number = 1)

rev10paris <- rev10 %>% 
  filter(DEP == "75")

# Activités
act10 <- read_xls("iris/data/2010/activite.xls",
                  sheet = 1) # identifier: "IRIS" 

act10 <- act10[c(5:nrow(act10)),] %>% 
  row_to_names(row_number = 1)

act10paris <- act10 %>% 
  filter(DEP == "75") # 992

# Logements
log10 <- read_xls("iris/data/2010/logement.xls",
                  sheet = 1)# identifier: "IRIS"

log10 <- log10[c(5:nrow(log10)),] %>% 
  row_to_names(row_number = 1)

log10paris <- log10 %>% 
  filter(DEP == "75") # 992

# Formation
for10 <- read_xls("iris/data/2010/formation.xls",
                  sheet = 1)# identifier: "IRIS"

for10 <- for10[c(5:nrow(for10)),] %>% 
  row_to_names(row_number = 1)

for10paris <- for10 %>% 
  filter(DEP == "75") # 992

# Population
pop10 <- read_xls("iris/data/2010/population.xls",
                  sheet = 1)# identifier: "IRIS"

pop10 <- pop10[c(5:nrow(pop10)),] %>% 
  row_to_names(row_number = 1)

pop10paris <- pop10 %>% 
  filter(DEP == "75") # 992

# Familles
fam10 <- read_xls("iris/data/2010/familles.xls",
                  sheet = 1)# identifier: "IRIS"

fam10 <- fam10[c(5:nrow(fam10)),] %>% 
  row_to_names(row_number = 1)

fam10paris <- fam10 %>% 
  filter(DEP == "75") # 992

# Merge into a single file
paris10 <- rev10paris %>% 
  full_join(act10paris, by = "IRIS") %>% 
  full_join(log10paris, by = "IRIS") %>%
  full_join(for10paris, by = "IRIS") %>%
  full_join(pop10paris, by = "IRIS") %>%
  full_join(fam10paris, by = "IRIS") # 992


# Drop the previous datasets to save memory
rm(list = c("rev10", "act10", "log10", "for10", "pop10", "fam10"))

# Save Paris
write_csv(rev10paris, "iris/data/2010/revenus_paris.csv")
write_csv(act10paris, "iris/data/2010/activite_paris.csv")
write_csv(log10paris, "iris/data/2010/logement_paris.csv")
write_csv(for10paris, "iris/data/2010/formation_paris.csv")
write_csv(pop10paris, "iris/data/2010/population_paris.csv")
write_csv(fam10paris, "iris/data/2010/familles_paris.csv")

# Save joint Paris
write_csv(paris10, "iris/data/2010/paris10.csv")

# Drop the Paris datasets as well
rm(list = c("rev10paris", "act10paris", "log10paris",
            "for10paris", "pop10paris", "fam10paris",
            "paris10"))


# 2011
# Revenus
rev11 <- read_xls("iris/data/2011/revenus.xls",
                  sheet = 2) # identifier: "IRIS"

rev11 <- rev11[c(6:nrow(rev11)),] %>% 
  row_to_names(row_number = 1)

rev11paris <- rev11 %>% 
  filter(DEP == "75")

# Activités
act11 <- read_xls("iris/data/2011/activite.xls",
                  sheet = 1) # identifier: "IRIS" 

act11 <- act11[c(5:nrow(act11)),] %>% 
  row_to_names(row_number = 1)

act11paris <- act11 %>% 
  filter(DEP == "75") # 992

# Logements
log11 <- read_xls("iris/data/2011/logement.xls",
                  sheet = 1)# identifier: "IRIS"

log11 <- log11[c(5:nrow(log11)),] %>% 
  row_to_names(row_number = 1)

log11paris <- log11 %>% 
  filter(DEP == "75") # 992

# Formation
for11 <- read_xls("iris/data/2011/formation.xls",
                  sheet = 1)# identifier: "IRIS"

for11 <- for11[c(5:nrow(for11)),] %>% 
  row_to_names(row_number = 1)

for11paris <- for11 %>% 
  filter(DEP == "75") # 992

# Population
pop11 <- read_xls("iris/data/2011/population.xls",
                  sheet = 1)# identifier: "IRIS"

pop11 <- pop11[c(5:nrow(pop11)),] %>% 
  row_to_names(row_number = 1)

pop11paris <- pop11 %>% 
  filter(DEP == "75") # 992

# Familles
fam11 <- read_xls("iris/data/2011/familles.xls",
                  sheet = 1)# identifier: "IRIS"

fam11 <- fam11[c(5:nrow(fam11)),] %>% 
  row_to_names(row_number = 1)

fam11paris <- fam11 %>% 
  filter(DEP == "75") # 992

# Merge into a single file
paris11 <- rev11paris %>% 
  full_join(act11paris, by = "IRIS") %>% 
  full_join(log11paris, by = "IRIS") %>%
  full_join(for11paris, by = "IRIS") %>%
  full_join(pop11paris, by = "IRIS") %>%
  full_join(fam11paris, by = "IRIS") # 992


# Drop the previous datasets to save memory
rm(list = c("rev11", "act11", "log11", "for11", "pop11", "fam11"))

# Save Paris
write_csv(rev11paris, "iris/data/2011/revenus_paris.csv")
write_csv(act11paris, "iris/data/2011/activite_paris.csv")
write_csv(log11paris, "iris/data/2011/logement_paris.csv")
write_csv(for11paris, "iris/data/2011/formation_paris.csv")
write_csv(pop11paris, "iris/data/2011/population_paris.csv")
write_csv(fam11paris, "iris/data/2011/familles_paris.csv")

# Save joint Paris
write_csv(paris11, "iris/data/2011/paris11.csv")

# Drop the Paris datasets as well
rm(list = c("rev11paris", "act11paris", "log11paris",
            "for11paris", "pop11paris", "fam11paris",
            "paris11"))


# 2012
# Revenus
rev12 <- read_xls("iris/data/2012/revenus.xls",
                  sheet = 1) # identifier: "IRIS"

rev12 <- rev12[c(5:nrow(rev12)),] %>% 
  row_to_names(row_number = 1)

rev12paris <- rev12 %>% 
  filter(str_sub(IRIS, 1, 2) == "75") # 853

# Activités
act12 <- read_xls("iris/data/2012/activite.xls",
                  sheet = 1) # identifier: "IRIS" 

act12 <- act12[c(5:nrow(act12)),] %>% 
  row_to_names(row_number = 1)

act12paris <- act12 %>% 
  filter(DEP == "75") # 992

# Logements
log12 <- read_xls("iris/data/2012/logement.xls",
                  sheet = 1)# identifier: "IRIS"

log12 <- log12[c(5:nrow(log12)),] %>% 
  row_to_names(row_number = 1)

log12paris <- log12 %>% 
  filter(DEP == "75") # 992

# Formation
for12 <- read_xls("iris/data/2012/formation.xls",
                  sheet = 1)# identifier: "IRIS"

for12 <- for12[c(5:nrow(for12)),] %>% 
  row_to_names(row_number = 1)

for12paris <- for12 %>% 
  filter(DEP == "75") # 992

# Population
pop12 <- read_xls("iris/data/2012/population.xls",
                  sheet = 1)# identifier: "IRIS"

pop12 <- pop12[c(5:nrow(pop12)),] %>% 
  row_to_names(row_number = 1)

pop12paris <- pop12 %>% 
  filter(DEP == "75") # 992

# Familles
fam12 <- read_xls("iris/data/2012/familles.xls",
                  sheet = 1)# identifier: "IRIS"

fam12 <- fam12[c(5:nrow(fam12)),] %>% 
  row_to_names(row_number = 1)

fam12paris <- fam12 %>% 
  filter(DEP == "75") # 992

# Merge into a single file
paris12 <- rev12paris %>% 
  full_join(act12paris, by = "IRIS") %>% 
  full_join(log12paris, by = "IRIS") %>%
  full_join(for12paris, by = "IRIS") %>%
  full_join(pop12paris, by = "IRIS") %>%
  full_join(fam12paris, by = "IRIS") # 992


# Drop the previous datasets to save memory
rm(list = c("rev12", "act12", "log12", "for12", "pop12", "fam12"))

# Save Paris
write_csv(rev12paris, "iris/data/2012/revenus_paris.csv")
write_csv(act12paris, "iris/data/2012/activite_paris.csv")
write_csv(log12paris, "iris/data/2012/logement_paris.csv")
write_csv(for12paris, "iris/data/2012/formation_paris.csv")
write_csv(pop12paris, "iris/data/2012/population_paris.csv")
write_csv(fam12paris, "iris/data/2012/familles_paris.csv")

# Save joint Paris
write_csv(paris12, "iris/data/2012/paris12.csv")

# Drop the Paris datasets as well
rm(list = c("rev12paris", "act12paris", "log12paris",
            "for12paris", "pop12paris", "fam12paris",
            "paris12"))


# 2013
# Revenus
rev13 <- read_xls("iris/data/2013/revenus.xls",
                  sheet = 1) # identifier: "IRIS"

rev13 <- rev13[c(5:nrow(rev13)),] %>% 
  row_to_names(row_number = 1)

rev13paris <- rev13 %>% 
  filter(str_sub(IRIS, 1, 2) == "75") # 853

# Activités
act13 <- read_xls("iris/data/2013/activite.xls",
                  sheet = 1) # identifier: "IRIS" 

act13 <- act13[c(5:nrow(act13)),] %>% 
  row_to_names(row_number = 1)

act13paris <- act13 %>% 
  filter(DEP == "75") # 992

# Logements
log13 <- read_xls("iris/data/2013/logement.xls",
                  sheet = 1)# identifier: "IRIS"

log13 <- log13[c(5:nrow(log13)),] %>% 
  row_to_names(row_number = 1)

log13paris <- log13 %>% 
  filter(DEP == "75") # 992

# Formation
for13 <- read_xls("iris/data/2013/formation.xls",
                  sheet = 1)# identifier: "IRIS"

for13 <- for13[c(5:nrow(for13)),] %>% 
  row_to_names(row_number = 1)

for13paris <- for13 %>% 
  filter(DEP == "75") # 992

# Population
pop13 <- read_xls("iris/data/2013/population.xls",
                  sheet = 1)# identifier: "IRIS"

pop13 <- pop13[c(5:nrow(pop13)),] %>% 
  row_to_names(row_number = 1)

pop13paris <- pop13 %>% 
  filter(DEP == "75") # 992

# Familles
fam13 <- read_xls("iris/data/2013/familles.xls",
                  sheet = 1)# identifier: "IRIS"

fam13 <- fam13[c(5:nrow(fam13)),] %>% 
  row_to_names(row_number = 1)

fam13paris <- fam13 %>% 
  filter(DEP == "75") # 992

# Merge into a single file
paris13 <- rev13paris %>% 
  full_join(act13paris, by = "IRIS") %>% 
  full_join(log13paris, by = "IRIS") %>%
  full_join(for13paris, by = "IRIS") %>%
  full_join(pop13paris, by = "IRIS") %>%
  full_join(fam13paris, by = "IRIS") # 992


# Drop the previous datasets to save memory
rm(list = c("rev13", "act13", "log13", "for13", "pop13", "fam13"))

# Save Paris
write_csv(rev13paris, "iris/data/2013/revenus_paris.csv")
write_csv(act13paris, "iris/data/2013/activite_paris.csv")
write_csv(log13paris, "iris/data/2013/logement_paris.csv")
write_csv(for13paris, "iris/data/2013/formation_paris.csv")
write_csv(pop13paris, "iris/data/2013/population_paris.csv")
write_csv(fam13paris, "iris/data/2013/familles_paris.csv")

# Save joint Paris
write_csv(paris13, "iris/data/2013/paris13.csv")

# Drop the Paris datasets as well
rm(list = c("rev13paris", "act13paris", "log13paris",
            "for13paris", "pop13paris", "fam13paris",
            "paris13"))


# 2014
# Revenus
rev14 <- read_xls("iris/data/2014/revenus.xls",
                  sheet = 1) # identifier: "IRIS"

rev14 <- rev14[c(5:nrow(rev14)),] %>% 
  row_to_names(row_number = 1)

rev14paris <- rev14 %>% 
  filter(str_sub(IRIS, 1, 2) == "75") # 868

# Activités
act14 <- read_xls("iris/data/2014/activite.xls",
                  sheet = 1) # identifier: "IRIS" 

act14 <- act14[c(5:nrow(act14)),] %>% 
  row_to_names(row_number = 1)

act14paris <- act14 %>% 
  filter(DEP == "75") # 992

# Logements
log14 <- read_xls("iris/data/2014/logement.xls",
                  sheet = 1)# identifier: "IRIS"

log14 <- log14[c(5:nrow(log14)),] %>% 
  row_to_names(row_number = 1)

log14paris <- log14 %>% 
  filter(DEP == "75") # 992

# Formation
for14 <- read_xls("iris/data/2014/formation.xls",
                  sheet = 1)# identifier: "IRIS"

for14 <- for14[c(5:nrow(for14)),] %>% 
  row_to_names(row_number = 1)

for14paris <- for14 %>% 
  filter(DEP == "75") # 992

# Population
pop14 <- read_xls("iris/data/2014/population.xls",
                  sheet = 1)# identifier: "IRIS"

pop14 <- pop14[c(5:nrow(pop14)),] %>% 
  row_to_names(row_number = 1)

pop14paris <- pop14 %>% 
  filter(DEP == "75") # 992

# Familles
fam14 <- read_xls("iris/data/2014/familles.xls",
                  sheet = 1)# identifier: "IRIS"

fam14 <- fam14[c(5:nrow(fam14)),] %>% 
  row_to_names(row_number = 1)

fam14paris <- fam14 %>% 
  filter(DEP == "75") # 992

# Merge into a single file
paris14 <- rev14paris %>% 
  full_join(act14paris, by = "IRIS") %>% 
  full_join(log14paris, by = "IRIS") %>%
  full_join(for14paris, by = "IRIS") %>%
  full_join(pop14paris, by = "IRIS") %>%
  full_join(fam14paris, by = "IRIS") # 992


# Drop the previous datasets to save memory
rm(list = c("rev14", "act14", "log14", "for14", "pop14", "fam14"))

# Save Paris
write_csv(rev14paris, "iris/data/2014/revenus_paris.csv")
write_csv(act14paris, "iris/data/2014/activite_paris.csv")
write_csv(log14paris, "iris/data/2014/logement_paris.csv")
write_csv(for14paris, "iris/data/2014/formation_paris.csv")
write_csv(pop14paris, "iris/data/2014/population_paris.csv")
write_csv(fam14paris, "iris/data/2014/familles_paris.csv")

# Save joint Paris
write_csv(paris14, "iris/data/2014/paris14.csv")

# Drop the Paris datasets as well
rm(list = c("rev14paris", "act14paris", "log14paris",
            "for14paris", "pop14paris", "fam14paris",
            "paris14"))


# 2015
# Revenus
rev15 <- read_xls("iris/data/2015/revenus.xls",
                  sheet = 1) # identifier: "IRIS"

rev15 <- rev15[c(5:nrow(rev15)),] %>% 
  row_to_names(row_number = 1)

rev15paris <- rev15 %>% 
  filter(str_sub(IRIS, 1, 2) == "75") # 868

# Activités
act15 <- read_xls("iris/data/2015/activite.xls",
                  sheet = 1) # identifier: "IRIS" 

act15 <- act15[c(5:nrow(act15)),] %>% 
  row_to_names(row_number = 1)

act15paris <- act15 %>% 
  filter(DEP == "75") # 992

# Logements
log15 <- read_xls("iris/data/2015/logement.xls",
                  sheet = 1)# identifier: "IRIS"

log15 <- log15[c(5:nrow(log15)),] %>% 
  row_to_names(row_number = 1)

log15paris <- log15 %>% 
  filter(DEP == "75") # 992

# Formation
for15 <- read_xls("iris/data/2015/formation.xls",
                  sheet = 1)# identifier: "IRIS"

for15 <- for15[c(5:nrow(for15)),] %>% 
  row_to_names(row_number = 1)

for15paris <- for15 %>% 
  filter(DEP == "75") # 992

# Population
pop15 <- read_xls("iris/data/2015/population.xls",
                  sheet = 1)# identifier: "IRIS"

pop15 <- pop15[c(5:nrow(pop15)),] %>% 
  row_to_names(row_number = 1)

pop15paris <- pop15 %>% 
  filter(DEP == "75") # 992

# Familles
fam15 <- read_xls("iris/data/2015/familles.xls",
                  sheet = 1)# identifier: "IRIS"

fam15 <- fam15[c(5:nrow(fam15)),] %>% 
  row_to_names(row_number = 1)

fam15paris <- fam15 %>% 
  filter(DEP == "75") # 992

# Merge into a single file
paris15 <- rev15paris %>% 
  full_join(act15paris, by = "IRIS") %>% 
  full_join(log15paris, by = "IRIS") %>%
  full_join(for15paris, by = "IRIS") %>%
  full_join(pop15paris, by = "IRIS") %>%
  full_join(fam15paris, by = "IRIS") # 992


# Drop the previous datasets to save memory
rm(list = c("rev15", "act15", "log15", "for15", "pop15", "fam15"))

# Save Paris
write_csv(rev15paris, "iris/data/2015/revenus_paris.csv")
write_csv(act15paris, "iris/data/2015/activite_paris.csv")
write_csv(log15paris, "iris/data/2015/logement_paris.csv")
write_csv(for15paris, "iris/data/2015/formation_paris.csv")
write_csv(pop15paris, "iris/data/2015/population_paris.csv")
write_csv(fam15paris, "iris/data/2015/familles_paris.csv")

# Save joint Paris
write_csv(paris15, "iris/data/2015/paris15.csv")

# Drop the Paris datasets as well
rm(list = c("rev15paris", "act15paris", "log15paris",
            "for15paris", "pop15paris", "fam15paris",
            "paris15"))


# 2016
# Revenus
rev16 <- read_xls("iris/data/2016/revenus.xls",
                  sheet = 1) # identifier: "IRIS"

rev16 <- rev16[c(5:nrow(rev16)),] %>% 
  row_to_names(row_number = 1)

rev16paris <- rev16 %>% 
  filter(str_sub(IRIS, 1, 2) == "75") # 869

# Activités
act16 <- read_xls("iris/data/2016/activite.xls",
                  sheet = 1) # identifier: "IRIS" 

act16 <- act16[c(5:nrow(act16)),] %>% 
  row_to_names(row_number = 1)

act16paris <- act16 %>% 
  filter(DEP == "75") # 992

# Logements
log16 <- read_xls("iris/data/2016/logement.xls",
                  sheet = 1)# identifier: "IRIS"

log16 <- log16[c(5:nrow(log16)),] %>% 
  row_to_names(row_number = 1)

log16paris <- log16 %>% 
  filter(DEP == "75") # 992

# Formation
for16 <- read_xls("iris/data/2016/formation.xls",
                  sheet = 1)# identifier: "IRIS"

for16 <- for16[c(5:nrow(for16)),] %>% 
  row_to_names(row_number = 1)

for16paris <- for16 %>% 
  filter(DEP == "75") # 992

# Population
pop16 <- read_xls("iris/data/2016/population.xls",
                  sheet = 1)# identifier: "IRIS"

pop16 <- pop16[c(5:nrow(pop16)),] %>% 
  row_to_names(row_number = 1)

pop16paris <- pop16 %>% 
  filter(DEP == "75") # 992

# Familles
fam16 <- read_xls("iris/data/2016/familles.xls",
                  sheet = 1)# identifier: "IRIS"

fam16 <- fam16[c(5:nrow(fam16)),] %>% 
  row_to_names(row_number = 1)

fam16paris <- fam16 %>% 
  filter(DEP == "75") # 992

# Merge into a single file
paris16 <- rev16paris %>% 
  full_join(act16paris, by = "IRIS") %>% 
  full_join(log16paris, by = "IRIS") %>%
  full_join(for16paris, by = "IRIS") %>%
  full_join(pop16paris, by = "IRIS") %>%
  full_join(fam16paris, by = "IRIS") # 992


# Drop the previous datasets to save memory
rm(list = c("rev16", "act16", "log16", "for16", "pop16", "fam16"))

# Save Paris
write_csv(rev16paris, "iris/data/2016/revenus_paris.csv")
write_csv(act16paris, "iris/data/2016/activite_paris.csv")
write_csv(log16paris, "iris/data/2016/logement_paris.csv")
write_csv(for16paris, "iris/data/2016/formation_paris.csv")
write_csv(pop16paris, "iris/data/2016/population_paris.csv")
write_csv(fam16paris, "iris/data/2016/familles_paris.csv")

# Save joint Paris
write_csv(paris16, "iris/data/2016/paris16.csv")

# Drop the Paris datasets as well
rm(list = c("rev16paris", "act16paris", "log16paris",
            "for16paris", "pop16paris", "fam16paris",
            "paris16"))


# 2017
# Revenus
rev17 <- read_xlsx("iris/data/2017/revenus.xlsx",
                  sheet = 1) # identifier: "IRIS"

rev17 <- rev17[c(5:nrow(rev17)),] %>% 
  row_to_names(row_number = 1)

rev17paris <- rev17 %>% 
  filter(str_sub(IRIS, 1, 2) == "75") # 871

# Activités
act17 <- read_delim("iris/data/2017/activite.csv",
                    delim = ";") # identifier: "IRIS" 

act17paris <- act17 %>% 
  filter(str_sub(IRIS, 1, 2) == "75") # 992

# Logements
log17 <- read_delim("iris/data/2017/logement.csv",
                    delim = ";") # identifier: "IRIS" 

log17paris <- log17 %>% 
  filter(str_sub(IRIS, 1, 2) == "75") # 992

# Formation
for17 <- read_delim("iris/data/2017/formation.csv",
                    delim = ";") # identifier: "IRIS" 

for17paris <- for17 %>% 
  filter(str_sub(IRIS, 1, 2) == "75") # 992

# Population
pop17 <- read_delim("iris/data/2017/population.csv",
                    delim = ";") # identifier: "IRIS" 

pop17paris <- pop17 %>% 
  filter(str_sub(IRIS, 1, 2) == "75") # 992

# Familles
fam17 <- read_delim("iris/data/2017/familles.csv",
                    delim = ";") # identifier: "IRIS" 

fam17paris <- fam17 %>% 
  filter(str_sub(IRIS, 1, 2) == "75") # 992

# Merge into a single file
paris17 <- rev17paris %>% 
  full_join(act17paris, by = "IRIS") %>% 
  full_join(log17paris, by = "IRIS") %>%
  full_join(for17paris, by = "IRIS") %>%
  full_join(pop17paris, by = "IRIS") %>%
  full_join(fam17paris, by = "IRIS") # 992


# Drop the previous datasets to save memory
rm(list = c("rev17", "act17", "log17", "for17", "pop17", "fam17"))

# Save Paris
write_csv(rev17paris, "iris/data/2017/revenus_paris.csv")
write_csv(act17paris, "iris/data/2017/activite_paris.csv")
write_csv(log17paris, "iris/data/2017/logement_paris.csv")
write_csv(for17paris, "iris/data/2017/formation_paris.csv")
write_csv(pop17paris, "iris/data/2017/population_paris.csv")
write_csv(fam17paris, "iris/data/2017/familles_paris.csv")

# Save joint Paris
write_csv(paris17, "iris/data/2017/paris17.csv")

# Drop the Paris datasets as well
rm(list = c("rev17paris", "act17paris", "log17paris",
            "for17paris", "pop17paris", "fam17paris",
            "paris17"))

# 2018
# Revenus
rev18 <- read_delim("iris/data/2018/revenus.csv",
                   delim = ";") # identifier: "IRIS"

rev18paris <- rev18 %>% 
  filter(str_sub(IRIS, 1, 2) == "75") # 870

# Activités
act18 <- read_delim("iris/data/2018/activite.csv",
                    delim = ";") # identifier: "IRIS" 

act18paris <- act18 %>% 
  filter(str_sub(IRIS, 1, 2) == "75") # 992

# Logements
log18 <- read_delim("iris/data/2018/logement.csv",
                    delim = ";") # identifier: "IRIS" 

log18paris <- log18 %>% 
  filter(str_sub(IRIS, 1, 2) == "75") # 992

# Formation
for18 <- read_delim("iris/data/2018/formation.csv",
                    delim = ";") # identifier: "IRIS" 

for18paris <- for18 %>% 
  filter(str_sub(IRIS, 1, 2) == "75") # 992

# Population
pop18 <- read_delim("iris/data/2018/population.csv",
                    delim = ";") # identifier: "IRIS" 

pop18paris <- pop18 %>% 
  filter(str_sub(IRIS, 1, 2) == "75") # 992

# Familles
fam18 <- read_delim("iris/data/2018/familles.csv",
                    delim = ";") # identifier: "IRIS" 

fam18paris <- fam18 %>% 
  filter(str_sub(IRIS, 1, 2) == "75") # 992

# Merge into a single file
paris18 <- rev18paris %>% 
  full_join(act18paris, by = "IRIS") %>% 
  full_join(log18paris, by = "IRIS") %>%
  full_join(for18paris, by = "IRIS") %>%
  full_join(pop18paris, by = "IRIS") %>%
  full_join(fam18paris, by = "IRIS") # 992


# Drop the previous datasets to save memory
rm(list = c("rev18", "act18", "log18", "for18", "pop18", "fam18"))

# Save Paris
write_csv(rev18paris, "iris/data/2018/revenus_paris.csv")
write_csv(act18paris, "iris/data/2018/activite_paris.csv")
write_csv(log18paris, "iris/data/2018/logement_paris.csv")
write_csv(for18paris, "iris/data/2018/formation_paris.csv")
write_csv(pop18paris, "iris/data/2018/population_paris.csv")
write_csv(fam18paris, "iris/data/2018/familles_paris.csv")

# Save joint Paris
write_csv(paris18, "iris/data/2018/paris18.csv")

# Drop the Paris datasets as well
rm(list = c("rev18paris", "act18paris", "log18paris",
            "for18paris", "pop18paris", "fam18paris",
            "paris18"))

# 2019
# Revenus
rev19 <- read_delim("iris/data/2019/revenus.csv") # identifier: "IRIS"

rev19paris <- rev19 %>% 
  filter(str_sub(IRIS, 1, 2) == "75") # 869

# Activités
act19 <- read_delim("iris/data/2019/activite.csv",
                    delim = ";") # identifier: "IRIS" 

act19paris <- act19 %>% 
  filter(str_sub(IRIS, 1, 2) == "75") # 992

# Logements
log19 <- read_delim("iris/data/2019/logement.csv",
                    delim = ";") # identifier: "IRIS" 

log19paris <- log19 %>% 
  filter(str_sub(IRIS, 1, 2) == "75") # 992

# Formation
for19 <- read_delim("iris/data/2019/formation.csv",
                    delim = ";") # identifier: "IRIS" 

for19paris <- for19 %>% 
  filter(str_sub(IRIS, 1, 2) == "75") # 992

# Population
pop19 <- read_delim("iris/data/2019/population.csv",
                    delim = ";") # identifier: "IRIS" 

pop19paris <- pop19 %>% 
  filter(str_sub(IRIS, 1, 2) == "75") # 992

# Familles
fam19 <- read_delim("iris/data/2019/familles.csv",
                    delim = ";") # identifier: "IRIS" 

fam19paris <- fam19 %>% 
  filter(str_sub(IRIS, 1, 2) == "75") # 992

# Merge into a single file
paris19 <- rev19paris %>% 
  full_join(act19paris, by = "IRIS") %>% 
  full_join(log19paris, by = "IRIS") %>%
  full_join(for19paris, by = "IRIS") %>%
  full_join(pop19paris, by = "IRIS") %>%
  full_join(fam19paris, by = "IRIS") # 992


# Drop the previous datasets to save memory
rm(list = c("rev19", "act19", "log19", "for19", "pop19", "fam19"))

# Save Paris
write_csv(rev19paris, "iris/data/2019/revenus_paris.csv")
write_csv(act19paris, "iris/data/2019/activite_paris.csv")
write_csv(log19paris, "iris/data/2019/logement_paris.csv")
write_csv(for19paris, "iris/data/2019/formation_paris.csv")
write_csv(pop19paris, "iris/data/2019/population_paris.csv")
write_csv(fam19paris, "iris/data/2019/familles_paris.csv")

# Save joint Paris
write_csv(paris19, "iris/data/2019/paris19.csv")

# Drop the Paris datasets as well
rm(list = c("rev19paris", "act19paris", "log19paris",
            "for19paris", "pop19paris", "fam19paris",
            "paris19"))


# 2020
# Revenus
rev20 <- read_delim("iris/data/2020/revenus.csv") # identifier: "IRIS"

rev20paris <- rev20 %>% 
  filter(str_sub(IRIS, 1, 2) == "75") # 992









# ------------------ PICK CODE BACK FROM HERE -------------------#
##############################
##### TREATMENT STATUS  ######
##############################

# Import data generated in Python with an overlay slow zones/IRIS 
iris_treat <- st_read("zones_30/iris_treatment.geojson")

# Import slow zone data
zones <- st_read("zones_30/zones-30.shp")

# Generate a .txt file with the list of treated IRIS
# iris_treat_list <- unique(iris_treat$iris) # 546
# write_delim(as_tibble(iris_treat_list), "zones_30/iris_treatment_list.txt", delim = " ", col_names = FALSE)

# Group by IRIS and find the earliest year
iris_treat_g <- iris_treat %>% 
  group_by(iris) %>% 
  summarise(earliest_year = min(as.integer(year), na.rm = FALSE),
            n = n()) %>% 
  mutate(earliest_year = ifelse(is.infinite(earliest_year),
                                NA,
                                earliest_year)) %>%
  filter(!(earliest_year %in% c("2007", "2008")))

iris_treat_g[iris_treat_g$earliest_year %in% c("2007", "2008"),]

# Classify those which receive treatment in 2007 and 2008 as controls,
# as there's just one slow zone each year in a very preliminary and disconnected
# fashion. As controls (i.e. never-treated IRIS) are not in this dataset,
# simply drop them in the last line of the previous code chunk (just 12 observations).

# See the number of missing values for year of implementation
sum(is.na(iris_treat$year)) # 129
sum(is.na(iris_treat_g$earliest_year)) # 118

# Group by IRIS and find the earliest year (if we assign to IRIS with more than one
# LSZ the value of the one that has it and ignore the NA assuming it'll be later...)
iris_treat_g <- iris_treat %>% 
  group_by(iris) %>% 
  summarise(earliest_year = min(as.integer(year), na.rm = TRUE), # just change here
            n = n()) %>% 
  mutate(earliest_year = ifelse(is.infinite(earliest_year),
                                NA,
                                earliest_year))

# See the number of missing values for year of implementation
sum(is.na(iris_treat$year)) # 129
sum(is.na(iris_treat_g$earliest_year)) # 85


# Distribution of IRIS as a function of the number of slow zones that fall within them
iris_treat_g %>% 
  group_by(n) %>% 
  summarise(n = n())


# Save this
st_write(iris_treat_g, "zones_30/iris_treatment_grouped.geojson")

#################################
##### CREATE A LONG PANEL  ######
#################################

# Create a panel with all the IRIS, even the non-treated ones
# Import 2015 (for example) geolocated data for IRIS
iris15_geo <- st_read("iris/geo/2015/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2015/CONTOURS-IRIS_2-1_SHP_LAMB93_FE-2015/CONTOURS-IRIS.SHP") %>% 
  filter(str_sub(INSEE_COM, 1, 2) == "75") %>% 
  rename(iris_code = CODE_IRIS)

# Retrieve a list of all the IRIS in Paris & convert it into a dataframe
iris <- as_tibble(iris15_geo$iris_code) %>% 
  rename(iris_code = value) %>% 
  slice(rep(1:nrow(iris),
            11))

which(iris$iris_code == "751010204") # just a simple check to see each observation is
# 992 positions away from each other

# Now, rearrange and assign year and join other relevant info
iris <- iris %>% 
  arrange(iris_code) %>% 
  mutate(year = rep.int(c("2010", "2011", "2012", "2013", "2014", "2015",
                      "2016", "2017", "2018", "2019", "2020"),
                        992)) %>% 
  full_join(iris15_geo %>% 
              st_drop_geometry(), by = "iris_code")


####### TREATMENT STATUS NOW

# Create a dummy for "overlapping with at least a slow zone" to keep track of which
# IRIS are so, as some of these do not have information on the year of implementation 
# and as such as problematic to classify. They will be eventually dropped but I want
# to keep track of them for now

iris_treat_g$inside_slow_zone = 1
iris_treat_g <- rename(iris_treat_g, iris_code = iris)

# Now, do the join
iris <- iris %>% 
  full_join(iris_treat_g %>% 
              st_drop_geometry(), by = "iris_code") %>% 
  rename(n_slow_zones = n) # number of slow zones
