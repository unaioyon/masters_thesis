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
library(SuperLearner)


# Set working directory
setwd("/Users/unaioyon/Desktop/masters_thesis/data/fra")

options(scipen = 999)



#############################################
#### ELECTION DATA ##########################
#############################################

# 2008

for (i in c(1, 2, 4:10, 12:15, 17, 18, 20)) {
  if(i < 10){
    assign(sprintf("ele08_%s",
                   as.character(i)),
           read_excel(sprintf("elections/2008/2eme_tour/DDCT_BERP_municipales_2008_tour2_Ardt_%s_20080317.xlsx",
                              paste0("0",
                                     as.character(i)))))
    
    ele08 <- ele08 %>% 
      bind_rows(get(sprintf("ele08_%s",
                            as.character(i))))
  }
  
  else{
    assign(sprintf("ele08_%s",
                   as.character(i)),
           read_excel(sprintf("elections/2008/2eme_tour/DDCT_BERP_municipales_2008_tour2_Ardt_%s_20080317.xlsx",
                              as.character(i))))
    
    ele08 <- ele08 %>% 
      bind_rows(get(sprintf("ele08_%s",
                            as.character(i))))
  }
}

# 1 arr
ele08_1 <- ele08_1 %>% 
  mutate(droite = `LEGARET Jean-François`,
         gauche = `DAGOMA Seybah`)

# 2 arr
ele08_2 <- ele08_2 %>% 
  mutate(droite = `LEKIEFFRE Christophe`,
         gauche = `BOUTAULT Jacques`)

# 4 arr
ele08_4 <- ele08_4 %>% 
  mutate(droite = `ROGER Vincent`,
         gauche = `BERTINOTTI Dominique`)

# 5 arr
ele08_5 <- ele08_5 %>% 
  mutate(droite = `MEYER Philippe` + `TIBERI Jean`,
         gauche = `COHEN-SOLAL Lyne`)

# 6 arr
ele08_6 <- ele08_6 %>% 
  mutate(droite = `LECOQ Jean-Pierre`,
         gauche = `LÉVY Romain`)

# 7 arr
ele08_7 <- ele08_7 %>% 
  mutate(droite = `DATI Rachida`,
         gauche = `GIRARD Laurence` + `DELVOLVÉ-ROSSET V`)

# 8 arr
ele08_8 <- ele08_8 %>% 
  mutate(droite = `LEBEL François` + `LELLOUCHE Pierre`,
         gauche = `RANÇON-CAVENEL Heidi`)

# 9 arr
ele08_9 <- ele08_9 %>% 
  mutate(droite = `BURKLI Delphine`,
         gauche = `BRAVO Jacques`)

# 10 arr
ele08_10 <- ele08_10 %>% 
  mutate(droite = `ASMANI Lynda`,
         gauche = `FÉRAUD Rémi`)

# 12 arr
ele08_12 <- ele08_12 %>% 
  mutate(droite = `CAVADA Jean-Marie`,
         gauche = `BLUMENTHAL Michèle`)

# 13 arr
ele08_13 <- ele08_13 %>% 
  mutate(droite = `VASSEUR Véronique`,
         gauche = `COUMET Jérôme`)

# 14 arr
ele08_14 <- ele08_14 %>% 
  mutate(droite = `DE SARNEZ Marielle` + `CARRÈRE-GÉE Marie-Claire`,
         gauche = `CASTAGNOU Pierre`)

# 15 arr
ele08_15 <- ele08_15 %>% 
  mutate(droite = `GOUJON Philippe`,
         gauche = `HIDALGO Anne`)

# 17 arr
ele08_17 <- ele08_17 %>% 
  mutate(droite = `DE PANAFIEU Françoise`,
         gauche = `LEPETIT Annick`)

# 18 arr
ele08_18 <- ele08_18 %>% 
  mutate(droite = `DECORTE Roxane`,
         gauche = `VAILLANT Daniel`)

# 20 arr
ele08_20 <- ele08_20 %>% 
  mutate(droite = `CHARZAT Michel`,
         gauche = `CALANDRA Frédérique`)


# Merge everything
ele08 <- tibble()

for (i in c(1, 2, 4:10, 12:15, 17, 18, 20)) {
  ele08 <- ele08 %>% 
      bind_rows(get(sprintf("ele08_%s",
                            as.character(i))))
}

ele08 <- ele08 %>% 
  select(colnames(ele08)[c(1:15,18,19)]) %>% 
  mutate(gauche_share = gauche/NB_EXPRIM,
         droite_share = droite/NB_EXPRIM,
         .after = gauche)


# Import the bureaux de vote geographic information with the weights
bureaux <- st_read("delineation/iris_bureaux_vote.geojson")

# No NAs in the 2008 values or in the weights
lapply(ele08, function(x) sum(is.na(x))) # 0 for all variables
sum(is.na(bureaux$weights)) # 0

# Join dataframe & drop NAs
bureaux <- bureaux %>% 
  left_join(ele08 %>% 
              select(ID_BVOTE, gauche_share, droite_share), by = "ID_BVOTE")  %>% 
  filter(!is.na(gauche_share))

# Since some of the arrondissements are not made available in the data distribution
# but the référentiel géographique includes all of them, now NAs are generated

# Thus, redefine weights to sum up to 1 instead of dropping the unnecessary stuff.
bureaux <- bureaux %>% 
  group_by(iris) %>% 
  mutate(sum_weights = sum(weights)) %>% 
  ungroup() %>% 
  rename(old_unscaled_weights = weights) %>% 
  mutate(weights = old_unscaled_weights/sum_weights,
         arr = str_sub(iris,
                       4,
                       5))

st_write(bureaux, "delineation/iris_bureaux_vote_corrected.geojson")

# Import the right data now
bureaux <- st_read("delineation/iris_bureaux_vote_corrected.geojson") %>% 
  st_drop_geometry() %>% 
  as_tibble()


# Now, group to get the averages
bureaux_g <- bureaux %>% 
  group_by(iris) %>% 
  summarise(gauche_share = weighted.mean(gauche_share, weights, na.rm = TRUE),
            droite_share = weighted.mean(droite_share, weights, na.rm = TRUE),
            sum_weights = sum(weights))


# Visual check for weights
bureaux[bureaux$iris == "751156014",]


# 2014
for (i in c(2:5, 7:15, 18:20)) {
  
  if(i < 10){
    assign(sprintf("ele_08_%s",
                   as.character(i)),
           read_excel(sprintf("elections/2008/2eme_tour/DDCT_BERP_municipales_2008_tour2_Ardt_%s_20080317.xlsx",
                              paste0("0",
                                     as.character(i)))))
  }
  
  else{
    assign(sprintf("ele_08_%s",
                   as.character(i)),
           read_excel(sprintf("elections/2008/2eme_tour/DDCT_BERP_municipales_2008_tour2_Ardt_%s_20080317.xlsx",
                              as.character(i))))
  }
}


