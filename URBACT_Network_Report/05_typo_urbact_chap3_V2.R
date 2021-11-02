
##==========================================================================##         
##                      TYPOLOGIE DES PROJETS URBACT                        ##
##                                                                          ##
##                                                                          ##    
## DESCRIPTION : Base URBACT / préparation des variables en vue de la       ##
##               réalisation d'une typologie des projets (CAH)              ##
##                                                                          ##
## PG, AD, Novembre 2019                                                    ##
##==========================================================================##

# CONTENTS

# Working directory huma-num
#setwd("~/BD_Keep_Interreg/KEEP")


# Library
library(readr)
library(sf)
library(mapview)
library(tidyverse)
library(tidylog)
# library(cartography)
library(skimr)
# library(ggsn) # scalebar on maps



# Import data
# sfEU <- st_read("AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)
# 
# rec <- st_read("AD/FDCARTE/rec_3035.geojson")

urbactCities <- read.csv2("~/Chap5_TextMining/Data/URBACT/URBACT_Membership_GNidCorr_complete_V2.csv", stringsAsFactors = F)

GN_DB_City <- readRDS("~/Chap5_TextMining/Data/DBCity/DBCity_LauUmzFua.rds")

urbactCities <-  urbactCities %>% left_join(select(GN_DB_City, geonameId, adminLevel, countryCode_GN = countryCode, population, CODE_LAU, 
                                                                       NAME_LAU, PopAdmin06,PopAdmin11))
# ==== Last try : new city size percentages, primacy index & coef of variation =====

## Creation of a new table of URBACT projects (with number of city in each network)
network <- urbactCities %>% 
  group_by(Code_Network) %>% 
  summarise(Ncities = n()) 


## % of cities by city size (category) in each project

### First, create a new vars: class of city size (1, 2, 3 ou 4) 
### si classe de taille validée, nommer ces classes (petite ville, moyenne, etc. par ex)
skim(urbactCities$PopAdmin11)
urbactCities <- urbactCities %>% 
  filter(!is.na(PopAdmin11)) %>% 
  mutate(KPOP = case_when(PopAdmin11 < 50000 ~ "1",
                          PopAdmin11 > 50000 & PopAdmin11 < 150000 ~ "2",
                          PopAdmin11 > 150000 & PopAdmin11 < 500000 ~ "3",
                          TRUE ~ "4"))

table(urbactCities$KPOP)

### Count number of cities in each category by network
kTY_city <- urbactCities %>% 
  group_by(Code_Network, KPOP) %>% 
  summarise(N_Kcity_byNw = n())

### Add number of cities in each network
kTY_city <- left_join(kTY_city, network, by = "Code_Network")

### % 
kTY_city <- kTY_city %>% 
  mutate(P_Kcity_byNw = round(N_Kcity_byNw / Ncities * 100))

kTY_city_spread <- kTY_city %>% 
  select(Code_Network, KPOP, P_Kcity_byNw) %>% 
  spread(key = KPOP, value = P_Kcity_byNw, sep = "")

network <- left_join(network, kTY_city_spread)           

rm(kTY_city_spread, kTY_city)

network <- network %>% 
  mutate_at(vars("KPOP1", "KPOP2", "KPOP3", "KPOP4"), 
            replace_na, 0)


## real primacy index (pop city 1 / pop city 2)
pI <- urbactCities %>% 
  group_by(Code_Network) %>% 
  top_n(n = 2, wt = PopAdmin11) %>% 
  arrange(Code_Network, PopAdmin11) %>% 
  mutate(primacy_index = max(PopAdmin11)/min(PopAdmin11)) %>% 
  filter(!duplicated(primacy_index))

network <- network %>% left_join(select(pI, Code_Network, primacy_index))
rm(pI)

## coef of variation on the pop
variance <- urbactCities %>% 
  group_by(Code_Network) %>% 
  mutate(ect = sd(PopAdmin11),
         moy = mean(PopAdmin11),
         coefVar = ect/moy) %>% 
  filter(!duplicated(Code_Network))

network <- network %>% left_join(select(variance, Code_Network, coefVar))
rm(variance)

## save for exploratR
# write.csv2(network, "AD/URBACT/URBACTforCAH.csv", row.names = FALSE, fileEncoding = "UTF-8")



## Commenges H. (2016) ExploratR : outil interactif d'exploration statistique uni- bi- tri- et multi-variée avec R, UMR 8504 Géographie-cités. 
## APPLI : https://analytics.huma-num.fr/geographie-cites/ExploratR/
## CODE : https://zenodo.org/record/155333#.XdZn7dVCfIU

library(cluster)
library(ggdendro)
library(reshape2)
suppressWarnings(source("~/Chap5_TextMining/multi_bivariateFuntions_exploratR/Functions_anova_cah.R"))

colnames(network)
myVar <- c("KPOP1","KPOP2","KPOP3","KPOP4","coefVar")
cah <- ComputeClassif(df = network,
                      varquanti = myVar, method = "ward", stand = TRUE)


dendro <- PlotDendro(classifobj = cah)
dendro

inert <- PlotHeight(classifobj = cah)
inert
myProfiles <- PlotProfile(classifobj = cah, nbclus = 6)
myProfiles$PROFILE
table(myProfiles$CLUSID)
network2 <- network %>% mutate(CAH = myProfiles$CLUSID)


## plot profile of CAH with original variables
PlotCAHProfile <- network2 %>% select(c(3:6,8,9)) %>% pivot_longer(-CAH)

library(questionr)
irec(PlotCAHProfile)
## Recodage de PlotCAHProfile$name en PlotCAHProfile$name_rec
PlotCAHProfile$name_rec <- fct_recode(PlotCAHProfile$name,
  "1. Petites Villes (%)" = "KPOP1",
  "2. Villes Moyennes (%)" = "KPOP2",
  "3. Grandes Villes (%)" = "KPOP3",
  "4. Très Grandes Villes (%)" = "KPOP4",
  "Coefficient Variation" = "coefVar"
)
PlotCAHProfile$name_rec <- as.character(PlotCAHProfile$name_rec)## Recodage de PlotCAHProfile$name en PlotCAHProfile$name_rec

ggplot(PlotCAHProfile)+ 
  geom_boxplot(aes(x =CAH, fill = CAH, y = value), axes = TRUE, outlier.colour="black", outlier.size=0.5, outlier.alpha = 0.6)+ 
  facet_wrap(~ name_rec, scales = "free_y")+ 
  labs(y = "", x = "", fill = "Typologies\ndes projets URBACT\n(CAH)", 
       caption = "Seuils : 50k, 150k, 500k\nPG. 2020")+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 12))



ggsave( filename = "OUT/URBACTsizeNetwork_CAH6Profiles_BoxPlot.pdf", width = 8.3, height = 5.8 , units = "in" )


## 


NameTypoCah <- data.frame(CAH = unique(network2$CAH), CAH_PartnerSize = c("1. Taille mixte,\nsous représentation des petites villes", 
                                                                          "2. Villes moyennes majoritaires", 
                                                                          "3. Très grandes villes majoritaires" , 
                                                                          "4. Très hétérogènes",
                                                                          "5. Villes petites majoritaires", 
                                                                          "6. Grandes villes majoritaires"))

PlotCAHProfile <- PlotCAHProfile %>% left_join(NameTypoCah)

ggplot(PlotCAHProfile)+ 
  geom_boxplot(aes(x =CAH_PartnerSize, fill = CAH_PartnerSize, y = value), 
               axes = TRUE, outlier.colour="black", outlier.size=0.5, outlier.alpha = 0.6, show.legend = NA)+ 
  facet_wrap(~ name_rec, scales = "free_y")+ 
  labs(y = "", x = "", fill = "Typologies\ndes projets URBACT\n(CAH)", 
       caption = "Seuils : 50k, 150k, 500k\nPG. 2020")+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 12),
        legend.position = c(0.835,0.25))



ggsave( filename = "URBACT_Network_Report/OUT/URBACTsizeNetwork_CAH6Profiles_BoxPlot.pdf", width = 8.3, height = 5.8 , units = "in" )

network2 <- network2 %>% left_join(NameTypoCah)

write.csv2(network2, "DataProd/CAH6_URBACTProjectSize.csv", row.names = F, fileEncoding = "UTF-8")


