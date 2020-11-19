###############################   URBACT Good Practices   ##################################
#                               
#                          
# DESCRIPTION : Analyse textuelle et géographique des bonnes pratiques urbact
#
# 
############################################################################## PG septembre 2020

setwd("~/Chap5_TextMining/URBACT_Keywords_goodpractices/")

# Packages

library(tidyverse)
library(tidytext)
library(FactoMineR)
library(explor)
library(sf)
library(skimr)
library(GGally)
library(stargazer)
library(patchwork)
library(forcats)
library(randomcoloR)

### Data

# Table from websscrap 2018
GoodPract <- read.csv2("~/Chap5_TextMining/Data/URBACT/GoodPracticesUrbact_WebscrapMay2018_Clean.csv", stringsAsFactors = F)

# Table with some attributes 
Attributes <- read.csv2("~/Chap5_TextMining/Data/URBACT/BestPracticesUrbact_Attributes_V2.csv", stringsAsFactors = F, encoding = "UTF-8")
Attributes <- Attributes %>% mutate(AllText = paste(Summary,Solution, Approach,Participation,Outcomes,Transferability, sep = ". "))

# New harvest of keywords
Kwords2020 <- read.csv2("~/Chap5_TextMining/Data/URBACT/GoodPracticesUrbact_KwordsUpdated2020.csv", stringsAsFactors = F, encoding = "UTF-8")

# Code GN for cities
DicoGN <- read.csv2("~/Chap5_TextMining/Data/URBACT/DicoCity_GN_GoodPracticesURBACT.csv")
DicoGN <- DicoGN %>% select(- CodePractices, - CountryCode) %>% distinct()

# Join new data to attributes
Attributes <- Attributes %>% left_join(select(Kwords2020,Name, Kwords2020 = Kwords ) )

Attributes <- Attributes %>%  left_join(DicoGN)

# Check the df
skim(Attributes)

# Palette color for main URBACT key words (from step 1)
palette <- readRDS("DataProd/palette.rds")

# Geom data
## rec

rec <- st_read("~/Chap5_TextMining/Data/Geometry/rec_3035.geojson")

#EU 
sfEU <- st_read("~/Chap5_TextMining/Data/Geometry/fondEuropeLarge.geojson", stringsAsFactors = FALSE,crs = 3035)


################### ====  SUMMARY  Variable ===========###############


Attributes <- Attributes %>% mutate(SizeClassURBACT = case_when(Pop< 50000 ~"Less than 50 000",
                                                                Pop> 250000 ~ "More than 250 000",
                                                                TRUE ~ "Between 50 000 and 250 000"))

Attributes <- Attributes %>% mutate(Region = recode(Region, "Western Asia" = "Southern Europe"))

AttributesClean <- Attributes %>% arrange(CodePractices)

skim(Attributes)

ggpairs(Attributes, columns = c(18,23,30))

#save the table of good practices 
write.csv(AttributesClean,file = "DataProd/GoodPracticesURBACT_Corpus.csv", row.names = FALSE, fileEncoding = "UTF-8")

corpus <- import_corpus("DataProd/GoodPracticesURBACT_Corpus.csv", textcolumn = 20, format = "csv", language = "en", )

################### === Centralities of cities with good practices in network ===############### 

# Data (load centralities of cities, cf Chap 4)
URBACTnet <- readRDS("~/Chap5_TextMining/Data/NetworkData/Centralities_URBACT_enww_em2nw.rds")
ETMUNnet <- readRDS("~/Chap5_TextMining/Data/NetworkData/Centralities_ETMUN_enww_em2nw.rds")
EUCICOPnet <- readRDS("~/Chap5_TextMining/Data/NetworkData/Centralities_EUCICOP_enww_em2nw_1stComp.rds")

# Get cities centralities
URBACTnet <- URBACTnet[[1]]
ETMUNnet <- ETMUNnet[[1]]
EUCICOPnet <- EUCICOPnet[[1]]

# Join Degrees of the 3 networks (nb of participations) to cities with Good Practices

CityGoodPracticeCentrality <- DicoGN %>% select(-City) %>% mutate(geonameId = as.character(geonameId))%>%
                              left_join(select(URBACTnet, geonameId, D_urbact = D)) %>%
                              left_join(select(EUCICOPnet, geonameId, D_eucicop = D))%>%
                              left_join(select(ETMUNnet, geonameId, D_etmun = D)) %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))
# Check summary of degrees

summary(CityGoodPracticeCentrality$D_urbact)
summary(CityGoodPracticeCentrality$D_eucicop)
summary(CityGoodPracticeCentrality$D_etmun)

# Export the dataframe for latex

colnames(CityGoodPracticeCentrality) <- c("geonameId", "asciiName","Participations URBACT", "Participations EUCICOP", "Adhésions ETMUN")
stargazer(CityGoodPracticeCentrality,  median= TRUE)


rm(URBACTnet, ETMUNnet, EUCICOPnet)


############# ====  KEY WORDS ===========###############

## Prepare data for main keywords

# Main keywords
listKwords <- as.vector(unique(str_trim(unlist(str_split_fixed(Attributes$Kwords, '\n', n=2)))))

# Check duplicated
sort(unique(listKwords))
duplicated(unique(listKwords))

# get the five main KW (themes)

listKwords <- listKwords[listKwords %in% listKwords[1:5]]

# Spread KW into columns
for(i in 1:length(listKwords)) {
  Attributes[,listKwords[i]] <- grepl(listKwords[[i]],Attributes$Kwords)
}

### Convert True/False in 0/1
Bool_kwords <- sapply(X = Attributes[,31:ncol(Attributes)], FUN = function(x){ifelse(x == "TRUE", 1, 0)})
# Add bool keword to the dataframe
PracticeKW <- cbind(Attributes[,1:30],Bool_kwords)
# Erase old variable of KW
PracticeKW <- PracticeKW %>% select( -Kwords)

# Number of KW by projects
summary(rowSums(Bool_kwords))


PracticeKW <- PracticeKW%>%  pivot_longer(-c(1:29),names_to = "MainKeyWord", values_to = "value")%>%  filter(value>0) %>% filter(!duplicated(CodePractices))


# Keep practice with two main key word
# PracticeKW <- PracticeKW %>% group_by(CodePractices)%>% mutate(MainKeyWord =  paste(MainKeyWord, collapse = "/")) %>% distinct() %>%ungroup()

unique(PracticeKW$MainKeyWord)


SummaryPractice <- PracticeKW %>% group_by(Region, SizeClassURBACT, MainKeyWord)%>% count()



g1 <- ggplot(SummaryPractice) + 
  geom_bar( aes(x = reorder(Region,n), y= n, group = Region), stat = "identity", show.legend = FALSE) + 
  coord_flip() + 
  scale_y_continuous(n.breaks = 6)+
  labs(y = "Nombre de 'Bonnes Pratiques'", x = "Région")

g2 <- ggplot(SummaryPractice) + 
  geom_bar( aes(x = fct_rev(fct_infreq(SizeClassURBACT)), y= n, group = SizeClassURBACT), stat = "identity", show.legend = FALSE) + 
  coord_flip() + 
  scale_y_continuous(n.breaks = 6)+
  labs(y = "Nombre de 'Bonnes Pratiques'", x = "Classe de taille")


palette2 <- palette[names(palette) %in% unique(SummaryPractice$MainKeyWord)]
g3<- SummaryPractice %>%
  group_by(MainKeyWord) %>% summarise(N= sum(n))%>%
  ggplot(aes(x = reorder(MainKeyWord, N), y = N, fill = MainKeyWord), show.legend=FALSE) +
  scale_fill_manual(values= palette2)+
  geom_bar(stat = 'identity') +
  coord_flip() + 
      scale_y_continuous(n.breaks = 6)+
  labs(y = "Nombre de 'Bonnes Pratiques'", x = "Mot-clé")+
  theme(legend.position = "none")



#Map localisation
SfcityGoodPractice <- Attributes %>% 
  select(asciiName,geonameId, SizeClassURBACT, Long, Lat) %>% 
  st_as_sf(., coords = c("Long", "Lat"), crs = 4326) %>%
  st_transform(3035) %>% filter(!duplicated(geonameId))%>%
  mutate(SizeClassURBACT = fct_relevel(SizeClassURBACT, 
                                       "Less than 50 000", "Between 50 000 and 250 000", "More than 250 000")) 

myScaleBar <- data.frame(X = c(c(st_bbox(rec)[3]-900000), c(st_bbox(rec)[3]-400000)),
                         Y = c(c(st_bbox(rec)[2]+200000), c(st_bbox(rec)[2]+200000)))



g4 <- ggplot() + 
  geom_sf(data = sfEU, fill = "#bfbfbf", color = "white", size = 0.5) +
  geom_sf(data = SfcityGoodPractice ,
          mapping = aes( fill = SizeClassURBACT), shape= 21  , color = "gray80",show.legend = NA, size = 2) +
  scale_fill_brewer(palette = "YlOrRd")+
  ggplot2::annotate("text", label = "Source : URBACT site web/ PG. 2020",
           size = 2.2, 
           hjust = 1,
           x = c(st_bbox(rec)[3]), y = c(st_bbox(rec)[2]-130000)) +
  labs(x = "", y = "") +
  geom_line(data = myScaleBar, aes(x = X, y = Y), size = 0.5, color = "#333333") +
  ggplot2::annotate("text", label = "500 km", size = 2.5, color = "#333333", hjust = 0,
           x = c(st_bbox(rec)[3]-800000), y = c(st_bbox(rec)[2]+280000)) +
  geom_sf(data = rec, fill = NA, color = "ivory4", size = 0.5) +
  coord_sf(crs = 3035, datum = NA,
           xlim = st_bbox(rec)[c(1,3)],
           ylim = st_bbox(rec)[c(2,4)]) + 
  labs(fill= "Classe de taille déclarée par URBACT") +
  theme_void() +
  theme(legend.position =  c(0.25, 0.80), 
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 7.5))



grid1 <- g4 + {
  g3 + {
    g1 +
      g2 +
      plot_layout(ncol = 1)
  }
} + plot_layout(ncol = 1)


gcountry <- PracticeKW %>% group_by(CountryCode) %>% count() %>% arrange(desc(n))%>%
  ggplot() + 
  geom_bar( aes(x = reorder(CountryCode,n), y= n), stat = "identity", show.legend = FALSE) + 
  coord_flip() + 
  scale_y_continuous(n.breaks = 6)+
  labs(y = "Nombre de 'Bonnes Pratiques'", x = "Pays")




# Clean workspace
rm(g1,g2,g3, g4, gcountry, grid1)

############### ===== Keywords 2020 CA and 2018  ======= #####

##################### Prepare Data #################
listKwords18 <- as.vector(unique(str_trim(unlist(str_split(AttributesClean$Kwords, '\n')))))
listKwords20 <- as.vector(unique(str_trim(unlist(str_split(AttributesClean$Kwords2020, '\n')))))

# Check duplicated
sort(unique(listKwords18))
duplicated(unique(listKwords18))

sort(unique(listKwords20))
duplicated(unique(listKwords20))

# Remove empty one

listKwords18 <- listKwords18[listKwords18 != ""]

listKwords20 <- listKwords20[listKwords20 != ""]

## Prepare DF

Attributes18 <- AttributesClean
Attributes20 <- AttributesClean

##### Recode some key words 
# For 2020

Attributes20 <- Attributes20 %>% mutate(Kwords2020_rec =  str_replace_all(Kwords2020,c("Climate change" = "Climate adaptation"   ,  "Participatory approach" = "Participation" )))


listKwords20 <- as.vector(unique(str_trim(unlist(str_split(Attributes20$Kwords2020_rec, '\n')))))
listKwords20 <- listKwords20[listKwords20 != ""]

# Spread KW into columns
for(i in 1:length(listKwords18)) {
  Attributes18[,listKwords18[i]] <- grepl(listKwords18[[i]],Attributes18$Kwords)
}

  
for(i in 1:length(listKwords20)) {
  Attributes20[,listKwords20[i]] <- grepl(listKwords20[[i]],Attributes20$Kwords2020_rec)
  }  
### Convert True/False in 0/1
Bool_kwords18 <- sapply(X = Attributes18[,31:ncol(Attributes18)], FUN = function(x){ifelse(x == "TRUE", 1, 0)})

Bool_kwords20 <- sapply(X = Attributes20[,32:ncol(Attributes20)], FUN = function(x){ifelse(x == "TRUE", 1, 0)})
# Add bool keword to the dataframe
PracticeKW18 <- cbind(Attributes18[,1:30],Bool_kwords18)
PracticeKW20 <- cbind(Attributes20[,1:31],Bool_kwords20)


##################### TOP KEYWORDS #################


## TOP keywords

LongPracticeUrbact <- PracticeKW20%>% select(18,23,30, 32:ncol(PracticeKW20)) %>% pivot_longer( -c(1:3), names_to = "Keywords") 

Top  <- LongPracticeUrbact %>% group_by(Keywords)%>% summarise(N= sum(value))%>% top_n(70,N) %>% arrange(desc(N))


# by region
NbPracticeRegion <- PracticeKW20 %>% group_by(Region) %>% summarise(NbPracticePerRegion = n())


CountProjectRegion <- LongPracticeUrbact%>% 
  group_by(Region, Keywords) %>% 
  summarise(N = sum(value)) %>% 
  mutate(freq = N / sum(N)*100) %>%
  left_join(NbPracticeRegion) %>% 
  group_by(Region)%>%
  mutate(PctProjectTagged = N/NbPracticePerRegion*100)


TopRegion <- CountProjectRegion %>%  group_by(Region) %>% top_n(5, PctProjectTagged)
TopRegion <- TopRegion %>% mutate(Keywords = recode(Keywords, "Physical Urban Development" = "Integrated Urban Development"))

kwordsPlot <- unique(TopRegion$Keywords)
paletteToKeep <- palette[names(palette) %in% unique(TopRegion$Keywords)]
kwMissing <- kwordsPlot[!kwordsPlot %in% names(paletteToKeep)] 
paletteMissing <-distinctColorPalette(length(kwMissing))
names(paletteMissing) <- kwMissing

palette3 <- append(paletteToKeep,paletteMissing)
ggplot(TopRegion) + 
  geom_bar(aes(x= reorder_within(Keywords, PctProjectTagged, Region), y = PctProjectTagged, fill = Keywords), stat = "identity", show.legend = FALSE) + 
  scale_x_reordered()+
  scale_fill_manual(values= palette3)+
  facet_wrap( ~ Region, scales ="free", ncol = 2) + 
  geom_label(aes(label = N, x= reorder_within(Keywords, PctProjectTagged, Region), y = PctProjectTagged), position = position_stack(0.5), color = "black")+
  geom_text( data    = NbPracticeRegion,
             mapping = aes(x = -Inf, y = -Inf, label = paste("Nb Practices = ", NbPracticePerRegion, sep = "")),
             hjust   = -0.95,
             vjust   = -1.2, size = 3.5)+
  coord_flip() + 
  labs(y = "Pourcentage de pratiques étiquetés", x = "Mots-clés", 
       caption = "Sources : URBACT site web / PG. 2020")

ggsave( filename = "OUT/topKeyWordsUrbactPractices_Region.pdf", width = 8.3, height = 5.8, units = "in" )

# by city Size
NbPracticeSize <- PracticeKW20 %>% group_by(SizeClassURBACT) %>% summarise(NbPracticePerSize = n())

CountProjectSize <- LongPracticeUrbact%>% 
  group_by(SizeClassURBACT, Keywords) %>% 
  summarise(N = sum(value)) %>% 
  mutate(freq = N / sum(N)*100) %>%
  left_join(NbPracticeSize) %>% 
  group_by(SizeClassURBACT)%>%
  mutate(PctProjectTagged = N/NbPracticePerSize*100)


TopSize <-CountProjectSize %>%  group_by(SizeClassURBACT) %>% top_n(5, PctProjectTagged)
TopSize <- TopSize %>% mutate(Keywords = recode(Keywords, "Physical Urban Development" = "Integrated Urban Development"))

kwordsPlot <- unique(TopSize$Keywords)
paletteToKeep <- palette3[names(palette3) %in% kwordsPlot]
kwMissing <- kwordsPlot[!kwordsPlot %in% names(paletteToKeep)] 
paletteMissing <-distinctColorPalette(length(kwMissing))
names(paletteMissing) <- kwMissing

palette4 <- append(paletteToKeep,paletteMissing)

ggplot(TopSize) + 
  geom_bar(aes(x= reorder_within(Keywords, PctProjectTagged, SizeClassURBACT), y = PctProjectTagged, fill = Keywords), stat = "identity", show.legend = FALSE) + 
  scale_x_reordered()+
  scale_fill_manual(values= palette4)+
  facet_wrap( ~ SizeClassURBACT, scales ="free", ncol = 2) + 
  geom_label(aes(label = N, x= reorder_within(Keywords, PctProjectTagged, SizeClassURBACT), y = PctProjectTagged), position = position_stack(0.5), color = "black")+
  geom_text( data    = NbPracticeSize,
             mapping = aes(x = -Inf, y = -Inf, label = paste("Nb Practices = ", NbPracticePerSize, sep = "")),
             hjust   = -0.95,
             vjust   = -1.2, size = 3.5)+
  coord_flip() + 
  labs(y = "Pourcentage de pratiques étiquetées", x = "Mots-clés", 
       caption = "Sources : URBACT site web / PG. 2020")

ggsave( filename = "OUT/topKeyWordsUrbactPractices_Size.pdf", width = 8.3, height = 5.8, units = "in" )


### by Period

# library(questionr)
## Recodage de PracticeKW20$ClasseStart en PracticeKW20$ClasseStart_rec
PracticeKW20$ClasseStart <- fct_recode(PracticeKW20$ClasseStart,
               "Before 2010" = "Before 1990",
               "Before 2010" = "1990-2000",
               "Before 2010" = "2000-2007",
               "Before 2010" = "2007-2010")
PracticeKW20$ClasseStart <- as.character(PracticeKW20$ClasseStart)

NbPracticePeriod <- PracticeKW20 %>% group_by(ClasseStart) %>% summarise(NbPracticePerPeriod = n()) %>% filter(!is.na(ClasseStart))

LongPracticeUrbact <- PracticeKW20%>% select(18,23,30, 32:ncol(PracticeKW20)) %>% pivot_longer( -c(1:3), names_to = "Keywords") 

CountProjectPeriod <- LongPracticeUrbact%>% 
  group_by(ClasseStart, Keywords) %>% 
  summarise(N = sum(value)) %>% 
  mutate(freq = N / sum(N)*100) %>%
  left_join(NbPracticePeriod) %>% 
  group_by(ClasseStart)%>%
  mutate(PctProjectTagged = N/NbPracticePerPeriod*100)


TopPeriod <-CountProjectPeriod %>%  group_by(ClasseStart) %>% top_n(5, PctProjectTagged)
TopPeriod <- TopPeriod %>% mutate(Keywords = recode(Keywords, "Physical Urban Development" = "Integrated Urban Development"))
TopPeriod <- TopPeriod %>% filter(!is.na(ClasseStart))

kwordsPlot <- unique(TopPeriod$Keywords)
paletteToKeep <- palette4[names(palette4) %in% kwordsPlot]
kwMissing <- kwordsPlot[!kwordsPlot %in% names(paletteToKeep)] 
paletteMissing <-distinctColorPalette(length(kwMissing))
names(paletteMissing) <- kwMissing

palette5 <- append(paletteToKeep,paletteMissing)

ggplot(TopPeriod) + 
  geom_bar(aes(x= reorder_within(Keywords, PctProjectTagged, ClasseStart), y = PctProjectTagged, fill = Keywords), stat = "identity", show.legend = FALSE) + 
  scale_x_reordered()+
  scale_fill_manual(values= palette5)+
  facet_wrap( ~ ClasseStart, scales ="free", ncol = 2) + 
  geom_label(aes(label = N, x= reorder_within(Keywords, PctProjectTagged, ClasseStart), y = PctProjectTagged), position = position_stack(0.5), color = "black")+
  geom_text( data    = NbPracticePeriod,
             mapping = aes(x = -Inf, y = -Inf, label = paste("Nb Practices = ", NbPracticePerPeriod, sep = "")),
             hjust   = -0.95,
             vjust   = -1.2, size = 3.5)+
  coord_flip() + 
  labs(y = "Pourcentage de pratiques étiquetées", x = "Mots-clés", 
       caption = "Sources : URBACT site web / PG. 2020")

ggsave( filename = "OUT/topKeyWordsUrbactPractices_Period.pdf", width = 8.3, height = 5.8, units = "in" )

## Clean workspace

rm(palette3, palette4, palette5, paletteMissing, paletteToKeep)
rm(TopPeriod, TopRegion, TopSize, LongPracticeUrbact)
rm(kwordsPlot, kwMissing,CountProjectPeriod, CountProjectRegion, CountProjectSize)
rm(Bool_kwords, Bool_kwords18, Bool_kwords20)
rm(NbPracticePeriod,NbPracticeRegion,NbPracticeSize)

##################### CORRESPONDANCE ANALYSIS ON KW #################

# Filter KWords

KW_2018 <- PracticeKW18
summary(colSums(KW_2018[,31:ncol(KW_2018)]))
kwremove18 <- which(colSums(KW_2018[,31:ncol(KW_2018)]) < 5)
KW_2018 <- KW_2018 %>% select(!names(kwremove18))

KW_2020 <- PracticeKW20
summary(colSums(KW_2020[,32:ncol(KW_2020)]))

kwremove20 <- which(colSums(KW_2020[,32:ncol(KW_2020)]) < 3)

KW_2020 <- KW_2020 %>% select(!names(kwremove20))


### CA

#Kw 2018
DfCA<- KW_2018  %>% 
  select(c(1, 31:ncol(KW_2018 )))


rownames(DfCA) <- DfCA$Name
DfCA <- DfCA %>% select(-Name)
res.ca <- CA(DfCA)

explor(res.ca)

# Kw 2020

DfCA<- KW_2020  %>% 
  select(c(1, 32:ncol(KW_2020 )))


rownames(DfCA) <- DfCA$Name
DfCA <- DfCA %>% select(-Name) 
res.ca <- CA(DfCA)

explor(res.ca)

