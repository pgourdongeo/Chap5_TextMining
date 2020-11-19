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
GoodPract <- read.csv2("~/Chap5_TextMining/Data/URBACT/GoodPracticesUrbact_WebscrapMay2018_Clean.csv", stringsAsFactors = F)

Attributes <- read.csv2("~/Chap5_TextMining/Data/URBACT/BestPracticesUrbact_Attributes_V2.csv", stringsAsFactors = F, encoding = "UTF-8")
Attributes <- Attributes %>% mutate(AllText = paste(Summary,Solution, Approach,Participation,Outcomes,Transferability, sep = ". "))

Kwords2020 <- read.csv2("~/Chap5_TextMining/Data/URBACT/GoodPracticesUrbact_KwordsUpdated2020.csv", stringsAsFactors = F, encoding = "UTF-8")

Attributes <- Attributes %>% left_join(select(Kwords2020,Name, Kwords2020 = Kwords ) )
skim(Attributes)


palette <- readRDS("DataProd/palette.rds")

## rec


rec <- st_read("~/Chap5_TextMining/Data/Geometry/rec_3035.geojson")

#EU 
sfEU <- st_read("~/Chap5_TextMining/Data/Geometry/fondEuropeLarge.geojson", stringsAsFactors = FALSE,crs = 3035)


################### ====  SUMMARY  Variable ===========###############

library(skimr)
Attributes <- Attributes %>% mutate(SizeClassURBACT = case_when(Pop< 50000 ~"Less than 50 000",
                                                                Pop> 250000 ~ "More than 250 000",
                                                                TRUE ~ "Between 50 000 and 250 000"))

Attributes <- Attributes %>% mutate(Region = recode(Region, "Western Asia" = "Southern Europe"))

AttributesClean <- Attributes

skim(Attributes)
library(GGally)
ggpairs(Attributes, columns = c(18,23,28))

#save the table of good practices 
write.csv(AttributesClean,file = "DataProd/GoodPracticesURBACT_Corpus.csv", row.names = FALSE, fileEncoding = "UTF-8")

############# ====  KEY WORDS ===========###############

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
Bool_kwords <- sapply(X = Attributes[,29:ncol(Attributes)], FUN = function(x){ifelse(x == "TRUE", 1, 0)})
# Add bool keword to the dataframe
PracticeKW <- cbind(Attributes[,1:28],Bool_kwords)
# Erase old variable of KW
PracticeKW <- PracticeKW %>% select( -Kwords)

# Number of KW by projects
summary(rowSums(Bool_kwords))


PracticeKW <- PracticeKW%>%  pivot_longer(-c(1:27),names_to = "MainKeyWord", values_to = "value")%>%  filter(value>0) %>% filter(!duplicated(CodePractices))


# Keep practice with two main key word
# PracticeKW <- PracticeKW %>% group_by(CodePractices)%>% mutate(MainKeyWord =  paste(MainKeyWord, collapse = "/")) %>% distinct() %>%ungroup()

unique(PracticeKW$MainKeyWord)


SummaryPractice <- PracticeKW %>% group_by(Region, SizeClassURBACT, MainKeyWord)%>% count()
library(ggalluvial)
ggplot(SummaryPractice,
       aes(axis1 = Region,
           axis2 = SizeClassURBACT,
           axis3 = MainKeyWord,
           y = n)) +
  geom_alluvium(aes(fill = Region)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Region", "Classe de taille", "Mot-Clé Principal"),
                   expand = c(.1, .1)) +
  scale_fill_viridis_d() +
  theme_minimal()

ggpairs(PracticeKW, columns = c(18,27,28))



tab1 <- table(PracticeKW$Region,PracticeKW$SizeClassURBACT)%>% prop.table(.)%>%as_tibble(.name_repair = c("unique"))
colnames(tab1) <- c("Region", "ClasseTaille", "n")

tab2 <- table(PracticeKW$Region,PracticeKW$MainKeyWord)%>% prop.table(.)%>%as_tibble(.name_repair = c("unique"))
colnames(tab2) <- c("Region", "MotClé", "n")

tab3 <- table(PracticeKW$SizeClassURBACT,PracticeKW$MainKeyWord)%>% prop.table(.)%>%as_tibble(.name_repair = c("unique"))
colnames(tab3) <- c("ClasseTaille", "MotClé", "n")

ggplot(tab1, aes(x = Region, y= ClasseTaille, fill = n))+
  geom_tile()+
  scale_fill_gradient2(low ="grey" , high = "orange")+
  geom_text(aes(label = round(n, 3))) + 
  # scale_y_discrete(labels=c(  "Less Than 50 000", "Between 50 000 and 250 000","More than 250 000") ) + 
  labs(caption = "P.Gourdon 2020",
        y= NULL, x = NULL, fill = "Fréquence")+ 
  theme(text = element_text(size = 16), axis.text.x = element_text(angle = 30))

ggplot(tab2, aes(x = Region, y= MotClé, fill = n))+
  geom_tile()+
  scale_fill_gradient2(low ="grey" , high = "orange")+
  geom_text(aes(label = round(n, 3))) + 
  # scale_y_discrete(labels=c(  "Less Than 50 000", "Between 50 000 and 250 000","More than 250 000") ) + 
  labs(caption = "P.Gourdon 2020",
       y= NULL, x = NULL, fill = "Fréquence")+ 
  theme(text = element_text(size = 16), axis.text.x = element_text(angle = 30))

ggplot(tab3, aes(x = ClasseTaille, y= MotClé, fill = n))+
  geom_tile()+
  scale_fill_gradient2(low ="grey" , high = "orange")+
  geom_text(aes(label = round(n, 3))) + 
  # scale_y_discrete(labels=c(  "Less Than 50 000", "Between 50 000 and 250 000","More than 250 000") ) + 
  labs(caption = "P.Gourdon 2020",
       y= NULL, x = NULL, fill = "Fréquence")+ 
  theme(text = element_text(size = 16), axis.text.x = element_text(angle = 30))




g1 <- ggplot(SummaryPractice) + 
  geom_bar( aes(x = reorder(Region,n), y= n, group = Region), stat = "identity", show.legend = FALSE) + 
  coord_flip() + 
  scale_y_continuous(n.breaks = 6)+
  labs(y = "Nombre de 'Bonnes Pratiques'", x = "Région")

library(forcats)
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
  select(City, SizeClassURBACT, Long, Lat) %>% 
  st_as_sf(., coords = c("Long", "Lat"), crs = 4326) %>%
  st_transform(3035) %>% filter(!duplicated(City))%>%
  mutate(SizeClassURBACT = fct_relevel(SizeClassURBACT, 
                                       "Less than 50 000", "Between 50 000 and 250 000", "More than 250 000")) 

myScaleBar <- data.frame(X = c(c(st_bbox(rec)[3]-900000), c(st_bbox(rec)[3]-400000)),
                         Y = c(c(st_bbox(rec)[2]+200000), c(st_bbox(rec)[2]+200000)))



g4 <- ggplot() + 
  geom_sf(data = sfEU, fill = "#bfbfbf", color = "white", size = 0.5) +
  geom_sf(data = SfcityGoodPractice ,
          mapping = aes( fill = SizeClassURBACT), shape= 21  , color = "gray80",show.legend = NA, size = 2) +
  scale_fill_brewer(palette = "YlOrRd")+
  annotate("text", label = "Source : URBACT site web/ PG. 2020",
           size = 2.2, 
           hjust = 1,
           x = c(st_bbox(rec)[3]), y = c(st_bbox(rec)[2]-130000)) +
  labs(x = "", y = "") +
  geom_line(data = myScaleBar, aes(x = X, y = Y), size = 0.5, color = "#333333") +
  annotate("text", label = "500 km", size = 2.5, color = "#333333", hjust = 0,
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

library(patchwork)


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






############### ===== Kwords 2020 CA and 2018  ======= #####


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
Bool_kwords18 <- sapply(X = Attributes18[,29:ncol(Attributes18)], FUN = function(x){ifelse(x == "TRUE", 1, 0)})

Bool_kwords20 <- sapply(X = Attributes20[,30:ncol(Attributes20)], FUN = function(x){ifelse(x == "TRUE", 1, 0)})
# Add bool keword to the dataframe
PracticeKW18 <- cbind(Attributes18[,1:28],Bool_kwords18)
PracticeKW20 <- cbind(Attributes20[,1:29],Bool_kwords20)


### Top keywords

# region


LongPracticeUrbact <- PracticeKW20%>% select(18,23,28, 30:ncol(PracticeKW20)) %>% pivot_longer( -c(1:3), names_to = "Keywords") 

Top  <- LongPracticeUrbact %>% group_by(Keywords)%>% summarise(N= sum(value))%>% top_n(70,N) %>% arrange(desc(N))



NbPracticeRegion <- PracticeKW20 %>% group_by(Region) %>% summarise(NbPracticePerRegion = n())


CountProjectRegion <- LongPracticeUrbact%>% 
  group_by(Region, Keywords) %>% 
  summarise(N = sum(value)) %>% 
  mutate(freq = N / sum(N)*100) %>%
  left_join(NbPracticeRegion) %>% 
  group_by(Region)%>%
  mutate(PctProjectTagged = N/NbPracticePerRegion*100)

library(randomcoloR)
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

# Size
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


### Period

library(questionr)
## Recodage de PracticeKW20$ClasseStart en PracticeKW20$ClasseStart_rec
PracticeKW20$ClasseStart <- fct_recode(PracticeKW20$ClasseStart,
               "Before 2010" = "Before 1990",
               "Before 2010" = "1990-2000",
               "Before 2010" = "2000-2007",
               "Before 2010" = "2007-2010")
PracticeKW20$ClasseStart <- as.character(PracticeKW20$ClasseStart)

NbPracticePeriod <- PracticeKW20 %>% group_by(ClasseStart) %>% summarise(NbPracticePerPeriod = n()) %>% filter(!is.na(ClasseStart))

LongPracticeUrbact <- PracticeKW20%>% select(18,23,28, 30:ncol(PracticeKW20)) %>% pivot_longer( -c(1:3), names_to = "Keywords") 

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



## CA

# Filter KWords

KW_2018 <- PracticeKW18
summary(colSums(KW_2018[,29:ncol(KW_2018)]))
kwremove18 <- which(colSums(KW_2018[,29:ncol(KW_2018)]) < 5)
KW_2018 <- KW_2018 %>% select(!names(kwremove18))

KW_2020 <- PracticeKW20
summary(colSums(KW_2020[,30:ncol(KW_2020)]))

kwremove20 <- which(colSums(KW_2020[,30:ncol(KW_2020)]) < 3)

KW_2020 <- KW_2020 %>% select(!names(kwremove20))


### CA

#Kw 2018
DfCA<- KW_2018  %>% 
  select(c(1, 29:ncol(KW_2018 )))


rownames(DfCA) <- DfCA$Name
DfCA <- DfCA %>% select(-Name)
res.ca <- CA(DfCA)

explor(res.ca)

# Kw 2020

DfCA<- KW_2020  %>% 
  select(c(1, 30:ncol(KW_2020 )))


rownames(DfCA) <- DfCA$Name
DfCA <- DfCA %>% select(-Name) 
res.ca <- CA(DfCA)

explor(res.ca)


## summary by variable





CountKWPeriod <- LongKeywordCIty%>% filter(!is.na(ClasseStart))%>%
  group_by(ClasseStart, MKw) %>% 
  summarise(N = sum(value)) %>% 
  mutate(freq = N / sum(N)*100)



CountKWSize <- LongKeywordCIty%>% 
  group_by(ClassePop, MKw) %>% 
  summarise(N = sum(value)) %>% 
  mutate(freq = N / sum(N)*100)

CountKWRegion <- LongKeywordCIty%>% 
  group_by(Region, MKw) %>% 
  summarise(N = sum(value)) %>% 
  mutate(freq = N / sum(N)*100)


ggplot(CountKWSize) + 
  geom_bar(aes(x= reorder_within(MKw, freq, ClassePop), y = freq, fill = MKw), stat = "identity", show.legend = FALSE) + 
  scale_x_reordered()+
  facet_wrap( ~ ClassePop, scales ="free", ncol = 2) + 
  geom_label(aes(label = N, x= reorder_within(MKw, freq, ClassePop), y = freq), position = position_stack(0.5), color = "black")+
  coord_flip() + 
  labs(y = "Fréquence", x = "Mots-clés Principaux", 
       caption = "Sources : URBACT Good Practices / PG. 2020")

#Map
## Sf city good practices


SfcityGoodPractice <- st_as_sf(LongKeywordCIty, coords = c("Long", "Lat"), crs = 4326)%>% st_transform(3035)
SfcityGoodPractice <- SfcityGoodPractice %>% arrange(desc(Pop))

myScaleBar <- data.frame(X = c(c(st_bbox(rec)[3]-900000), c(st_bbox(rec)[3]-400000)),
                         Y = c(c(st_bbox(rec)[2]+200000), c(st_bbox(rec)[2]+200000)))

s <-summary(SfcityGoodPractice$Pop)
s[[1]]
bks <- c(s[[1]],50000,200000, 500000,s[[6]])
lbs <- c("5 000", "50 000","200 000", "500 000", "7 M" )




KeyWOrdsGoodPractice <- ggplot() + 
  geom_sf(data = sfEU, fill = "#bfbfbf", color = "white", size = 0.5) +
 geom_sf(data = SfcityGoodPractice %>% arrange(Pop),
          mapping = aes(  size = Pop, fill = ClasseStart), shape= 21  , color = "gray80",show.legend = NA) +
  #   geom_sf(data = SfcityGoodPractice ,
  #          mapping = aes(  size = Pop), shape = 1, color = "grey80",  show.legend = NA) +
  scale_size(name = "Population Administrative",
             breaks = bks,
             labels = lbs,
             range = c(2, 11)) +
  scale_fill_brewer(palette = "Blues",
                    breaks =c("Before 1990", "1990-2000", "2000-2007", "2007-2010", "2010-2015", "2015-Present", NA),
                    labels = c("Before 1990", "1990-2000", "2000-2007", "2007-2010", "2010-2015", "2015-Present", NA),
                     direction = -1)+
  annotate("text", label = "Source : URBACT site web/ PG. 2020",
           size = 2.2, 
           hjust = 1,
           x = c(st_bbox(rec)[3]), y = c(st_bbox(rec)[2]-130000)) +
  labs(x = "", y = "") +
  geom_sf_text(data = SfcityGoodPractice %>% filter(Pop<100000), aes(label = City), size = 2.2, color = "#4d4d4d",
               check_overlap = TRUE, nudge_x = +100000, nudge_y = -50000) +
  geom_line(data = myScaleBar, aes(x = X, y = Y), size = 0.5, color = "#333333") +
  annotate("text", label = "500 km", size = 2.5, color = "#333333", hjust = 0,
           x = c(st_bbox(rec)[3]-800000), y = c(st_bbox(rec)[2]+280000)) +
  geom_sf(data = rec, fill = NA, color = "ivory4", size = 0.5) +
  coord_sf(crs = 3035, datum = NA,
           xlim = st_bbox(rec)[c(1,3)],
           ylim = st_bbox(rec)[c(2,4)]) + labs(color = "Période de mise en place de la bonne pratique") +
  theme_void() +
  theme(legend.position =  c(0.18, 0.60), 
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 7.5))



## display end save
pdf(file = "OUT/top50Map_centralities_EUCICOP_ETMUN.pdf", width = 8.3, height = 5.8, pagecentre = FALSE)
KeyWOrdsGoodPractice
dev.off()

ggsave( file="OUT/top50Map_centralities_EUCICOP_ETMUN.svg", plot=topsCitiesNetwork, width=8.3, height=5.8, device = "svg")


############# ====  TEXT MINING ===========###############

### Creation corpus 
library(R.temis)
corpus <- import_corpus("DataProd/GoodPracticesURBACT_Corpus.csv", "csv", "en", textcolumn = 20)



# Document terme matrix
dtm <- build_dtm(corpus)
###Affichage dtm
View(meta(corpus))
inspect(dtm)



###Creation dictionnaire
dico <- dictionary(dtm,remove_stopwords = TRUE)
View(dico)
###Affichage nuage de mots
set.seed(1)
cloud <- word_cloud(dtm, color="darkcyan", n=100, min.freq=10)

###Creation du nouveau tableau lexical sans mots outils
dtmsmo <- build_dtm(corpus, remove_stopwords = TRUE)

###Aide Ã  interpretation : concordance des mots (exemple : "terre")
concordances(corpus, dtm, "rsp")

###FrÃ©quences des termes, sur dico sans mots outils
freqterms <- frequent_terms(dtmsmo)
#Essai transformation en tableau (la fonction frequentterms = par dÃ©faut les 25 premiers)
freqtermsdf <- as.data.frame(freqterms) %>% 
  rownames_to_column()
colnames(freqtermsdf) <- c("Mot","Occurrences", "Freq")
#Essai transformation en graphique.CrÃ©ation du graphique de frequence des 25 premiers termes
library(tidyverse)
ggplot(freqtermsdf) +
  geom_bar(aes(x = reorder(Mot, Occurrences) , y = Occurrences), stat = "identity", fill= "darkcyan") +
  coord_flip() + 
  theme_bw()+ labs(x = "Mots")

#Essai ordre des lignes
ggplot(freqtermsdf) +
  geom_bar(aes(x = reorder(Mot,Freq) , y = Freq), stat = "identity", fill= "darkcyan") +
  coord_flip() + 
  theme_bw()+ labs(x = "Mots")



#Stats sur metadata (Ecole et Classe par ex)
library(questionr)
tab1 <- freq(meta(corpus)$CountryCode)  ##Montre rÃ©partition des phrases/prises de paroles par Ecole
tab2 <- freq(meta(corpus)$ClassePop)

###Bilan lexical (avec au moins une metadata. Par ex Ecole)
#Par Ecole
lexical_summary(dtmsmo, corpus,"Region")
lexical_summary(dtmsmo, corpus,"Region", unit= "global")
#Par Classe
lexical_summary(dtmsmo, corpus,"SizeClassURBACT")
lexical_summary(dtmsmo, corpus,"SizeClassURBACT", unit= "global")

###Mots spÃ©cifiques par mÃ©tadonnÃ©es
#Par Ecole
specific_terms(dtmsmo,meta(corpus)$"Region")
#Par Classe
specific_terms(dtmsmo,meta(corpus)$"SizeClassURBACT")

###Documents caractÃ©ristiques par mÃ©tadonnÃ©es
#Par Ecole
characteristic_docs(corpus,dtmsmo,meta(corpus)$"Region")
#Par Classe
characteristic_docs(corpus,dtmsmo,meta(corpus)$"ClassePop")

###Recherche de co-occurrences
#Exemple "terre"
cooc_terms(dtmsmo,"social")

cooc_terms(dtmsmo,"people", meta(corpus)$"ClassePop")

###Pour faire les analyses sur les stems plutÃ´t que sur les mots
stems <- combine_terms(dtmsmo, dico)

###FrÃ©quences des termes, sur dico sans mots outils
freqterms <- frequent_terms(stems, n=100)

####Faire des graphiques Ã  partir des calculs de frÃ©quences
#Transformation en data frame 
freqtermsdf <- as.data.frame(freqterms) %>% 
  rownames_to_column()
colnames(freqtermsdf) <- c("Mot","Occurrences", "Freq")
#CrÃ©ation du graphique, avec mots qui ont plus de 10 occurrences
ggplot(freqtermsdf[1:50,]) +
  geom_bar(aes(x = reorder(Mot,Freq) , y = Occurrences), stat = "identity", fill= "lightsteelblue2") +
  coord_flip() + 
  theme_bw()+
  theme (axis.title =element_text(size=12,  family="Wingdings 2"))+
  labs( x= "Stems", y="Occurrences dans le corpus (Nb)")




###Affichage nuage de mots
set.seed(1)
cloud <- word_cloud(stems,color="darkcyan", n=100, min.freq = 10)

frequent_terms(stems, variable = meta(corpus)$ClassePop, n=30) #pour voir frÃ©quences par variable
frequent_terms(stems, variable = meta(corpus)$Region, n=30)
frequent_terms(stems, variable = meta(corpus)$ClasseStart, n=30)



###Aide Ã  interpretation : concordance des mots (exemple : "terre")
concordances(corpus,dtm, "boule")


term_freq(stems,"loin", meta(corpus)$Classe)



###Mots spÃ©cifiques par mÃ©tadonnÃ©es
#Par Ecole
specific_terms(stems,meta(corpus)$"ClassePop")  ##Toujours mÃªme pb de "nb de dimensions incorrect".


###############""" SpacyR and tidy approach
# install.packages("spacyr")
library(spacyr)
library(tidytext)
library(wordcloud)
library(udpipe)
# spacy_install()
#spacy_download_langmodel(model = "en_core_web_lg" )
spacy_initialize(model = "en_core_web_lg")
#spacyr::spacy_finalize()

txt <- c(d1 = "spaCy is great at fast natural language processing.",
         d2 = "Mr. Smith spent two years in North Carolina.")

# process documents and obtain a data.table
parsedtxt <- spacy_parse(Attributes$AllText,)
parsedtxt


# Remove stopwords

CleanParsedText<- unnest_tokens(parsedtxt, word, token) %>%
  dplyr::anti_join(stop_words)


# lexique

Lexique <- CleanParsedText %>%
 count(word,  sort = T)

Top100 <- Lexique %>% top_n(100,n)
wordcloud(words = Top100$word, freq = Top100$n, min.freq = 1,
          max.words=100,  random.order=FALSE, rot.per=0.35)



stats <- txt_freq(CleanParsedText$pos)
ggplot(stats) +
  geom_bar(aes(x = reorder(key, -freq_pct), y = freq_pct), stat = "identity") +
  coord_flip() + theme_bw()

# nouns
stats <- subset(CleanParsedText, pos %in% c("NOUN")) 
stats <- txt_freq(stats$lemma) 
ggplot(stats[1:30, ]) +
  geom_bar(aes(x = reorder(key, freq_pct), y = freq_pct), stat = "identity") +
  coord_flip() + theme_bw() + labs(x = "Noms", y = "Fréquence (%)")

## adjectives
stats <- subset(CleanParsedText, pos %in% c("ADJ")) 
stats <- txt_freq(stats$lemma)
ggplot(stats[1:30, ]) +
  geom_bar(aes(x = reorder(key, freq_pct), y = freq_pct), stat = "identity") +
  coord_flip() + theme_bw() + labs(x = "Adjectifs", y = "Fréquence (%)")

## Verb
stats <- subset(CleanParsedText, pos  %in% c("VERB")) 
stats <- txt_freq(stats$lemma)
ggplot(stats[1:20, ]) +
  geom_bar(aes(x = reorder(key, freq_pct), y = freq_pct), stat = "identity") +
  coord_flip() + theme_bw() +  labs(x = "Verbes", y = "Fréquence (%)")




################ Key words

## Using RAKE

stats <- keywords_rake(x = CleanParsedText, term = "lemma", group = "sentence_id", 
                       relevant = CleanParsedText$pos %in% c("NOUN", "ADJ"), ngram_max = 3,n_min = 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))

statsTop <- stats %>% filter(ngram >1)%>% top_n(20, freq)
ggplot(statsTop) +
  geom_bar(aes(x = reorder(key, freq), y = freq), stat = "identity") +
  coord_flip() + theme_bw() +  labs(x = "Keywords identified by RAKE", y = "Fréquence")


## Using Pointwise Mutual Information Collocations
# remove stop words

stats <- keywords_collocation(x = CleanParsedText, term = "lemma", group = "sentence_id", ngram_max = 4, n_min = 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))

statsTop <- stats %>% top_n(50, freq)
ggplot(statsTop) +
  geom_bar(aes(x = reorder(key, freq), y = freq), stat = "identity") +
  coord_flip() + theme_bw() +  labs(x = "Keywords identified by PMI Collocation", y = "Fréquence")


## Tf idf 

Ntermdoc <- CleanParsedText %>% count(lemma, doc_id)
TfIdfDoc <- Ntermdoc %>% bind_tf_idf(lemma, doc_id, n)

TopTfIdf <- TfIdfDoc %>% group_by(doc_id)%>% top_n(10, tf_idf)

# tf idf by category

Attributes <- Attributes %>% mutate(doc_id = paste0("text", CodePractices))

CleanParsedText <- CleanParsedText %>% left_join(select(Attributes, 17,18,22,23,28,29))

# Region
Ntermdoc <- CleanParsedText %>% filter(!pos == "PROPN" & !pos == "NUM" & !pos == "X")%>% filter(entity == "") %>%count(lemma, Region)
TfIdfDoc <- Ntermdoc %>% bind_tf_idf(lemma, Region, n)

TopTfIdf <- TfIdfDoc %>% group_by(Region)%>% top_n(10, tf_idf)

ggplot(TopTfIdf) + 
  geom_bar(aes(x= reorder_within(lemma, tf_idf, Region), y = tf_idf, fill = Region), stat = "identity", show.legend = FALSE) + 
  scale_x_reordered()+
  facet_wrap( ~ Region, scales ="free", ncol = 2) + 
  coord_flip() + 
  labs(y = "TF-IDF", x = "Lemmes", 
       caption = "Sources : EUCICOP 2019 ; KEEP Closed Projects 2000-2019 / PG. 2020")

# city size
Ntermdoc <- CleanParsedText %>% filter(!pos == "PROPN" & !pos == "NUM" & !pos == "X")%>% filter(entity == "") %>%count(lemma, SizeClassURBACT)
TfIdfDoc <- Ntermdoc %>% bind_tf_idf(lemma, SizeClassURBACT, n)

TopTfIdf <- TfIdfDoc %>% group_by(SizeClassURBACT)%>% top_n(10, tf_idf)

ggplot(TopTfIdf) + 
  geom_bar(aes(x= reorder_within(lemma, tf_idf, SizeClassURBACT), y = tf_idf, fill = SizeClassURBACT), stat = "identity", show.legend = FALSE) + 
  scale_x_reordered()+
  facet_wrap( ~ SizeClassURBACT, scales ="free", ncol = 2) + 
  coord_flip() + 
  labs(y = "TF-IDF", x = "Lemmes", 
       caption = "Sources : EUCICOP 2019 ; KEEP Closed Projects 2000-2019 / PG. 2020")

# period
Ntermdoc <- CleanParsedText %>% filter(!pos == "PROPN" & !pos == "NUM" & !pos == "X" & !entity == "DATE_B")%>% 
  filter(entity == "") %>%
  count(lemma, ClasseStart) %>%
  filter(!is.na(ClasseStart))

TfIdfDoc <- Ntermdoc %>% bind_tf_idf(lemma, ClasseStart, n)

TopTfIdf <- TfIdfDoc %>% group_by(ClasseStart)%>% top_n(10, tf_idf)

ggplot(TopTfIdf) + 
  geom_bar(aes(x= reorder_within(lemma, tf_idf, ClasseStart), y = tf_idf, fill = ClasseStart), stat = "identity", show.legend = FALSE) + 
  scale_x_reordered()+
  facet_wrap( ~ ClasseStart, scales ="free", ncol = 2) + 
  coord_flip() + 
  labs(y = "TF-IDF", x = "Lemmes", 
       caption = "Sources : EUCICOP 2019 ; KEEP Closed Projects 2000-2019 / PG. 2020")

# KW by caterory

cat <- CleanParsedText %>% 
  filter(!pos == "PROPN" & !pos == "NUM" & !pos == "X")%>% 
  filter(entity == "") %>% 
  filter(SizeClassURBACT == "Less than 50 000")
stats <- keywords_rake(x = cat, term = "lemma", group = "sentence_id", 
                       relevant = cat$pos %in% c("NOUN", "ADJ"), ngram_max = 3,n_min = 1)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))

statsTop <- stats %>% top_n(20, freq)
ggplot(statsTop) +
  geom_bar(aes(x = reorder(key, freq), y = freq), stat = "identity") +
  coord_flip() + theme_bw() +  labs(x = "Keywords identified by RAKE", y = "Fréquence")


## Using Pointwise Mutual Information Collocations
# remove stop words

stats <- keywords_collocation(x = cat, term = "lemma", group = "sentence_id", ngram_max = 4, n_min = 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))

statsTop <- stats %>% top_n(20, freq)
ggplot(statsTop) +
  geom_bar(aes(x = reorder(key, freq), y = freq), stat = "identity") +
  coord_flip() + theme_bw() +  labs(x = "Keywords identified by PMI Collocation", y = "Fréquence")


#LDA
library(topicmodels)

data <- CleanParsedText %>%  filter(!pos == "PROPN" & !pos == "NUM" & !pos == "X")%>% 
  filter(entity == "") 


x4 <- document_term_frequencies(data[,c("doc_id","lemma")])

x5 <- document_term_frequencies_statistics(x4)

### DTM filter
dtm <- document_term_matrix(x4)
# dtm <- dtm_remove_tfidf(dtm, top = 1500)

dtm<- dtm_remove_tfidf(dtm, prob = 0.20)
#frq filter
lexiquecat <- data %>%
  group_by(lemma) %>%
  mutate(word_total = n()) %>%
  ungroup() %>%
  filter(word_total > 20)

#tf idf filter

Ntermdoc <- data %>% 
  count(lemma, sentence_id) 

TfIdfDoc <- Ntermdoc %>% bind_tf_idf(lemma, sentence_id, n)

lexiquecat <- TfIdfDoc %>% top_frac(0.8, tf_idf)
# convert into a document-term matrix
# with document names such as sci.crypt_14147
cat_dtm <- lexiquecat %>%
  count(sentence_id, lemma) %>%
  cast_dtm(sentence_id, lemma, n)

# choose K with Ldatuning
# library(ldatuning)
# saveRDS(dtm, "DataProd/dtm_test.rds")
# 
# 
# ktopics <- FindTopicsNumber(dtm,
#   topics = seq(from = 2, to = 50, by = 1),
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 2016),
#   mc.cores = 2L,
#   verbose = TRUE)
# k 26 or 14

library(topicmodels)
cat_lda <- LDA(dtm, k = 14, control = list(seed = 2016), method = "Gibbs")


cat_lda %>%
  tidy() %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered()
