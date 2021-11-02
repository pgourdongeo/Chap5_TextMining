###############################   URBACT    ##################################
#                               
#                          
# DESCRIPTION : Préparation d'une table des projets URBACT avec les mots-clés 
#               en colonnes booléennes 
#
# 
############################################################################## PG aout 2020

setwd("URBACT_Keywords_goodpractices/")

# Packages
library(tidyverse)
library(FactoMineR)
library(explor)
# Data

ProjectURBACT <- read.csv2("~/Chap5_TextMining/Data/URBACT/UrbactNetworks_complete_V2.csv", 
                           stringsAsFactors = F)

  UrbactMembership <- read.csv2("~/Chap5_TextMining/Data/URBACT/URBACT_Membership_GNidCorr_complete_V2.csv", 
                                 stringsAsFactors = F)

  # old <- read.csv2("~/Chap5_TextMining/Data/URBACT/UrbactNetworks_complete.csv",  stringsAsFactors = F)
  
#### Summary Participation period (cf chap 3)
NparticipByphase <- UrbactMembership %>% 
                    left_join(select(ProjectURBACT, Code_Network, Phase))  %>%
                    filter(!Phase == "Urbact I" )%>% group_by(Phase, Region) %>% 
                    summarise(N = n())%>%
                    group_by(Phase) %>%
                    mutate(freqPhase = N / sum(N)*100)

nbParticipationbuRegion <- UrbactMembership %>% 
  group_by( Region) %>% 
  summarise(N = n())%>%
  mutate(freqPhase = N / sum(N)*100)

nbCitybyregion <-  UrbactMembership %>% filter(!duplicated(geonameId))%>%
  group_by( Region) %>% 
  summarise(N = n())%>%
  mutate(freqPhase = N / sum(N)*100)

NleadByPhase <-  UrbactMembership %>% 
                left_join(select(ProjectURBACT, Code_Network, Phase))  %>%
                filter(!Phase == "Urbact I" & City.Statut == "Lead Partner" )%>% group_by(Phase, Region) %>% 
                summarise(N = n())%>%
                group_by(Phase) %>%
                mutate(freqPhase = N / sum(N)*100)
  
#### ==== Preparation Keywords boolean =====
ProjectURBACT <- ProjectURBACT  %>% filter(!Phase == "Urbact I")
# Create a unique column with all key words
ProjectURBACT <- ProjectURBACT %>% mutate(Kwords = paste(Main.Key.Word, Additionnal.key.words, sep = "/ "))

##### This part is useful when you want to separate main KW and additional ones
# Main KWords

# listMainKeyWords<- as.vector(unique(str_trim(unlist(str_split(ProjectURBACT$Main.Key.Word, "/ ")))))
# 
# ProjectURBACT[paste0(listMainKeyWords,"_Main")] <- NA
# 
# for(i in 1:length(listMainKeyWords)) {
#   ProjectURBACT[,listMainKeyWords[i]] <- grepl(listMainKeyWords[[i]],ProjectURBACT$Main.Key.Word)
# }
# 
# #
# 
# listAdditionnalKwords <- as.vector(unique(str_trim(unlist(str_split(ProjectURBACT$Additionnal.key.words, "/ ")))))
# 
# sort(unique(listAdditionnalKwords))
# 
# listAdditionnalKwords <- listAdditionnalKwords[listAdditionnalKwords != ""]
# 
# for(i in 1:length(listAdditionnalKwords)) {
#   ProjectURBACT[,listAdditionnalKwords[i]] <- grepl(listAdditionnalKwords[[i]],ProjectURBACT$Additionnal.key.words)
# }
# 
# ### Convert True/False
# 
# 
# Bool_kwords <- sapply(X = ProjectURBACT[,22:93], FUN = function(x){ifelse(x == "TRUE", 1, 0)})
# ProjectURBACT <- cbind(ProjectURBACT[,1:21],Bool_kwords)

## Get a list of all keywords (all phases)
listKwords <- as.vector(unique(str_trim(unlist(str_split(ProjectURBACT$Kwords, "/")))))

# Check duplicated
sort(unique(listKwords))
duplicated(unique(listKwords))

# Remove empty one

listKwords <- listKwords[listKwords != ""]

# Spread KW into columns
for(i in 1:length(listKwords)) {
  ProjectURBACT[,listKwords[i]] <- as.integer(grepl(listKwords[[i]],ProjectURBACT$Kwords))
}

### Convert True/False in 0/1
Bool_kwords <- sapply(X = ProjectURBACT[,21:ncol(ProjectURBACT)], FUN = function(x){ifelse(x == "TRUE", 1, 0)})
# Add bool keword to the dataframe
ProjectURBACT <- cbind(ProjectURBACT[,1:20],Bool_kwords)
# Erase old variable of KW
ProjectURBACT <- ProjectURBACT %>% select(-Main.Key.Word, -Additionnal.key.words, -Kwords)

# Number of KW by projects
summary(rowSums(Bool_kwords))

#
MaxKeywordsNb <- ProjectURBACT %>% select(1,2, 16,20:ncol(ProjectURBACT) ) %>% mutate(Nkw = rowSums(.[4:44]))%>% select(-c(4:44)) %>% filter(Nkw>6)
## Create a list of KW with less than 3 occurrences 
kwordsToRemove <- which(colSums(ProjectURBACT[,18:ncol(ProjectURBACT)]) < 4)

# filter KW
FilteredProjectURBACT <- ProjectURBACT %>% select(!names(kwordsToRemove))



############ Create one dataframe for each phase
#By phase
KW_UrbactI <- ProjectURBACT %>% filter(Phase == "Urbact I")
kwremoveI <- which(colSums(KW_UrbactI[,20:ncol(KW_UrbactI)]) < 3)
KW_UrbactI <- KW_UrbactI %>% select(!names(kwremoveI))

KW_UrbactII <- ProjectURBACT %>% filter(Phase == "Urbact II")
kwremoveII <- which(colSums(KW_UrbactII[,18:ncol(KW_UrbactII)]) < 3)
KW_UrbactII <- KW_UrbactII %>% select(!names(kwremoveII))

KW_UrbactIII <- ProjectURBACT %>% filter(Phase == "Urbact III")
kwremoveIII <- which(colSums(KW_UrbactIII[,18:ncol(KW_UrbactIII)]) < 3)
KW_UrbactIII <- KW_UrbactIII %>% select(!names(kwremoveIII))

# Save

saveRDS(ProjectURBACT, "DataProd/ProjectURBACT_complete_kWordsBool.rds")


#### ===== Exploration CA =====

## All phases
# Keep only network names and Keywords
# All KW
DfCA <- ProjectURBACT %>% 
  select(c(1, 18:ncol(ProjectURBACT)))
#Filtered KW
DfCA<- FilteredProjectURBACT %>% 
  select(c(1, 18:ncol(FilteredProjectURBACT)))


# Perform CA

rownames(DfCA) <- DfCA$Name
DfCA <- DfCA %>% select(-Name)
res.ca <- CA(DfCA)



explor(res.ca)

## By phases

# # Urbact I
# DfCA<- KW_UrbactI  %>% 
#   select(c(1, 18:ncol(KW_UrbactI )))
# 
# 
# rownames(DfCA) <- DfCA$Name
# DfCA <- DfCA %>% select(-Name)
# res.ca <- CA(DfCA)
# 
# explor(res.ca)

# Urbact II

DfCA<- KW_UrbactII  %>% 
  select(c(1, 18:ncol(KW_UrbactII )))


rownames(DfCA) <- DfCA$Name
DfCA <- DfCA %>% select(-Name)
res.ca <- CA(DfCA)

explor(res.ca)

# Urbact III

DfCA<- KW_UrbactIII  %>% 
  select(c(1, 18:ncol(KW_UrbactIII )))


rownames(DfCA) <- DfCA$Name
DfCA <- DfCA %>% select(-Name)
res.ca <- CA(DfCA)

explor(res.ca)


### ===== Coocurence Networks =====

# With Urbact II and III
DfCA<- FilteredProjectURBACT %>% 
  select(c(1, 18:ncol(FilteredProjectURBACT)))

rownames(DfCA) <- DfCA$Name
DfCA <- DfCA %>% select(-Name)
## Matrix of cooccurrence between key words 
KW_m <- as.matrix(DfCA)

KW_m <- t(KW_m) %*% KW_m

## Nb of projects by key words
nbproject <- diag(KW_m) %>% as.data.frame()
nbproject$name <- rownames(nbproject)
nbproject<- nbproject %>% rename("nbProjects" = '.')

## Set the matrix diagonal to 0 (to avoid loop in the graph)

 diag(KW_m) <- 0

 
 # crete igraph object
library(igraph)

KW_g <- graph.adjacency(KW_m, mode = "undirected", weighted = TRUE) 
is.connected(KW_g)
summary(E(KW_g)$weight)
# set basic nodes attributes

V(KW_g)$degree <- degree(KW_g )
V(KW_g)$stength <- strength(KW_g)
KW_g <-  set_vertex_attr(KW_g,"NbProj", index = nbproject$name, value = nbproject$nbProjects)

degreeKW <- as_data_frame(KW_g, what = "vertices")
# clustering 

Louvain <- cluster_louvain(KW_g, weights = E(KW_g)$weight)
Louvain$modularity
Fgreedy <- cluster_fast_greedy(KW_g, weights = E(KW_g)$weight)
V(KW_g)$louvain  <- Louvain$memberships

## plot the grapg
library(tidygraph)
library(ggraph)

# transfo to tidygraph object
Td_KW_g <- as_tbl_graph(KW_g)

# create a sub graph

FilterG <- Td_KW_g %>% 
  # activate(edges) %>% 
  # filter(weight > 4 ) %>% 
  activate(nodes) %>% 
  mutate(gp = group_spinglass())%>%
  activate(edges) %>% 
  filter(weight > 5 ) %>% 
  activate(nodes) %>% 
  filter(!node_is_isolated())

ggraph(FilterG, layout = "fr") +
geom_edge_diagonal(aes(edge_width = weight,alpha = weight), start_cap = circle(4, 'mm'),
               end_cap = circle(4, 'mm'))+
  scale_edge_alpha(range = c(0.05,0.4))+
  geom_node_point( aes(color = as.character(gp), size = NbProj), alpha= 0.9  )+ 
  geom_node_text(aes(label = name),repel = TRUE, size = 3)+
  scale_size(range = c(1,15))+
  labs(x = "long", y = "lat",
       color = "Communautés fast greedy", size = "Nb projets",
       caption = "Sources : EUCICOP 2019/ URBACT. PG 2020")




###### Create a matrix to keep the maximum of cooccurrences by keywords (Major flows)

### Create new 
# transform Max into boolean matrix
library(matrixStats)
MaxCooc <- +(KW_m==rowMaxs(KW_m))

# create igraph object

MaxCooc.g <- graph.adjacency(MaxCooc, mode = "directed")

MaxCooc.g <-  set_vertex_attr(MaxCooc.g,"NbProj", index = nbproject$name, value = nbproject$nbProjects)

# clustering with infomap
cluster <- cluster_infomap(MaxCooc.g, modularity = FALSE, nb.trials = 100)
 
V(MaxCooc.g)$infomap <- cluster$membership


## plot the graph
Td_maxCooc <- as_tbl_graph(MaxCooc.g)

# filter
FilterG <- Td_maxCooc %>% 
  activate(nodes) %>% 
  mutate(gp = group_spinglass())


ggraph(FilterG, layout = "fr")+
  geom_edge_parallel(arrow = arrow(length = unit(1.8, 'mm')), 
                start_cap = circle(4, 'mm'),
                end_cap = circle(6, 'mm'), alpha = 0.3) +
  geom_node_point( aes(color = as.character(gp),size = NbProj), alpha= 0.7  )+ 
  geom_node_text(aes(label = name) ,size = 3, repel = TRUE)+
  scale_size(range = c(0.5,12))+ 
  labs( size = "Nb projets", color = "Communauté (spinglass)",
       caption = "Notes : les liens orientés représentent le maximum de co-occurrences pour chaque mot-clé.\nExemple : 'Ageing' apparait le plus souvent avec 'Health' qui, lui-même apparait le plus souvent avec 'Integrated Urban Development'\n\nSources : EUCICOP 2019/ URBACT. PG 2020")+
  guides(colour = guide_legend(override.aes = list(shape = 15, size = 4)))+
  theme(legend.position="bottom")

ggsave(filename = "OUT/MajorFlow_KeyWordsCoocURBACT_2_3.pdf",width = 11.7, height = 8.3)



ggraph(FilterG, layout = "fr")+
  geom_edge_parallel(arrow = arrow(length = unit(2.5, 'mm')), 
                     aes(start_cap = label_rect(node1.name),
                         end_cap = label_rect(node2.name)), alpha = 0.3) +

  geom_node_text(aes(label = name, color = as.character(gp)), size = 3)+
  scale_size(range = c(2,6))+ 
  labs( size = "Nb projets", color = "Communauté (infomap)",
        caption = "Notes : les liens orientés représentent le maximum de co-occurrences pour chaque mot-clé.\nExemple : 'Ageing' apparait le plus souvent avec 'Health' qui, lui-même apparait le plus souvent avec 'Integrated Urban Development'\n\nSources : EUCICOP 2019/ URBACT. PG 2020")+
  guides(colour = guide_legend(override.aes = list(shape = 15, size = 4)))+
  theme(legend.position="bottom")



## ==== Summary Keywords ======

########### Top Keywords by phase


NbprojetPhase <- ProjectURBACT %>% group_by(Phase) %>% summarise(NbProjetsPerPhase = n())

LongProjectUrbact <- ProjectURBACT %>% select(1,12,16, 18:ncol(ProjectURBACT)) %>% pivot_longer( -c(1:3), names_to = "Keywords") 

CountProjectPhase <- LongProjectUrbact%>% 
  group_by(Phase, Keywords) %>% 
  summarise(N = sum(value)) %>% 
  mutate(freq = N / sum(N)*100) %>% 
  left_join(NbprojetPhase) %>% 
  group_by(Phase)%>%
  mutate(PctProjectTagged = N/NbProjetsPerPhase*100)


Top <- CountProjectPhase %>%  group_by(Phase) %>% top_n(9, PctProjectTagged)

## Define color vect 
library(randomcoloR)
library(RColorBrewer)
palette <- brewer.pal(12, "Set3")

names(palette) <- sort(unique(Top$Keywords))

saveRDS(palette, file = "DataProd/palette.rds")
ggplot(Top) + 
  geom_bar(aes(x= reorder_within(Keywords, PctProjectTagged, Phase), y = PctProjectTagged, fill = Keywords), stat = "identity", show.legend = FALSE) + 
  scale_x_reordered()+
  scale_fill_manual(values= palette)+
  facet_wrap( ~ Phase, scales ="free", ncol = 2) + 
  geom_label(aes(label = N, x= reorder_within(Keywords, PctProjectTagged, Phase), y = PctProjectTagged), position = position_stack(0.5), color = "black")+
  geom_text( data    = NbprojetPhase,
             mapping = aes(x = -Inf, y = -Inf, label = paste("Nb Proj = ", NbProjetsPerPhase, sep = "")),
             hjust   = -1.7,
             vjust   = -1.2, size = 3.5)+
  coord_flip() + 
  labs(y = "Pourcentage de projets étiquetés", x = "Mots-clés", 
       caption = "Sources : EUCICOP 2019 ; KEEP Closed Projects 2000-2019 / PG. 2020")



ggsave( filename = "OUT/topKeyWordsUrbact_phase.pdf", width = 8.3, height = 5.8, units = "in" )


##################" Top Keywords by period

ProjectURBACT$Start_rec <- as.character(ProjectURBACT$Start)
ProjectURBACT$Start_rec[ProjectURBACT$Start == "2015"] <- "2015-2016"
ProjectURBACT$Start_rec[ProjectURBACT$Start == "2008"] <- "2007-2008"
ProjectURBACT$Start_rec[ProjectURBACT$Start == "2009"] <- "2009-2010"
ProjectURBACT$Start_rec[ProjectURBACT$Start == "2010"] <- "2009-2010"
ProjectURBACT$Start_rec[ProjectURBACT$Start == "2007"] <- "2007-2008"
ProjectURBACT$Start_rec[ProjectURBACT$Start == "2016"] <- "2015-2016"

NbprojetPeriod <- ProjectURBACT %>% group_by(Start_rec) %>% summarise(NbProjetsPerPeriod = n())

LongProjectUrbact <- ProjectURBACT %>% select(1,12,16, 18:ncol(ProjectURBACT)) %>% pivot_longer( -c(1:3,47), names_to = "Keywords") 

CountProjectPeriod <- LongProjectUrbact%>% 
                      group_by(Start_rec, Keywords) %>% 
                    summarise(N = sum(value)) %>% 
                    mutate(freq = N / sum(N)*100) %>% 
                    left_join(NbprojetPeriod) %>% 
                    group_by(Start_rec)%>%
                    mutate(PctProjectTagged = N/NbProjetsPerPeriod*100)


Top <- CountProjectPeriod%>%  group_by(Start_rec) %>% top_n(5, PctProjectTagged)



ggplot(Top) + 
  geom_bar(aes(x= reorder_within(Keywords, PctProjectTagged, Start_rec), y = PctProjectTagged, fill = Keywords), stat = "identity", show.legend = FALSE) + 
  scale_x_reordered()+
  facet_wrap( ~ Start_rec, scales ="free", ncol = 2) + 
  geom_label(aes(label = N, x= reorder_within(Keywords, PctProjectTagged, Start_rec), y = PctProjectTagged), position = position_stack(0.5), color = "black")+
  geom_text( data    = NbprojetPeriod,
             mapping = aes(x = -Inf, y = -Inf, label = paste("Nb Proj = ", NbProjetsPerPeriod, sep = "")),
             hjust   = -3,
             vjust   = -1.2, size = 2.5)+
  coord_flip() + 
  labs(y = "Pourcentage de projets étiquetés", x = "Mots-clés", 
       caption = "Sources : EUCICOP 2019 ; KEEP Closed Projects 2000-2019 / PG. 2020")



ggsave( filename = "OUT/topKeyWordsUrbact_periods.pdf", width = 8.3, height = 8.3, units = "in" )


############## Top Keywords by region
NbprojetRegion <- ProjectURBACT %>% group_by(Region) %>% summarise(NbProjetsPerRegion = n())

CountProjectRegion <- LongProjectUrbact%>% 
  group_by(Region, Keywords) %>% 
  summarise(N = sum(value)) %>% 
  mutate(freq = N / sum(N)*100) %>% 
  left_join(NbprojetRegion) %>% 
  group_by(Region)%>%
  mutate(PctProjectTagged = N/NbProjetsPerRegion*100)


Top <- CountProjectRegion %>%  group_by(Region) %>% top_n(5, PctProjectTagged)

palette2 <- palette[names(palette) %in% unique(Top$Keywords)]

palette2 <- append(palette2, "#F8E473")
names(palette2) <- append(names(palette2)[-10], "Priority Neighbourhoods")
ggplot(Top) + 
  geom_bar(aes(x= reorder_within(Keywords, PctProjectTagged, Region), y = PctProjectTagged, fill = Keywords), stat = "identity", show.legend = FALSE) + 
  scale_x_reordered()+
  scale_fill_manual(values= palette2)+
  facet_wrap( ~ Region, scales ="free", ncol = 2) + 
  geom_label(aes(label = N, x= reorder_within(Keywords, PctProjectTagged, Region), y = PctProjectTagged), position = position_stack(0.5), color = "black")+
  geom_text( data    = NbprojetRegion,
             mapping = aes(x = -Inf, y = -Inf, label = paste("Nb Proj = ", NbProjetsPerRegion, sep = "")),
             hjust   = -2.1,
             vjust   = -1.2, size = 3)+
  coord_flip() + 
  labs(y = "Pourcentage de projets étiquetés", x = "Mots-clés", 
       caption = "Sources : EUCICOP 2019 ; KEEP Closed Projects 2000-2019 / PG. 2020")



ggsave( filename = "OUT/topKeyWordsUrbact_region.pdf", width = 8.3, height = 5.8, units = "in" )


