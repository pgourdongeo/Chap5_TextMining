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

ProjectURBACT <- read.csv2("~/Chap5_TextMining/Data/URBACT/UrbactNetworks_complete.csv", 
                           stringsAsFactors = F)


#ProjectURBACT <- ProjectURBACT  %>% filter(!Phase == "Urbact I")
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
  ProjectURBACT[,listKwords[i]] <- grepl(listKwords[[i]],ProjectURBACT$Kwords)
}

### Convert True/False in 0/1
Bool_kwords <- sapply(X = ProjectURBACT[,23:ncol(ProjectURBACT)], FUN = function(x){ifelse(x == "TRUE", 1, 0)})
# Add bool keword to the dataframe
ProjectURBACT <- cbind(ProjectURBACT[,1:22],Bool_kwords)
# Erase old variable of KW
ProjectURBACT <- ProjectURBACT %>% select(-Main.Key.Word, -Additionnal.key.words, -Kwords)


## Create a list of KW with less than 3 occurrences 
kwordsToRemove <- which(colSums(ProjectURBACT[,20:ncol(ProjectURBACT)]) < 4)

# filter KW
FilteredProjectURBACT <- ProjectURBACT %>% select(!names(kwordsToRemove))



############ Create one dataframe for each phase
#By phase
KW_UrbactI <- ProjectURBACT %>% filter(Phase == "Urbact I")
kwremoveI <- which(colSums(KW_UrbactI[,20:ncol(KW_UrbactI)]) < 3)
KW_UrbactI <- KW_UrbactI %>% select(!names(kwremoveI))

KW_UrbactII <- ProjectURBACT %>% filter(Phase == "Urbact II")
kwremoveII <- which(colSums(KW_UrbactII[,20:ncol(KW_UrbactII)]) < 3)
KW_UrbactII <- KW_UrbactII %>% select(!names(kwremoveII))

KW_UrbactIII <- ProjectURBACT %>% filter(Phase == "Urbact III")
kwremoveIII <- which(colSums(KW_UrbactIII[,20:ncol(KW_UrbactIII)]) < 3)
KW_UrbactIII <- KW_UrbactIII %>% select(!names(kwremoveIII))

# Save

saveRDS(ProjectURBACT, "DataProd/ProjectURBACT_complete_kWordsBool.rds")


## Exploration CA

## All phases
# Keep only network names and Keywords
# All KW
DfCA <- ProjectURBACT %>% 
  select(c(3, 20:ncol(ProjectURBACT)))
#Filtered KW
DfCA<- FilteredProjectURBACT %>% 
  select(c(3, 20:ncol(FilteredProjectURBACT)))


# Perform CA

rownames(DfCA) <- DfCA$Name
DfCA <- DfCA %>% select(-Name)
res.ca <- CA(DfCA)



explor(res.ca)

## By phases

# Urbact I
DfCA<- KW_UrbactI  %>% 
  select(c(3, 20:ncol(KW_UrbactI )))


rownames(DfCA) <- DfCA$Name
DfCA <- DfCA %>% select(-Name)
res.ca <- CA(DfCA)

explor(res.ca)

# Urbact II

DfCA<- KW_UrbactII  %>% 
  select(c(3, 20:ncol(KW_UrbactII )))


rownames(DfCA) <- DfCA$Name
DfCA <- DfCA %>% select(-Name)
res.ca <- CA(DfCA)

explor(res.ca)

# Urbact III

DfCA<- KW_UrbactIII  %>% 
  select(c(3, 20:ncol(KW_UrbactIII )))


rownames(DfCA) <- DfCA$Name
DfCA <- DfCA %>% select(-Name)
res.ca <- CA(DfCA)

explor(res.ca)
