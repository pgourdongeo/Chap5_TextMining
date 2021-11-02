## File preparation
setwd("~/Chap5_TextMining/URBACT_Network_Report/")
library(tidyverse)
library(skimr)


URBACT_Membership <- read.csv2("~/Chap5_TextMining/Data/URBACT/URBACT_Membership_GNidCorr_complete_V2.csv", stringsAsFactors = F)
  
URBACT_Project <- read.csv2("~/Chap5_TextMining/Data/URBACT/UrbactNetworks_complete_V2.csv", stringsAsFactors = F)
  
URBACT_text <- readRDS("DataProd/textUrbactNetwork.rds")

Politics <- read.csv2("~/Chap5_TextMining/Data/CountryPolitical/CountryInfo_PoliticalTypo.csv", 
                      encoding = "UTF-8", stringsAsFactors = FALSE)

GN_DB_City <- readRDS("~/Chap5_TextMining/Data/DBCity/DBCity_LauUmzFua.rds")


KeyWordBool <- readRDS("~/Chap5_TextMining/URBACT_Keywords_goodpractices/DataProd/ProjectURBACT_complete_kWordsBool.rds")

# filter Membership

URBACT_MembershipF <- URBACT_Membership %>% filter(Code_Network %in% URBACT_text$Code_Network)

skim(URBACT_MembershipF)

colnames(URBACT_MembershipF)

### Linkage (need an oriented graph)

LeadPartner <- URBACT_MembershipF %>% filter(City.Statut == "Lead Partner") %>% 
                select(Code_Network,"CodeSource" = geonameId, "Source" =asciiName)

Partner <- URBACT_MembershipF %>% filter(City.Statut == "Partner") %>% 
            select(Code_Network, "CodeTarget" = geonameId, "Target" = asciiName)


edgesDirected <- Partner %>% left_join(LeadPartner) %>% left_join(select(URBACT_text, Code_Network, txt))

LinkageDf <- edgesDirected %>% select(Source, Target, txt )
# Remove stopwords 

rm_words <- function(string, words) {
  stopifnot(is.character(string), is.character(words))
  spltted <- strsplit(string, " ", fixed = TRUE) # fixed = TRUE for speedup
  vapply(spltted, function(x) paste(x[!tolower(x) %in% words], collapse = " "), character(1))
}


LinkageDf$txt <- rm_words(LinkageDf$txt, tm::stopwords("en")) 
write.csv2(LinkageDf, "DataProd/urbactDirected_Report.csv", row.names = F, fileEncoding = "UTF-8")









### Metadata for Cortex manager

## 1. lead partner and project info
URBACT_Project_Report <- URBACT_Project %>% filter(Code_Network %in% URBACT_text$Code_Network)

## add politics info

Politics <- Politics %>% mutate(iso_a2 = recode(iso_a2,  "GB" = "UK"))
colnames(Politics)
URBACT_Project_Report <- URBACT_Project_Report %>% left_join(select(Politics, LeadPartnerCountry = iso_a2,
                                                        LocGovType_HorizontalPwrRelation, 
                                                        LocGovType_VerticalPwrRelation,
                                                        LocGovType_PoliticalLeadership,
                                                        LocGovType_MunicipalAdmin))

##  info on city 
LeadPartnerDF <- URBACT_MembershipF %>% filter(City.Statut== "Lead Partner")

URBACT_Project_Report <- URBACT_Project_Report %>% select(-CodeCity, -LeadPartner)
URBACT_Project_Report <- URBACT_Project_Report %>% left_join(select(LeadPartnerDF, CodeCity, Code_Network, geonameId, asciiName), by = "Code_Network") %>% distinct

GN_info <- GN_DB_City %>% select(geonameId, adminLevel, countryCode, population, CODE_LAU, 
                                 NAME_LAU, PopAdmin06,PopAdmin11, ID_UMZ, NAME_UMZ, POPUMZ11,  ID_FUA, NAME_FUA, POPFUA06)

URBACT_Project_Report <- URBACT_Project_Report %>% left_join(GN_info)

skim(URBACT_Project_Report)

# deal with NA when cities are outside UMZ (built-up urban area ) or FUA (Functional Urban areas)
colnames(URBACT_Project_Report)
URBACT_Project_Report <- URBACT_Project_Report %>% 
                          mutate_at(vars(c("ID_UMZ", "NAME_UMZ", "ID_FUA", "NAME_FUA" )), ~replace_na(., "Outside"))


# Add main keyword

KeyWordBool <- KeyWordBool %>% filter(Code_Network %in% URBACT_text$Code_Network)

KeyWordBool <- KeyWordBool %>% select(Code_Network, Governance, Economy, Inclusion, Environment, `Integrated Urban Development`)


URBACT_Project_Report <- URBACT_Project_Report %>% left_join(KeyWordBool)
colnames(URBACT_Project_Report)
URBACT_Project_Report <- URBACT_Project_Report %>% 
  select(- Main.Key.Word, -Additionnal.key.words, -Url, -End, -Aims, -Activities, - Members)


## Compute size class

# our own typo
labelClass <- c("4.Small City", "3.Medium-sized City", "2.Large City", "1.Very Large City")



cesDonnees <- URBACT_Project_Report$PopAdmin11
min<-min(cesDonnees)
max<- max(cesDonnees)
valBreaks <- c(min,50000,150000,500000,max)

URBACT_Project_Report$ClassePop<- cut(cesDonnees,
                              breaks = valBreaks,
                              labels = labelClass,
                              include.lowest = TRUE,
                              right= FALSE)

# URBACT typo

labelClass <- c("3. Less than 50 000", "2.Between 50 000 and 250 000", "1. More than 250 000")



cesDonnees <- URBACT_Project_Report$PopAdmin11
min<-min(cesDonnees)
max<- max(cesDonnees)
valBreaks <- c(min,50000,250000,max)

URBACT_Project_Report$SizeClassURBACT<- cut(cesDonnees,
                                      breaks = valBreaks,
                                      labels = labelClass,
                                      include.lowest = TRUE,
                                      right= FALSE)

skim(URBACT_Project_Report)

## add filename

URBACT_Project_Report$filename <- paste0("TexturbactNet_", URBACT_Project_Report$Code_Network)


URBACT_Project_Report <- URBACT_Project_Report %>% relocate(filename)

# Save file in csv (use tab separator for Cortext Manager)

write.table(URBACT_Project_Report, "DataProd/MetadataCortext_LeadPart_URBACTreport.csv", row.names = F, 
          fileEncoding = "UTF-8", sep="\t", qmethod = "double")




#########################################"
##### Urbact project CAH size of partners, export for Cortext


CAHSizeUrbactProject <- read.csv2("DataProd/CAH6_URBACTProjectSize.csv")
URBACT_Project_CAH <- URBACT_Project_Report %>% select(filename, Code_Network) %>% 
  left_join(select(CAHSizeUrbactProject, Code_Network, CAH, CAH_PartnerSize))%>% select(-Code_Network)


table(URBACT_Project_CAH$CAH_PartnerSize)

write.table(URBACT_Project_CAH, "DataProd/MetadataCortext_CAH_URBACTsize.csv", row.names = F, 
            fileEncoding = "UTF-8", sep="\t", qmethod = "double")
