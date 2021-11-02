###############################  CCD project  ##################################
#                               
#                          
# DESCRIPTION : Analyse et préparatation des fichiers pour le projet City Centre Doctor
#
# 
############################################################################## PG 2021




## File preparation
setwd("~/Chap5_TextMining/URBACT_Network_Report/")
library(tidyverse)
library(skimr)
library(igraph)
library(ggraph)
library(tidygraph)
library(tidytext)
library(skimr)
# library(GGally)
library(stargazer)
library(patchwork)
library(wordcloud)
library(udpipe)
library(ggrepel)
library(topicmodels)


URBACT_Membership <- read.csv2("~/Chap5_TextMining/Data/URBACT/URBACT_Membership_GNidCorr_complete_V2.csv", stringsAsFactors = F)

URBACT_Project <- read.csv2("~/Chap5_TextMining/Data/URBACT/UrbactNetworks_complete_V2.csv", stringsAsFactors = F)


Politics <- read.csv2("~/Chap5_TextMining/Data/CountryPolitical/CountryInfo_PoliticalTypo.csv", 
                      encoding = "UTF-8", stringsAsFactors = FALSE)

GN_DB_City <- readRDS("~/Chap5_TextMining/Data/DBCity/DBCity_LauUmzFua.rds")

CCD_cities <- read.csv2("~/Chap5_TextMining/Data/URBACT/CCD_member_DocId.csv", stringsAsFactors = F)
CCD_cities <- CCD_cities %>% mutate(doc_id = trimws(doc_id))


#### ==== CORPUS FROM TEXT FILES ==== 


dir <- "~/Chap5_TextMining/Data/URBACT/LAP_CCD/"
all_txts <- list.files(dir,pattern = '*.txt', full.names = T)

df<- map_df(all_txts, ~ tibble(txt = read_file(.x)) %>% mutate(doc_id = basename(.x)))

df <- df %>% mutate(Code_Part = str_remove(doc_id, ".txt"))

# saveRDS(df, "DataProd/textLAP_CCD.rds")
# write.csv2(df, "DataProd/textLAP_CCD.csv", fileEncoding = "UTF-8", row.names = F)


#### ==== PARTICIPATION and Network for PARTNER CITIES of CCD ==== 



### Member cities


CCD_Membership <- URBACT_Membership %>% filter(Code_Network == "11")

CCD_Membership <- CCD_Membership %>% left_join( select(GN_DB_City, geonameId, adminLevel, countryCode, population, CODE_LAU, 
                                                       NAME_LAU, PopAdmin06,PopAdmin11, ID_UMZ, NAME_UMZ, POPUMZ11,  ID_FUA, NAME_FUA, POPFUA06))

CCD_Membership$population <- as.numeric(CCD_Membership$population )

### Check urbact participation

CCDmember_PartURBACT <- URBACT_Membership %>% filter(geonameId %in% CCD_Membership$geonameId) %>% filter(Code_Network != "11")

# Remove participation after 2015 (start CCD)
CCDmember_PartURBACT <- CCDmember_PartURBACT %>% filter(Start< 2016)
  

### Check eucicop participation
EUCICOPparticipation <-readRDS("~/Chap5_TextMining/Data/EUCICOP/Participations_All_Eucicop_Europe.RDS")
  
CCDmember_PartEUCICOP <- EUCICOPparticipation %>% filter(geonameId %in% CCD_Membership$geonameId)

N_EUCICOP <- CCDmember_PartEUCICOP %>% filter(Acronym != "City Centre Doctor") %>% group_by(asciiName)%>% summarise(N= n())
N_EUCICOP_Programme <- CCDmember_PartEUCICOP %>% filter(Acronym != "City Centre Doctor") %>% group_by(asciiName,Programme)%>% summarise(N= n())
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

CCD_partner_Centrality <- CCD_Membership %>% 
  left_join(select(URBACTnet, geonameId, D_urbact = D)) %>%
  left_join(select(EUCICOPnet, geonameId, D_eucicop = D))%>%
  left_join(select(ETMUNnet, geonameId, D_etmun = D)) %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))
# Check summary of degrees

summary(CCD_partner_Centrality$D_urbact)
summary(CCD_partner_Centrality$D_eucicop)
summary(CCD_partner_Centrality$D_etmun)

rm(URBACTnet, ETMUNnet, EUCICOPnet)
#### Sub graphs




library(igraph)
# import graph from chap 4
GraphURBACT <-  readRDS("~/Chap5_TextMining/Data/NetworkData/DualProj_emnw_urbact.rds")
GraphEUCICOP <- readRDS("~/Chap5_TextMining/Data/NetworkData/DualProj_emnw_eucicop.rds")

## Weighted
GraphURBACT <- GraphURBACT$igraph_objects[[4]]

GraphEUCICOP <- GraphEUCICOP$igraph_objects[[4]]

g <- GraphURBACT
# make a list of the names of the nodes of interest
nodes_of_interest <- CCD_Membership$geonameId

# select the nodes having these names
selnodes <- V(g)[name %in% nodes_of_interest]
# get their network neighborhood 
selegoV <- ego(g, order=1, nodes = selnodes, mode = "all", mindist = 0)

# turn the returned list of igraph.vs objects into a graph
selegoG <- induced_subgraph(g,unlist(selegoV))

#Set label
library(sf)
NodeInfo <- GN_DB_City %>% st_drop_geometry() %>% filter(geonameId %in% V(selegoG)$name) %>% select(name=geonameId,asciiName)
selegoG <-  set_vertex_attr(selegoG,"label", index = NodeInfo$name, value = NodeInfo$asciiName)

## Centralities
V(selegoG)$degree <- degree(selegoG)
V(selegoG)$btwn <- betweenness(selegoG, directed = FALSE, normalized = TRUE)

## differenc CCD partner and other



NodeInfo <- NodeInfo  %>% mutate(CCD = case_when(name %in% nodes_of_interest ~ "CCD",
                                                    TRUE ~ "No_CCD"))


selegoG <-  set_vertex_attr(selegoG,"CCD", index = NodeInfo$name, value = NodeInfo$CCD)



### remove edge of value 1

Tdg <- as_tbl_graph(selegoG)

# Add complete node info
Tdg <- Tdg  %>% activate(edges) %>% filter(weight>1) %>% 
  # activate(nodes)%>% filter(!node_is_isolated())

# plot the subgraph


ggraph(Tdg, layout = "kk") + 
  geom_edge_link(alpha = 0.2)+
  geom_node_point( aes(color = CCD, size = btwn) )+ 
  geom_node_text(aes(label = label),repel = TRUE, size = 2.5)+  theme_graph()
  # scale_color_manual(values = pal, breaks = ComVec)+
  # scale_size(range = c(1,12))+
  # labs(color = "Communautés dans le biparti\n(louvain sur em2nw)", size = "Degrés biparti (nb villes membres)",
  #      caption = "Complément du graphe em2nw (villes-villes) :\nchaque lien représente l'absence de lien direct.\nSpatialisation : coordonnées géographiques\nSource : ETMUN 2019/ PG 2020")+



################ ====~ BASIC TEXT MINING ==== ##########
# install.packages("spacyr")
library(spacyr)

# spacy_install()
#spacy_download_langmodel(model = "en_core_web_lg" )

# Choose model
spacy_initialize(model = "en_core_web_lg")
#spacyr::spacy_finalize()


# process documents and obtain a data.table
textReport <- df$txt
names(textReport) <- df$Code_Part
parsedtxt <- spacy_parse(textReport)




# Remove stopwords

CleanParsedText <- unnest_tokens(parsedtxt, word, token) %>%
  dplyr::anti_join(stop_words)



# Size of the corpus

DocSize <- parsedtxt %>% group_by(doc_id)%>% summarise(NWords = as.numeric(n()))%>% arrange(NWords)  %>% as.data.frame() %>%
  left_join(select(CCD_cities, doc_id , NameCity))
ggplot(DocSize) + 
  geom_bar(aes(x= reorder(NameCity,-NWords), y = NWords), stat = "identity") + 
  theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1)) + 
  labs(title = "Nombre de mots par document du corpus du plans d'action locaux du projet CCD", x = "Ville") + 
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 16),
        axis.text.x= element_text(angle=90, vjust = 0.5))

summary(DocSize$NWords)
# stargazer needs a df not a tibble
stargazer(DocSize, median = TRUE, digit.separator = " ")

head(DocSize)
### lexique (without stopwords)

Lexique <- CleanParsedText %>%
  count(word,  sort = T)

### Top words
Top100 <- Lexique %>% top_n(100,n)
wordcloud(words = Top100$word, freq = Top100$n, min.freq = 1,
          max.words=100,  random.order=FALSE, rot.per=0.35)


## pct type of POS
stats <- txt_freq(parsedtxt$pos[!parsedtxt$pos == "PUNCT"])
ggplot(stats) +
  geom_bar(aes(x = reorder(key, -freq_pct), y = freq_pct), stat = "identity") +
  coord_flip() + theme_bw()

stats <- txt_freq(CleanParsedText$pos)
ggplot(stats) +
  geom_bar(aes(x = reorder(key, -freq_pct), y = freq_pct), stat = "identity") +
  coord_flip() + theme_bw()

## nouns
stats <- subset(CleanParsedText, pos %in% c("NOUN")) 
stats <- txt_freq(stats$lemma) 
g1 <- ggplot(stats[1:30, ]) +
  geom_bar(aes(x = reorder(key, freq), y = freq), stat = "identity") + 
  coord_flip() + theme_bw() +  labs(title = "Noms",x = NULL, y = "Occurrences (log10)") + scale_y_log10(n.breaks = 10)
# scale_y_log10()

## adjectives
stats <- subset(CleanParsedText, pos %in% c("ADJ")) 
stats <- txt_freq(stats$lemma)
g2 <- ggplot(stats[1:30, ]) +
  geom_bar(aes(x = reorder(key, freq), y = freq), stat = "identity") +
  coord_flip() + theme_bw() + labs(x = "Adjectifs", y = "Occurrences") + scale_y_continuous(n.breaks = 10)

## Verb
stats <- subset(CleanParsedText, pos  %in% c("VERB")) 
stats <- txt_freq(stats$lemma)
g3 <- ggplot(stats[1:30, ]) +
  geom_bar(aes(x = reorder(key, freq), y = freq), stat = "identity") +
  coord_flip() + theme_bw() + labs(title = "Verbes",x = NULL, y = "Occurrences", caption ="Sources : URBACT site web / P.Gourdon 2020" ) + 
  scale_y_continuous(n.breaks = 10)

# cooccurrence

cooctermsCorpus <- cooccurrence(CleanParsedText$lemma,  skipgram = 3)
# cooctermsCorpus %>% filter(term1 == "create" & cooc > 5)


## Plot
gridVerbNom <- g1/g3

gridVerbNom
ggsave("OUT/CCD_LAP_Top30_Verb_Noun.pdf", width = 8.3, height = 8.3 , units = "in" )


################ Key words

## Using RAKE

stats <- keywords_rake(x = parsedtxt, term = "lemma", group = "doc_id", 
                       relevant = parsedtxt$pos %in% c("NOUN", "ADJ"), ngram_max = 3,n_min = 3)

stats$key <- factor(stats$keyword, levels = rev(stats$keyword))

statsTop <- stats %>% filter(ngram >1)%>% top_n(30, freq)
summary(stats$freq)
summary(stats$rake)
statsTop <- stats %>% filter(ngram >1)%>% filter(freq>10 & rake > 1.2)
ggplot(statsTop) +
  geom_bar(aes(x = reorder(key, freq), y = freq), stat = "identity") +
  # scale_y_log10(n.breaks = 8)+
  scale_y_continuous(n.breaks = 8)+
  coord_flip() + theme_bw() +  labs(x = "Mots-clés identifiés par RAKE", y = "Occurrences dans le corpus", 
                                    caption = "Note : les mots-clés représentés sont issus du triple filtrage suivant : n-gram > 1 & occurences > 10 & Score 'Rake' > 1.2\nSources : URBACT site web / P.Gourdon 2020")

ggsave("OUT/KW_CCD_LAP_RAKE_ngram2_freq10_Rake1.2.pdf", width = 8.3, height = 5.8 , units = "in" )

statsTop <- stats %>% filter(ngram >1) %>% filter(freq > 10)
ggplot(statsTop) + 
  geom_point(aes(x=freq,y = rake), shape = 21, fill = "orange", color = "grey80", alpha= 0.7) + 
  # scale_size(range = c(2:10), breaks = c(min(TopTfIdfFreq$MeanTfIdf): max(TopTfIdfFreq$MeanTfIdf)))+
  scale_x_log10() + 
  geom_text_repel(aes(label = keyword,x=freq,y = rake ), size = 2.5, segment.alpha= 0.5,segment.size = 0.3)+
  labs(x = "Nombre d'occurrences dans le corpus (log10)", y = "Qualité du mot-clé (RAKE)")


## Using Pointwise Mutual Information Collocations
# remove other words like proper nouns, numeric

stats <- keywords_collocation(x = CleanParsedText %>% filter(!pos == "PROPN" & !pos == "NUM" & !pos == "X"), 
                              term = "lemma", group = "sentence_id", ngram_max = 4, n_min = 3)

stats$key <- factor(stats$keyword, levels = rev(stats$keyword))

statsTop <- stats %>% top_n(30, freq)
statsTop <- stats %>% filter(freq > 10 & pmi > 5)
ggplot(statsTop) +
  geom_bar(aes(x = reorder(key, freq), y = freq), stat = "identity") +
  coord_flip() + theme_bw() +  labs(x = "Mots-Clés identifiés avec la PMI Collocation", y = "Nombre d'occurrences dans le corpus",
                                    caption = "Note : les mots-clés représentés sont issus du double filtrage suivant : occurences > 10 & Score 'PMI' > 5\nSources : URBACT site web / P.Gourdon 2020")

ggsave("OUT/KW_CCD_LAP_RAKE_PMI.pdf", width = 8.3, height = 5.8 , units = "in" )
## Tf idf 

Ntermdoc <- CleanParsedText %>% 
  # filter(!pos == "VERB" ) %>% 
  filter(!pos == "PROPN" & !pos == "NUM" & !pos == "X") %>% 
  filter(entity == "")%>%
  count(lemma, doc_id)
TfIdfDoc <- Ntermdoc %>% bind_tf_idf(lemma, doc_id, n)

TopTfIdf <- TfIdfDoc %>% group_by(doc_id)%>% top_n(10, tf_idf)

TfIdMean <- TfIdfDoc %>% group_by(lemma) %>% summarise(MeanTfIdf = mean(tf_idf), Freq = sum(n), MaxTfIdf = max(tf_idf), idf = max(idf))

## mots frequent terms with low idf
Top1 <- TfIdMean %>% filter(idf<1) %>% arrange(desc(Freq))%>% ungroup() %>% top_n(50, Freq)



## mots frequent terms with high idf
Top2 <- TfIdMean %>% filter(idf>1) %>% arrange(desc(Freq))%>% ungroup() %>% top_n(50, Freq)


#plots 

ga <-ggplot(Top1) + 
  geom_point(aes(x=Freq,y = idf, size = MeanTfIdf), shape = 21, fill = "orange", color = "grey80", alpha= 0.7) + 
  # scale_size(range = c(2:10), breaks = c(min(TopTfIdfFreq$MeanTfIdf): max(TopTfIdfFreq$MeanTfIdf)))+
  scale_y_log10()+ 
  scale_x_log10() + 
  scale_size(range = c(1, 6), limits = c(min(Top1$MeanTfIdf), max(Top2$MeanTfIdf))) +
  geom_text_repel(aes(label = lemma,x=Freq,y = idf ), size = 2.5, segment.alpha= 0.5,segment.size = 0.3)+
  labs(x = "Nombre d'occurrences dans le corpus (log10)", y = "IDF (log10)", size = "Moyenne du Tf-Idf", 
       title = "A", subtitle = "Mots les plus fréquents communs à la majorité des documents")+
  theme(legend.position="none")

options(scipen= 999)         
gb <- ggplot(Top2) + 
  geom_point(aes(x=Freq,y = idf, size = MeanTfIdf), shape = 21, fill = "orange", color = "grey80", alpha= 0.7) + 
  # scale_size(range = c(2:10), breaks = c(min(TopTfIdfFreq$MeanTfIdf): max(TopTfIdfFreq$MeanTfIdf)))+
  scale_y_log10()+ 
  scale_x_log10() + 
  scale_size(range = c(1, 6), 
             limits = c(min(Top1$MeanTfIdf), max(Top2$MeanTfIdf)), 
             breaks = round(c(min(Top1$MeanTfIdf),0.0001, 0.0005,0.001, 0.005, 0.01, 0.020, max(Top2$MeanTfIdf)), digits = 5)) +
  geom_text_repel(aes(label = lemma,x=Freq,y = idf ), size = 2.5, segment.alpha= 0.5, segment.size = 0.3)+
  labs(x = "Nombre d'occurrences dans le corpus (log10)", y = "IDF (log10)", size = "Moyenne du Tf-Idf", 
       caption = "Source : URBACT site web / P. Gourdon 2020",title = "B", subtitle = "Mots les plus fréquents spécifiques à des sous-ensembles de documents")+
  theme(legend.position="bottom", legend.direction = "horizontal")+
  guides(size=guide_legend(nrow=1, label.position = "bottom"))

grid2 <- ga/gb
grid2

# ggsave("OUT/URBACTreport_topterms_Idf_N.pdf", width = 8.3, height = 8.3 , units = "in" )


### Variable
#### tf idf by category



CleanParsedText_var <- CleanParsedText %>% 
  left_join(select(CCD_cities, NameCity,Country, Region, asciiName, geonameId, doc_id))



Politics <- Politics %>% mutate(iso_a2 = recode(iso_a2,  "GB" = "UK"))
colnames(Politics)
CleanParsedText_var <- CleanParsedText_var %>% left_join(select(Politics, Country = iso_a2,
                                                        LocGovType_HorizontalPwrRelation, 
                                                        LocGovType_VerticalPwrRelation,
                                                        LocGovType_PoliticalLeadership,
                                                        LocGovType_MunicipalAdmin))

skim(CleanParsedText_var$LocGovType_MunicipalAdmin)

## Remove web site 

CleanParsedText_var <- CleanParsedText_var %>% filter(!str_detect(lemma,pattern = "http"))
### By city
cat <- "NameCity"
Cat_words <- CleanParsedText_var %>% 
  filter(!pos == "PROPN" & !pos == "NUM" & !pos == "X") %>% 
  filter(entity == "") %>%
  count(!!as.name(cat), lemma, sort = TRUE)


plot_cat <- Cat_words %>%
  bind_tf_idf(lemma,!!as.name(cat), n) %>%
  mutate(lemma = fct_reorder(lemma, tf_idf)) 

# n_cat <- URBACT_project_text %>% group_by(!!as.name(cat)) %>% count()

#

plot_cat %>% 
  group_by(!!as.name(cat)) %>% 
  top_n(8, tf_idf) %>% 
  ungroup() %>%
  ggplot(aes(reorder_within(lemma, tf_idf,!!as.name(cat) ), tf_idf, fill = !!as.name(cat))) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf", caption = "Source : URBACT site web / P. Gourdon 2020") +
  scale_x_reordered()+
  facet_wrap(as.formula(paste("~", cat)), ncol = 2, scales = "free") +
  coord_flip()+ 
  geom_label(aes(label = n, x= reorder_within(lemma, tf_idf,!!as.name(cat) ), y = tf_idf), position = position_stack(0.5), color = "black", size = 2)+
  theme(legend.position = "none")

ggsave("OUT/CCD_LAP_Cities_Idf_N.pdf", width = 8.3, height = 8.3 , units = "in" )
## Political typo

cat <- "LocGovType_MunicipalAdmin"
Cat_words <- CleanParsedText_var %>% 
  filter(!pos == "PROPN" & !pos == "NUM" & !pos == "X") %>% 
  filter(entity == "") %>%
  count(!!as.name(cat), lemma, sort = TRUE) 

plot_cat <- Cat_words %>%
  bind_tf_idf(lemma,!!as.name(cat), n) %>%
  mutate(lemma = fct_reorder(lemma, tf_idf)) 

n_cat <- CCD_cities %>% left_join(select(Politics, Country = iso_a2,
                                                  LocGovType_HorizontalPwrRelation, 
                                                  LocGovType_VerticalPwrRelation,
                                                  LocGovType_PoliticalLeadership,
                                                  LocGovType_MunicipalAdmin)) %>%  
  group_by(!!as.name(cat)) %>% count()

plot_cat %>% 
  group_by(!!as.name(cat)) %>% 
  top_n(10, tf_idf) %>% 
  ungroup() %>%
  ggplot(aes(reorder_within(lemma, tf_idf, !!as.name(cat)), tf_idf, fill = !!as.name(cat)), 
         show.legend = FALSE) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  scale_x_reordered() +
  facet_wrap(as.formula(paste("~", cat)), ncol = 2, scales = "free") +
  coord_flip() + 
  geom_label(aes(label = n, x= reorder_within(lemma, -tf_idf, !!as.name(cat)), y = tf_idf), 
             position = position_stack(0.5), color = "black", size = 2) +
  geom_text( data    = n_cat,
             mapping = aes(x = -Inf, y = Inf, label = paste("Nb docs = ", n, sep = "")),
             vjust = -1, hjust = 1.2, size = 2.5)+
  theme(legend.position = "none")



# KW by caterory
cat <- "NameCity"

## Using Pointwise Mutual Information Collocations
# remove stop words

cat_KW <- CleanParsedText_var %>% 
  filter(!pos == "PROPN" & !pos == "NUM" & !pos == "X") %>% 
  filter(entity == "") %>%
  group_by(!!as.name(cat)) %>%
  do(keywords_collocation(x = ., term = "lemma", group = "sentence_id", ngram_max = 4, n_min = 3) )

statsTop <- cat_KW %>% group_by(!!as.name(cat)) %>% top_n(5, freq)
statsTop <- cat_KW %>% group_by(!!as.name(cat)) %>% filter(freq > 4 & pmi > 5) %>% top_n(5, freq)

ggplot(statsTop) +
  geom_bar(aes(x = reorder_within(keyword, freq, !!as.name(cat)), y = freq), stat = "identity") +
  coord_flip() + theme_bw() +  labs(x = "Keywords identified by PMI Collocation", y = "Fréquence") + scale_x_reordered() +
  facet_wrap(as.formula(paste("~", cat)),scales = "free")


###### ==== META DATA fot CORTEXT MANAGER =====


### Metadata for Cortex manager
CCD_cities <- CCD_cities %>% mutate(geonameId = as.character(geonameId))
## add politics info

Politics <- Politics %>% mutate(iso_a2 = recode(iso_a2,  "GB" = "UK"))
colnames(Politics)
CCD_cities <- CCD_cities %>% left_join(select(Politics, Country = iso_a2,
                                                                    LocGovType_HorizontalPwrRelation, 
                                                                    LocGovType_VerticalPwrRelation,
                                                                    LocGovType_PoliticalLeadership,
                                                                    LocGovType_MunicipalAdmin))

##  info on city 

GN_info <- GN_DB_City %>% select(geonameId, adminLevel, countryCode, population, CODE_LAU, 
                                 NAME_LAU, PopAdmin06,PopAdmin11, ID_UMZ, NAME_UMZ, POPUMZ11,  ID_FUA, NAME_FUA, POPFUA06)

CCD_cities <- CCD_cities %>% left_join(GN_info)

skim(CCD_cities)

# deal with NA when cities are outside UMZ (built-up urban area ) or FUA (Functional Urban areas)
colnames(URBACT_Project_Report)
CCD_cities <- CCD_cities %>% 
  mutate_at(vars(c("ID_UMZ", "NAME_UMZ", "ID_FUA", "NAME_FUA" )), ~replace_na(., "Outside"))

CCD_cities <- CCD_cities %>% mutate(WithinUMZ = case_when( ID_UMZ == "Outside" ~ "Outside",
                                                           TRUE ~ "Within"),
                                    WithinFUA = case_when( ID_FUA == "Outside" ~ "Outside",
                                                           TRUE ~ "Within"))


## Select variables

colnames(CCD_cities)
CCD_citiesExport <- CCD_cities %>% select(doc_id, NameCity,Region, geonameId, asciiName, starts_with("LocGovType"), adminLevel, 
                                          population, NAME_LAU, NAME_UMZ, NAME_FUA, PopAdmin06, PopAdmin11, WithinUMZ, WithinFUA)
## add filename

CCD_citiesExport$filename <- paste0("LAP_CCD_", CCD_citiesExport$doc_id)


CCD_citiesExport <- CCD_citiesExport %>% relocate(filename)

# Save file in csv (use tab separator for Cortext Manager)

write.table(CCD_citiesExport, "DataProd/MetadataCortext_CCD_LAP_Cities.csv", row.names = F, 
            fileEncoding = "UTF-8", sep="\t", qmethod = "double")



######DTM
## Note keep the doc id and the vector variable in the SAME order, because specific_terms(dtm2, SizeVec) does not perform a merge

data <- CleanParsedText %>%  filter(!pos == "PROPN" & !pos == "NUM" & !pos == "X") %>% 
  filter(entity == "") %>% 
  arrange(doc_id)


x4 <- document_term_frequencies(data[,c("doc_id","lemma")])

x5 <- document_term_frequencies_statistics(x4)

### DTM
dtm <- document_term_matrix(x4)




# # some words in context
# dtmTemis <- build_dtm(corpus)
# 
# 
# concordances(corpus, dtmTemis, c("create", "citizen") )
# concordances(corpus, dtmTemis, "created")
# concordances(corpus, dtmTemis, "immigrants")
# 



################## Text similarities
df <- df %>% arrange(doc_id)
library(LSAfun)
tvectors <- lsa(dtm)
tvec <- as.matrix(tvectors$dk)
docsimilarities <- multidocs(df$txt, tvectors = tvec, breakdown = TRUE)

MatrixSimiText <- docsimilarities$cosmat
rownames(MatrixSimiText) <- df$Code_Part
colnames(MatrixSimiText) <- df$Code_Part

diag(MatrixSimiText) <- NA
PairSimilText <- MatrixSimiText %>% as.data.frame()

PairSimilText <- PairSimilText %>% mutate(doc1 = rownames(.)) %>% 
  pivot_longer(-doc1, names_to = "doc2", values_to = "CosineSimilarity") %>%
  filter(!is.na(CosineSimilarity))

PairSimilText <- PairSimilText %>%
  mutate(Pair = paste(pmin(doc1,doc2), pmax(doc1,doc2), sep = "_")) %>% filter(!duplicated(Pair))

PairSimilText <- as.data.frame(PairSimilText)
stargazer(PairSimilText, median = TRUE, )
summary(PairSimilText$CosineSimilarity)
# geographical distance
library(sf)

sf_CCD <- st_as_sf(CCD_cities, coords = c("lng_GN", "lat_GN"), crs = 4326) %>% st_transform(crs = 3035)
geoDist <- st_distance(sf_CCD, by_element = FALSE) 

geoDist <- as.data.frame(unclass(geoDist))
colnames(geoDist) <- rownames(geoDist)

PairGeoDist <- geoDist %>% mutate(city1 = rownames(.)) %>% 
  pivot_longer(-city1, names_to = "city2", values_to = "GeoDist") 

sf_CCDinfo <- sf_CCD %>% mutate(ID = rownames(.)) %>% st_drop_geometry()

PairGeoDist <- PairGeoDist %>% left_join(select(sf_CCDinfo, City1GN = geonameId, city1 = ID)) %>% 
  left_join(select(sf_CCDinfo, City2GN = geonameId, city2 = ID))


PairGeoDist <- PairGeoDist  %>% left_join(select(CCD_cities, City1GN = geonameId, doc1 = doc_id, Region1 = Region)) %>% 
  left_join(select(CCD_cities, City2GN = geonameId, doc2 = doc_id, Region2 = Region))

PairGeoDist <- PairGeoDist %>% mutate(Pair = paste(pmin(doc1,doc2), pmax(doc1,doc2), sep = "_"))

PairSimilText <- PairSimilText %>%left_join(select(PairGeoDist, Pair, GeoDist, Region1, Region2)) %>% 
  filter(!duplicated(Pair))


RegionSimil <- PairSimilText %>% group_by(Region1, Region2) %>% summarise(mean = mean(CosineSimilarity), med = median(CosineSimilarity))
ggplot(PairSimilText) + geom_point(aes(x = GeoDist, y = CosineSimilarity )) 



