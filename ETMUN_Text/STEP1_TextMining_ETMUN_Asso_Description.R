###############################  ETMUN decription  ##################################
#                               
#                          
# DESCRIPTION : Analyse textuelle des raisons sociales des associations ETMUN
#
# 
############################################################################## PG novembre 2020

setwd("~/Chap5_TextMining/ETMUN_Text/")

# Packages

library(tidyverse)
library(tidytext)
library(sf)
library(skimr)
library(GGally)
library(stargazer)
library(patchwork)
library(forcats)
library(wordcloud)
library(udpipe)
library(ggrepel)
library(topicmodels)
library(R.temis)
library(igraph)
library(ggraph)
library(tidygraph)
### Data

# BD ETMUN Asso
ETMUN_ASSO <- readRDS("~/Chap5_TextMining/Data/ETMUN/BD_ETMUN_ORGANISATION.rds")
ETMUN_ASSO <- ETMUN_ASSO %>% mutate_if(is.factor, ~as.character(.))

## add date when missing
# Eneergy Cities
ETMUN_ASSO[ETMUN_ASSO$Code == "04477","Date"] <- 1990
# Nordic cities
ETMUN_ASSO[ETMUN_ASSO$Code == "W6","Date"] <- 2006

# Web corpus : presentation text

ETMUN_webCorpus <- read.csv2("~/Chap5_TextMining/Data/ETMUN/ETMUN_TextWeb_Corpus.csv", stringsAsFactors = F)

# Geom data
## rec

rec <- st_read("~/Chap5_TextMining/Data/Geometry/rec_3035.geojson")

#EU 
sfEU <- st_read("~/Chap5_TextMining/Data/Geometry/fondEuropeLarge.geojson", stringsAsFactors = FALSE,crs = 3035)




############# ====  TEXT MINING ===========###############

ETMUN_ASSO <- ETMUN_ASSO %>% left_join(select(ETMUN_webCorpus, Code, textweb_presentation, textweb_objective_missions), by = "Code")
ETMUN_Text <- ETMUN_ASSO %>% mutate(text = paste(Aims, Activities, textweb_presentation, textweb_objective_missions, sep = ". "))

###############""" SpacyR and tidy approach
# install.packages("spacyr")
library(spacyr)

# spacy_install()
#spacy_download_langmodel(model = "en_core_web_lg" )

# Choose model
spacy_initialize(model = "en_core_web_lg")
#spacyr::spacy_finalize()


# process documents and obtain a data.table
textAsso<- ETMUN_Text$text
names(textAsso) <- ETMUN_Text$Code
parsedtxt <- spacy_parse(textAsso)




# Remove stopwords

CleanParsedText<- unnest_tokens(parsedtxt, word, token) %>%
  dplyr::anti_join(stop_words)

################ ====~ BASIC TEXT MINING ==== ##########

# Size of the corpus

DocSize <- parsedtxt %>% group_by(doc_id)%>% summarise(NWords = as.numeric(n()))%>% arrange(NWords)  %>% as.data.frame()
ggplot(DocSize) + 
  geom_bar(aes(x= reorder(doc_id,-NWords), y = NWords), stat = "identity") + 
  theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1)) + 
  labs(title = "Nombre de mots par document du corpus de description des associations ETMUN", x = "Document ID") + 
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 16),
        axis.text.x= element_text(angle=90, vjust = 0.5))

summary(DocSize$NWords)
# stargazer needs a df not a tibble
stargazer(DocSize, median = TRUE, digit.separator = " ")


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

stats
stats <- txt_freq(CleanParsedText$pos)
ggplot(stats) +
  geom_bar(aes(x = reorder(key, -freq_pct), y = freq_pct), stat = "identity") +
  coord_flip() + theme_bw()

## nouns
stats <- subset(CleanParsedText, pos %in% c("NOUN")) 
stats <- txt_freq(stats$lemma) 
g1 <- ggplot(stats[1:30, ]) +
  geom_bar(aes(x = reorder(key, freq), y = freq), stat = "identity") +
  coord_flip() + theme_bw() +  labs(title = "Noms",x = NULL, y = "Occurrences (log 10)") + scale_y_log10(n.breaks = 10)
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
  coord_flip() + theme_bw() + labs(title = "Verbes",x = NULL, y = "Occurrences (log 10)", caption ="Sources : ETMUN 2019 / P.Gourdon 2020" ) + 
  scale_y_log10(n.breaks = 10)

# Proper noun
stats <- subset(CleanParsedText, pos  %in% c("PROPN")) 
stats <- txt_freq(stats$word)
g4 <- ggplot(stats[1:30, ]) +
  geom_bar(aes(x = reorder(key, freq), y = freq), stat = "identity") +
  coord_flip() + theme_bw() + labs(title = "Noms Propres",x = NULL, y = "Occurrences", caption ="Sources : ETMUN 2019 / P.Gourdon 2020" ) + 
  scale_y_continuous(n.breaks = 10)


# Loc
stats <- subset(CleanParsedText, entity  %in% c("LOC_B")) 
stats <- txt_freq(stats$word)
g5 <- ggplot(stats[1:9, ]) +
  geom_bar(aes(x = reorder(key, freq), y = freq), stat = "identity") + 
  coord_flip() + theme_bw() + labs(title = "Lieux",x = NULL, y = "Occurrences (log10)", caption ="Sources : ETMUN 2019 / P.Gourdon 2020" ) + 
  scale_y_log10(n.breaks = 10)




# cooccurrence

cooctermsCorpus <- cooccurrence(CleanParsedText$lemma,relevant = CleanParsedText$pos %in% c("ADJ", "NOUN") , skipgram = 3, group = "sentence_id")
cooctermsCorpus %>% filter(term1 == "level" & cooc > 1)


## Plot
gridVerbNom <- g1/g3

gridVerbNom
ggsave("OUT/ETMUNdescription_Top30_Verb_Noun.pdf", width = 8.3, height = 8.3 , units = "in" )


################ Key words

## Using RAKE

stats <- keywords_rake(x = parsedtxt, term = "lemma", group = "doc_id", 
                       relevant = parsedtxt$pos %in% c("NOUN", "ADJ"), ngram_max = 3,n_min = 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))

statsTop <- stats %>% filter(ngram >1)%>% top_n(20, freq)
summary(stats$freq)
summary(stats$rake)
# statsTop <- stats %>% filter(ngram >1)%>% filter(freq>10 & rake > 1.8)

ggplot(statsTop) +
  geom_bar(aes(x = reorder(key, freq), y = freq), stat = "identity") +
  # scale_y_log10(n.breaks = 8)+
  scale_y_continuous(n.breaks = 8)+
  coord_flip() + theme_bw() +  labs(x = "Mots-clés identifiés par RAKE", y = "Occurrences dans le corpus", 
                                    caption = "ETMUN2019 / P.Gourdon 2020")

ggsave("OUT/KW_ETMUN_Asso_RAKE.pdf", width = 8.3, height = 5.8 , units = "in" )

ggplot(statsTop) + 
  geom_point(aes(x=freq,y = rake), shape = 21, fill = "orange", color = "grey80", alpha= 0.7) + 
  # scale_size(range = c(2:10), breaks = c(min(TopTfIdfFreq$MeanTfIdf): max(TopTfIdfFreq$MeanTfIdf)))+
  scale_y_log10()+ 
  scale_x_log10() + 
  geom_text_repel(aes(label = keyword,x=freq,y = rake ), size = 2.5, segment.alpha= 0.5,segment.size = 0.3)+
  labs(x = "Nombre d'occurrences dans le corpus (log10)", y = "Qualité du mot-clé (RAKE)")


## Using Pointwise Mutual Information Collocations
# remove other words like proper nouns, numeric

stats <- keywords_collocation(x = CleanParsedText %>% filter(!pos == "PROPN" & !pos == "NUM" & !pos == "X"), 
                              term = "lemma", group = "sentence_id", ngram_max = 4, n_min = 3)

stats$key <- factor(stats$keyword, levels = rev(stats$keyword))

# statsTop <- stats %>% top_n(30, freq)
statsTop <- stats %>% filter(freq > 8 & pmi > 3.5)
ggplot(statsTop) +
  geom_bar(aes(x = reorder(key, freq), y = freq), stat = "identity") +
  coord_flip() + theme_bw() +  labs(x = "Mots-Clés identifiés avec la PMI Collocation", y = "Nombre d'occurrences dans le corpus",
                                    caption = "ETMUN2019 / P.Gourdon 2020")

ggsave("OUT/KW_ETMUN_Asso_PMI_Top.pdf", width = 8.3, height = 5.8 , units = "in" )
## Tf idf 

Ntermdoc <- CleanParsedText %>% 
  filter(!pos == "VERB" ) %>% 
  filter(!pos == "PROPN" & !pos == "NUM" & !pos == "X") %>% 
  filter(entity == "")%>%
  count(lemma, doc_id)
colnames(ETMUN_Text)
Ntermdoc <- CleanParsedText %>% filter(pos == "NOUN" | pos == "ADJ") %>% 
  # left_join(select(ETMUN_Text, doc_id = Code,Country..secretariat.))%>%
  count(lemma, doc_id)
TfIdfDoc <- Ntermdoc %>% bind_tf_idf(lemma,  doc_id, n)

TopTfIdf <- TfIdfDoc %>% group_by(doc_id)%>% top_n(10, tf_idf)

TfIdMean <- TfIdfDoc %>% group_by(lemma) %>% summarise(MeanTfIdf = mean(tf_idf), Freq = sum(n), MaxTfIdf = max(tf_idf), idf = max(idf))

# TfIdfDoc %>% 
#   group_by(Country..secretariat.) %>% 
#   top_n(5, tf_idf) %>% 
#   ungroup() %>%
#   ggplot(aes(reorder_within(lemma, tf_idf,Country..secretariat.), tf_idf, fill = Country..secretariat.)) +
#   geom_col(show.legend = FALSE) +
#   labs(x = NULL, y = "tf-idf", caption = "Source : ETMUN 2019 / P. Gourdon 2020") +
#   scale_x_reordered()+
#   facet_wrap(~Country..secretariat., ncol = 2, scales = "free") +
#   coord_flip()+ 
#   geom_label(aes(label = n, x= reorder_within(lemma, tf_idf,Country..secretariat.), y = tf_idf), position = position_stack(0.5), color = "black", size = 2)+
#   geom_text( data    = n_cat,
#              mapping = aes(x = -Inf, y = Inf, label = paste("Nb docs = ", n, sep = "")),
#              vjust = -1, hjust = 1.2, size = 2.5)+
#   theme(legend.position = "none")

## mots frequent terms with low idf
Top1 <- TfIdMean %>% 
        filter(idf<2) %>% 
          arrange(desc(Freq))%>% ungroup() %>% 
        top_n(50, Freq)

ga <-ggplot(Top1) + 
  geom_point(aes(x=Freq,y = idf, size = MeanTfIdf), shape = 21, fill = "orange", color = "grey80", alpha= 0.7) + 
  # scale_size(range = c(2:10), breaks = c(min(TopTfIdfFreq$MeanTfIdf): max(TopTfIdfFreq$MeanTfIdf)))+
  # scale_y_log10()+ 
  scale_x_log10(n.breaks = 8) + 
  geom_text_repel(aes(label = lemma,x=Freq,y = idf ), size = 2.5, segment.alpha= 0.5,segment.size = 0.3)+
  labs(x = "Nombre d'occurrences dans le corpus (log10)", y = "IDF", size = "Moyenne du Tf-Idf", 
       title = "A")+
  theme(legend.position="bottom")

## mots frequent terms with high idf
Top2 <- TfIdMean %>% filter(idf >2 & Freq>5) %>% arrange(desc(Freq))%>% ungroup() %>% top_n(100, Freq)
Top2 <- Top2 %>% filter(idf >2.5 | Freq > 15 )
# Top2 <- TfIdMean %>% filter(!lemma %in% Top1$lemma) %>% top_n(100, Freq)

gb <- ggplot(Top2) + 
  geom_point(aes(x=Freq,y = idf, size = MeanTfIdf), shape = 21, fill = "orange", color = "grey80", alpha= 0.7) + 
  # scale_size(range = c(2:10), breaks = c(min(TopTfIdfFreq$MeanTfIdf): max(TopTfIdfFreq$MeanTfIdf)))+
   # scale_y_log10()+ 
  scale_x_log10() + 
  geom_text_repel(aes(label = lemma,x=Freq,y = idf ), size = 2.5, segment.alpha= 0.5, segment.size = 0.3)+
  labs(x = "Nombre d'occurrences dans le corpus (log10)", y = "IDF", size = "Moyenne du Tf-Idf", 
       caption = "Source : URBACT site web / P. Gourdon 2020",title = "B")+
  theme(legend.position="bottom")

grid2 <- ga/gb
grid2

ggsave("OUT/ETMUN_Asso_topterms_Idf_N.pdf", width = 8.3, height = 8.3 , units = "in" )

### Keywords detection to get institution and localities 

## Using RAKE

stats <- keywords_rake(x = parsedtxt, term = "lemma", group = "doc_id", 
                       relevant = parsedtxt$pos %in% c("PROPN"), ngram_max = 5,n_min = 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))

statsTop <- stats %>% filter(ngram >1)%>% top_n(20, freq)
summary(stats$freq)
summary(stats$rake)
# statsTop <- stats %>% filter(ngram >1)%>% filter(freq>10 & rake > 1.8)

ggplot(statsTop) +
  geom_bar(aes(x = reorder(key, freq), y = freq), stat = "identity") +
  # scale_y_log10(n.breaks = 8)+
  scale_y_continuous(n.breaks = 8)+
  coord_flip() + theme_bw() +  labs(x = "Mots-clés identifiés par RAKE", y = "Occurrences dans le corpus", 
                                    caption = "ETMUN2019 / P.Gourdon 2020")

ggsave("OUT/KW_ETMUN_Asso_RAKE.pdf", width = 8.3, height = 5.8 , units = "in" )



## Using Pointwise Mutual Information Collocations
# create a dico of entities
dicoPOSEntities <- CleanParsedText %>% select(word,pos, entity) %>% distinct()

# keywords on all text
stats <- keywords_collocation(x = CleanParsedText , 
                              term = "word", group = "sentence_id", ngram_max = 4, n_min = 3)

# filter keywords by pos and entities
stats <- stats %>% left_join(select(dicoPOSEntities, left = word, pos_left = pos, entity_left = entity)) %>%
                  left_join(select(dicoPOSEntities, right = word, pos_right = pos, entity_right = entity)) 

statsPROPN <- stats %>% filter_at(vars(11:14), all_vars(. == "PROPN" | !. == "")) 

statsPROPN <- statsPROPN %>% filter(!duplicated(keyword))

statsPROPN$key <- factor(statsPROPN$keyword, levels = rev(statsPROPN$keyword))

# statsTop <- stats %>% top_n(30, freq)
statsTop <- statsPROPN %>% filter(freq > 5 & pmi > 6)
ggplot(statsTop) +
  geom_bar(aes(x = reorder(key, freq), y = freq), stat = "identity") +
  coord_flip() + theme_bw() +  labs(x = "Mots-Clés identifiés avec la PMI Collocation", y = "Nombre d'occurrences dans le corpus",
                                    caption = "ETMUN2019 / P.Gourdon 2020")

ggsave("OUT/KW_ETMUN_Asso_PMI_Top.pdf", width = 8.3, height = 5.8 , units = "in" )


########## TF-Idf by date


# add period of creation


ETMUN_Text <- ETMUN_Text %>% mutate(CreationPeriod = case_when(Date <1990 ~ "Before1990",
                                                               Date >= 1990 & Date <1995 ~ "1990-1995",
                                                               Date >= 1995 & Date <2000 ~ "1995-2000",
                                                               Date >= 2000 & Date <2006 ~ "2000-2006",
                                                               Date >= 2006 ~ "After 2006"
                                                               ))

CleanParsedText <- CleanParsedText %>% left_join(select(ETMUN_Text, doc_id = Code, CreationPeriod))
# tf-idf by  period of creation
cat <- "CreationPeriod"
Cat_words <- CleanParsedText %>% 
  filter(!pos == "PROPN" & !pos == "NUM" & !pos == "X") %>% 
  filter(entity == "") %>%
  count(!!as.name(cat), lemma, sort = TRUE)


plot_cat <- Cat_words %>%
  bind_tf_idf(lemma,!!as.name(cat), n) %>%
  mutate(lemma = fct_reorder(lemma, tf_idf)) 

n_cat <- ETMUN_Text %>% group_by(!!as.name(cat)) %>% count()


#

plot_cat %>% 
  group_by(!!as.name(cat)) %>% 
  top_n(10, tf_idf) %>% 
  ungroup() %>%
  ggplot(aes(reorder_within(lemma, tf_idf,!!as.name(cat) ), tf_idf, fill = !!as.name(cat))) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf", caption = "Source : ETMUN 2019 / P. Gourdon 2020") +
  scale_x_reordered()+
  facet_wrap(as.formula(paste("~", cat)), ncol = 2, scales = "free") +
  coord_flip()+ 
  geom_label(aes(label = n, x= reorder_within(lemma, tf_idf,!!as.name(cat) ), y = tf_idf), position = position_stack(0.5), color = "black", size = 2)+
  geom_text( data    = n_cat,
             mapping = aes(x = -Inf, y = Inf, label = paste("Nb docs = ", n, sep = "")),
             vjust = -1, hjust = 1.2, size = 2.5)+
  theme(legend.position = "none")

ggsave("OUT/ETMUN_AssoPeriod_Idf_N.pdf", width = 8.3, height = 8.3 , units = "in" )




######DTM
## Note keep the doc id and the vector variable in the SAME order, because specific_terms(dtm2, SizeVec) does not perform a merge

data <- CleanParsedText %>%  filter(!pos == "PROPN" & !pos == "NUM" & !pos == "X") %>% 
  filter(entity == "") %>% 
  arrange(doc_id)


x4 <- document_term_frequencies(data[,c("doc_id","lemma")])

x5 <- document_term_frequencies_statistics(x4)

### DTM
dtm <- document_term_matrix(x4)


################## Text similarities
ETMUN_Text <- ETMUN_Text %>% arrange(Code)
library(LSAfun)
tvectors <- lsa(dtm)
tvec <- as.matrix(tvectors$dk)
docsimilarities <- multidocs(ETMUN_Text$text, tvectors = tvec, breakdown = TRUE)

MatrixSimiText <- docsimilarities$cosmat
rownames(MatrixSimiText) <- ETMUN_Text$Code
colnames(MatrixSimiText) <- ETMUN_Text$Code

diag(MatrixSimiText) <- NA
PairSimilText <- MatrixSimiText %>% as.data.frame()

PairSimilText <- PairSimilText %>% mutate(doc1 = rownames(.)) %>% 
  pivot_longer(-doc1, names_to = "doc2", values_to = "CosineSimilarity") %>%
  filter(!is.na(CosineSimilarity))

PairSimilText <- PairSimilText %>% 
  mutate(Pair = paste(pmin(doc1,doc2), pmax(doc1,doc2), sep = "_")) %>% filter(!duplicated(Pair))

PairSimilText <- as.data.frame(PairSimilText)
stargazer(PairSimilText, median = TRUE)
summary(PairSimilText$CosineSimilarity)


##### Networks

##### BY TF-IDF overlapping
# library(devtools)
# install_github("cbail/textnets")
library(textnets)

ETMUN_Text[ETMUN_Text$Code == "04433","Acronym"] <- "AMPHICTYONY"


# AssoGroup_nouns <- PrepText(ETMUN_Text, groupvar = "Acronym", textvar = "text", 
#                        node_type = "groups", tokenizer = "words", 
#                        pos = "nouns", remove_stop_words = TRUE, compound_nouns = TRUE)

# Try to do the asso group nouns manually to keep what we want in pos
AssoGroup_nouns <- CleanParsedText %>% filter(pos == "NOUN" | pos == "ADJ") %>% 
                    filter(entity == "") %>% 
                    group_by(doc_id,lemma) %>% 
                    summarise(count = n())
AssoGroup_nouns <- AssoGroup_nouns %>% left_join(select(ETMUN_Text, doc_id = Code, Acronym))
AssoGroup_nouns <- AssoGroup_nouns %>% ungroup %>% select(-doc_id)%>% select(Acronym, lemma, count)

# remove some association
AssoGroup_nouns <- AssoGroup_nouns %>% filter(!Acronym == "WWCAM")


AssoWords_nouns <- PrepText(ETMUN_Text, groupvar = "Acronym", textvar = "text", 
                            node_type = "words", tokenizer = "words", 
                            pos = "nouns", remove_stop_words = TRUE, compound_nouns = TRUE)

# Filter the compound nouns that appear at least 3 times in a document

AssoWords_nounsFiltered <- AssoWords_nouns %>% group_by(lemma) %>% mutate(MaxN = max(count)) %>% filter(MaxN>2) %>% select(-MaxN)
# AssoGroup_nounsFiltered <- AssoGroup_nouns %>% group_by(lemma) %>% mutate(MaxN = max(count)) %>% filter(MaxN>2) %>% select(-MaxN)
## filter asso group net remove weak tf-idf 
filterAssoGroupNouns <- AssoGroup_nouns %>%
  bind_tf_idf(lemma,Acronym, count) 
filterAssoGroupNouns <- filterAssoGroupNouns %>% 
                        group_by(Acronym) %>% 
                        top_n(10, tf_idf)
filterAssoGroupNouns <- filterAssoGroupNouns %>% select(Acronym, lemma, count) %>% ungroup()
n<- filterAssoGroupNouns %>% group_by(Acronym) %>% summarise(n = n())
summary(n$n)
AssoGroup_Net <- CreateTextnet(AssoGroup_nouns)
AssoGroup_Net <- CreateTextnet(filterAssoGroupNouns)

# AssoGroup_Net <- CreateTextnet(AssoGroup_nounsFiltered)

AssoWords_Net <- CreateTextnet(AssoWords_nounsFiltered)

# VisTextNet(AssoGroup_Net, label_degree_cut = 0)

## Compare tf-idf overlapping with overlapping city members


AssoGroup_edgesDF <- as_data_frame(AssoGroup_Net, what = c("edges"))
AssoGroup_edgesDF <- AssoGroup_edgesDF %>% mutate(Pair = paste(pmin(from,to), pmax(from,to), sep = "_"))

DualProj_emnw <- readRDS("~/Chap4_NetworkAnalysis/Chap4_NetworkAnalysis/4.2.InterOrgaNet/ETMUN_inter/DataProd/DualProj_emnw.rds")

AssoNet_MemberCIties <- DualProj_emnw$igraph_objects[[2]]

AssoNet_edges <- as_data_frame(AssoNet_MemberCIties, what = c("edges"))
AssoNet_edges <- AssoNet_edges %>% left_join(select(ETMUN_Text, from = Code, AcronymFrom = Acronym)) %>% 
  left_join(select(ETMUN_Text, to = Code, Acronymto = Acronym)) %>% mutate(from = AcronymFrom, to = Acronymto) %>% 
  select(-AcronymFrom,- Acronymto) %>% mutate(Pair = paste(pmin(from,to), pmax(from,to), sep = "_"))

dfcompare <- AssoGroup_edgesDF %>% select(Pair,Semantic = weight) %>% left_join(select(AssoNet_edges, Pair, NmemberCities = nWeight))

dfcompare <- dfcompare %>% filter(!is.na(NmemberCities))
ggplot(dfcompare) + geom_point(aes(x = NmemberCities, y = Semantic))

# filter outlier
dfcompare2 <- dfcompare %>% filter(Semantic<0.05)
ggplot(dfcompare) + geom_point(aes(x = NmemberCities, y = Semantic)) 


### better vizu for asso networks (need to filter)


V(AssoGroup_Net)$wdegree <- strength(AssoGroup_Net, mode = "all")
V(AssoGroup_Net)$degree <- degree(AssoGroup_Net, mode = "all")
louvainAll <- cluster_walktrap(AssoGroup_Net, weights = E(AssoGroup_Net)$weight, modularity = T, steps = 5)
louvainAll <- cluster_louvain(AssoGroup_Net, weights = E(AssoGroup_Net)$weight)
louvainAll$membership
louvainAll$modularity
V(AssoGroup_Net)$louvainAll <- louvainAll$membership
V(AssoGroup_Net)$btwnAll <- betweenness(AssoGroup_Net, weights = E(AssoGroup_Net)$weight, normalized = TRUE)
graph.density(AssoGroup_Net)
AssoGroup_Net_tidy <- as_tbl_graph(AssoGroup_Net)


summary(V(AssoGroup_Net)$degree)
summary(V(AssoGroup_Net)$wdegree)
summary(E(AssoGroup_Net)$weight)
QassoGroup <- quantile(E(AssoGroup_Net)$weight, seq(0, 1, 0.05) )
QassoGroup

FAssoGroup_Net_Tidy <- AssoGroup_Net_tidy %>% 
   activate(edges) %>% 
  filter(weight >= QassoGroup[[20]]) %>% 
  activate(nodes) %>%
  filter(!node_is_isolated()) %>%
  mutate(louvain = group_louvain()) %>% 
  as.igraph()

V(FAssoGroup_Net_Tidy)$btwn <- betweenness(FAssoGroup_Net_Tidy, weights = E(FAssoGroup_Net_Tidy)$weight, normalized = TRUE)

ecount(FAssoGroup_Net_Tidy)
library(randomcoloR)
  colorlouvain <- distinctColorPalette(k = length(unique(V(FAssoGroup_Net_Tidy)$louvain))) 
colorlouvain <- brewer.pal(n = length(unique(V(FAssoGroup_Net_Tidy)$louvain)), name = "Set1")
names(colorlouvain) <-  unique(V(FAssoGroup_Net_Tidy)$louvain)

ggraph(FAssoGroup_Net_Tidy, layout = "kk") + 
  geom_edge_diagonal( aes( width = weight, alpha= weight)) +
  geom_node_point( aes(color = as.character(louvain), size = btwn) )+ 
  geom_node_text(aes(label = name),repel = TRUE, size = 2.5)+
  scale_edge_width(range = c(0.1,3)) +
  scale_edge_alpha(range = c(0.2,0.6))+
  scale_size(range = c(2,8))+
  scale_color_manual(values = colorlouvain )+
  labs(color = "Communautés (walktrap)", size = "Intermédiarité",
       caption = "Note : Seuls 25% des liens par ordre décroissant de poids ont été conservés pour cette représentation.\nLes indicateurs (communautés et intermédiarité) sont calculés sur le graphe entier et pondéré.\nETMUN 2019/ PG 2020")+
  theme_graph(base_family = 'Helvetica')+
  theme(plot.margin = unit(rep(0.1, 4), "points"))


# Save
ggsave("OUT/ETMUN_AssoNet_TfIdfoverlapp.pdf", width = 8.3, height = 8.3 , units = "in" )


## try first flow method

library(flows)

mat <-get.adjacency(AssoGroup_Net, sparse = FALSE, attr='weight')
head(mat)

M1stflows <- firstflows(mat, method = "nfirst", k = 2 ) 

g1stflowAsso <- graph.adjacency(M1stflows)

graph.density(g1stflowAsso)

V(g1stflowAsso)$wdegree <- strength(g1stflowAsso, mode = "all")
V(g1stflowAsso)$degree <- degree(g1stflowAsso, mode = "all")
louvainAll <- cluster_spinglass(g1stflowAsso, weights = E(g1stflowAsso)$weight)
louvainAll$membership
louvainAll$modularity
V(g1stflowAsso)$louvainAll <- louvainAll$membership
V(g1stflowAsso)$btwnAll <- betweenness(g1stflowAsso, normalized = TRUE)

colorlouvain <- brewer.pal(n = length(unique(V(g1stflowAsso)$louvainAll)), name = "Set1")
names(colorlouvain) <-  unique(V(g1stflowAsso)$louvainAll)

ggraph(g1stflowAsso, layout = "graphopt") + 
  # geom_edge_diagonal(start_cap = circle(4, 'mm'),end_cap = circle(4, 'mm')) +
  geom_edge_fan(arrow = arrow(length = unit(2.5, 'mm')), 
                start_cap = circle(3, 'mm'),
                end_cap = circle(3, 'mm') , alpha = 0.3)+
  geom_node_point( aes(color = as.character(louvainAll), size = btwnAll) )+ 
  geom_node_text(aes(label = name),repel = TRUE, size = 2.5)+
  scale_size(range = c(2,8))+
  scale_color_manual(values = colorlouvain )+
  labs(color = "Communautés (walktrap)", size = "Intermédiarité",
       caption = "Note : Seuls 25% des liens par ordre décroissant de poids ont été conservés pour cette représentation.\nLes indicateurs (communautés et intermédiarité) sont calculés sur le graphe entier et pondéré.\nETMUN 2019/ PG 2020")+
  theme_graph(base_family = 'Helvetica')+
  theme(plot.margin = unit(rep(0.1, 4), "points"))

## TF-Idf by louvain communitites


CommunityAsso <- as_data_frame(AssoGroup_Net, what = "vertices")
CommunityAsso <- CommunityAsso %>% select(name,louvainAll)
CommunityAsso <- CommunityAsso %>% left_join(select(ETMUN_Text, name = Acronym, Code ))

## major flow
CommunityAsso <- as_data_frame(g1stflowAsso, what = "vertices")
CommunityAsso <- CommunityAsso %>% select(name,louvainAll)
CommunityAsso <- CommunityAsso %>% left_join(select(ETMUN_Text, name = Acronym, Code ))


# tf-idf by louvin community

TextCat <- CleanParsedText %>% left_join(select(CommunityAsso, doc_id = Code, name, louvainAll))
TextCat <- TextCat %>% mutate(louvainAll = as.character(louvainAll))

cat <- "louvainAll"
Cat_words <- TextCat %>% 
  filter(pos == "NOUN" | pos == "ADJ") %>% 
  # filter(entity == "") %>%
  count(!!as.name(cat), lemma, sort = TRUE)%>% filter(!is.na(!!as.name(cat)))


plot_cat <- Cat_words %>%
  bind_tf_idf(lemma,!!as.name(cat), n) %>%
  mutate(lemma = fct_reorder(lemma, tf_idf)) 

n_cat <- CommunityAsso %>% group_by(!!as.name(cat)) %>% count()
n_cat <- n_cat %>% ungroup %>% mutate(louvainAll = as.character(louvainAll))


#

plot_cat %>% 
  group_by(!!as.name(cat)) %>% 
  top_n(5, tf_idf) %>% 
  ungroup() %>%
  ggplot(aes(reorder_within(lemma, tf_idf,!!as.name(cat) ), tf_idf, fill = !!as.name(cat))) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf", caption = "Source : ETMUN 2019 / P. Gourdon 2020") +
  scale_x_reordered()+
  scale_fill_manual(values = colorlouvain) +
  facet_wrap(as.formula(paste("~", cat)), ncol = 2, scales = "free") +
  coord_flip()+ 
  geom_label(aes(label = n, x= reorder_within(lemma, tf_idf,!!as.name(cat) ), y = tf_idf),
             position = position_stack(0.5), color = "black", size = 2)+
  geom_text( data    = n_cat,
             mapping = aes(x = -Inf, y = Inf, label = paste("Nb asso = ", n, sep = "")),
             vjust = -1, hjust = 1.2, size = 2.5)+
  theme(legend.position = "none")



ggsave("OUT/ETMUN_tfidf_louvain.pdf", width = 8.3, height = 8.3 , units = "in" )

# same with pos-tag of text net

TextCat <- AssoGroup_nouns %>% left_join(select(CommunityAsso, Acronym = name, louvainAll))
cat <- "louvainAll"
Cat_words <- TextCat %>% ungroup %>% select(-Acronym) %>%
  mutate(louvainAll = as.character(louvainAll)) %>% 
  group_by(louvainAll, lemma) %>%
 summarise(n = sum(count)) 


plot_cat <- Cat_words %>%
  bind_tf_idf(lemma,!!as.name(cat), n) %>%
  mutate(lemma = fct_reorder(lemma, tf_idf)) 




#

plot_cat %>% 
  group_by(!!as.name(cat)) %>% 
  top_n(10, tf_idf) %>% 
  ungroup() %>%
  ggplot(aes(reorder_within(lemma, tf_idf,!!as.name(cat) ), tf_idf, fill = !!as.name(cat))) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf", caption = "Source : ETMUN 2019 / P. Gourdon 2020") +
  scale_x_reordered()+
  scale_fill_manual(values = colorlouvain) +
  facet_wrap(as.formula(paste("~", cat)), ncol = 2, scales = "free") +
  coord_flip()+ 
  geom_label(aes(label = n, x= reorder_within(lemma, tf_idf,!!as.name(cat) ), y = tf_idf),
             position = position_stack(0.5), color = "black", size = 2)+
  geom_text( data    = n_cat,
             mapping = aes(x = -Inf, y = Inf, label = paste("Nb asso = ", n, sep = "")),
             vjust = -1, hjust = 1.2, size = 2.5)+
  theme(legend.position = "none")



## Preparation words network




V(AssoWords_Net)$wdegree <- strength(AssoWords_Net, mode = "all")
V(AssoWords_Net)$degree <- degree(AssoWords_Net, mode = "all")

AssoWords_Net

summary(V(AssoWords_Net)$degree)
summary(V(AssoWords_Net)$wdegree)
summary(E(AssoWords_Net)$weight)
quantile(E(AssoWords_Net)$weight, seq(0, 1, 0.05 ))

library(tidygraph)

AssoWords_Net_tidy <- as_tbl_graph(AssoWords_Net)


FAsswords_Net_Tidy <- AssoWords_Net_tidy %>% 
  activate(edges) %>% 
  filter(weight >   0.638899029 ) %>% 
  activate(nodes) %>%
  filter(!node_is_isolated()) %>%
  mutate(louvain = group_louvain()) %>% as.igraph()

V(FAsswords_Net_Tidy)$btwm <- betweenness(FAsswords_Net_Tidy, weights = E(FAsswords_Net_Tidy)$weight, normalized = TRUE)

vcount(FAsswords_Net_Tidy)

ecount(FAsswords_Net_Tidy)

louvainColor <- distinctColorPalette(k = length(unique(V(FAsswords_Net_Tidy)$louvain)))
library(ggraph)

 ggraph(FAsswords_Net_Tidy, layout = "fr") + 
  geom_edge_link( aes( width = weight, alpha = weight) )+
  geom_node_point( aes(color = as.character(louvain), size = btwm) )+ 
  geom_node_text(aes(label = name),repel = TRUE, size = 2.5)+
  scale_edge_width(range = c(0.1,2)) +
  scale_size(range = c(0.5,8))+
   scale_edge_alpha(range = c(0.1,0.3))+
  scale_color_manual(values = louvainColor )+
  labs(color = "Communautés (louvain)", size = "Intermédiarité des termes", 
       caption = "ETMUN 2019/ PG 2020")+
  theme_graph()

VisTextNet(AssoWords_Net, label_degree_cut = 200)


#### NETWORK COOC terms


cooc <- cooccurrence(CleanParsedText$lemma, relevant = CleanParsedText$pos %in% c("NOUN", "ADJ"), skipgram = 2)


##Graph COOC

Coocnetworkdf <- cooc %>% filter(cooc>3) %>% rename(Cooccurrences = cooc)

wordnetwork <- graph_from_data_frame(Coocnetworkdf, directed = F )
wordnetwork
##Louvain
louvain <- cluster_louvain(wordnetwork, weights = E(wordnetwork)$Cooccurrences)
louvain
V(wordnetwork)$louvain <- louvain$membership
V(wordnetwork)$btwm <- betweenness(wordnetwork, weights = E(wordnetwork)$weight, normalized = TRUE)


## prepare vizu

# color
louvainColor <- distinctColorPalette(k = length(unique(V(wordnetwork)$louvain)))

# layout

minC <- rep(-Inf, vcount(wordnetwork))
maxC <- rep(Inf, vcount(wordnetwork))
minC[1] <- maxC[1] <- 0


layoutManual  <- create_layout(wordnetwork, layout = "fr", minx=minC, maxx=maxC,
                               miny=minC, maxy=maxC, niter = 2000)


# plot

ggraph(layoutManual) + 
  geom_edge_link( aes( width = Cooccurrences, alpha = Cooccurrences) )+
  geom_node_point( aes(color = as.character(louvain), size = btwm) )+ 
  geom_node_text(aes(label = name),repel = TRUE, size = 2.8)+
  scale_edge_width(range = c(0.5,3)) +
  scale_size(range = c(2,12))+
  scale_edge_alpha(range = c(0.2,0.6))+
  scale_color_manual(values = louvainColor )+
  labs(color = "Communautés\n(louvain)", size = "Intermédiarité\ndes termes", 
       caption = "ETMUN 2019/ PG 2020")+
  theme_graph(base_family = 'Helvetica')+
  guides(color=guide_legend(nrow=2, label.position = "bottom", override.aes = list(size=3, shape = 15)),
         size = guide_legend(label.position = "bottom"))+
  theme(legend.position="bottom",legend.direction="horizontal",  legend.box.just = "center", legend.spacing.x= unit(0, 'cm'), 
        plot.margin = unit(rep(0, 4), "points"))

# save 
ggsave(filename = "OUT/ETMUN_CoocNet.pdf", width = 11.7, height = 8.3, units = "in" )

## Keep only the first component

FirstComponent <- decompose.graph(wordnetwork)
FirstComponent <- FirstComponent[[1]]

##Louvain
louvain <- cluster_louvain(FirstComponent, weights = E(FirstComponent)$Cooccurrences)
louvain
V(FirstComponent)$louvain <- louvain$membership
V(FirstComponent)$btwm <- betweenness(FirstComponent, weights = E(FirstComponent)$weight, normalized = TRUE)


## prepare vizu

# color
louvainColor <- distinctColorPalette(k = length(unique(V(FirstComponent)$louvain)))

# louvainColor <- randomColor(count = length(unique(V(FirstComponent)$louvain)), luminosity = c( "bright"))

# layout

 minC <- rep(-Inf, vcount(FirstComponent))
 maxC <- rep(Inf, vcount(FirstComponent))
 minC[1] <- maxC[1] <- 0
 
  layoutManual  <- create_layout(FirstComponent, layout = "fr", minx=minC, maxx=maxC,
                                miny=minC, maxy=maxC, niter = 700)


# plot

ggraph(layoutManual) + 
  geom_edge_link( aes( width = Cooccurrences, alpha = Cooccurrences) )+
  geom_node_point( aes(fill = as.character(louvain), size = btwm), shape = 21, stroke= 0.1 )+ 
  geom_node_text(aes(label = name),repel = TRUE, size = 2.8)+
  scale_edge_width(range = c(0.5,3)) +
  scale_size(range = c(2,12))+
  scale_edge_alpha(range = c(0.2,0.6))+
  scale_fill_manual(values = louvainColor )+
  labs(fill = "Communautés\n(louvain)", size = "Intermédiarité\ndes termes", 
       caption = "Note : Seule la première composante connexe est représentée\nETMUN 2019/ PG 2020")+
  theme_graph(base_family = 'Helvetica')+
  guides(fill=guide_legend(nrow=2, label.position = "bottom", override.aes = list(size=3, shape = 22)),
         size = guide_legend(label.position = "bottom"))+
  theme(legend.position="bottom",legend.direction="horizontal",  legend.box.just = "center", legend.spacing.x= unit(0, 'cm'), 
        plot.margin = unit(rep(0, 4), "points"))


ggraph(layoutManual) + 
  geom_edge_link( aes( width = Cooccurrences), alpha = 0.2 )+
  geom_node_point( aes(color = as.character(louvain), size = btwm))+ 
  geom_node_text(aes(label = name, size = btwm))+
  scale_edge_width(range = c(0.5,3)) +
  scale_size(range = c(2.3,7.5))+
  scale_edge_alpha(range = c(0.2,0.6))+
  scale_color_manual(values = louvainColor )+
  labs(color = "Communautés\n(louvain)", size = "Intermédiarité\ndes termes", 
       caption = "Note : Seule la première composante connexe est représentée\nETMUN 2019/ PG 2020")+
  theme_graph(base_family = 'Helvetica')+
  guides(color=guide_legend(nrow=2, label.position = "bottom", override.aes = list(size=3, shape = 15)),
         size = guide_legend(label.position = "bottom"))+
  theme(legend.position="bottom",legend.direction="horizontal",  legend.box.just = "center", legend.spacing.x= unit(0, 'cm'), 
        plot.margin = unit(rep(0.1, 4), "points"))


nodes<- as_data_frame(FirstComponent, what = "vertices")

table(nodes$louvain)

# explor usage

write.csv2(ETMUN_Text, "DataProd/ETMUN_AssoText_Corpus.csv", row.names = FALSE, fileEncoding = "UTF-8")
library(R.temis)
corpus <- import_corpus("DataProd/ETMUN_AssoText_Corpus.csv", textcolumn = 31, format = "csv", language = "en" )

dtmTemis <- build_dtm(corpus)
concordances(corpus, dtmTemis, "fashion")
# save 
ggsave(filename = "OUT/ETMUN_CoocNet_1stComponent.pdf", width = 11.7, height = 8.3, units = "in" )
ggsave(filename = "OUT/ETMUN_CoocNet_1stComponent.svg", width = 11.7, height = 8.3, units = "in" )
