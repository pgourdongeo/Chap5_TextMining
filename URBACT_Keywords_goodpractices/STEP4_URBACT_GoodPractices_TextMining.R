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

### Data

# Table from websscrap 2018
GoodPract <- read.csv2("~/Chap5_TextMining/Data/URBACT/GoodPracticesUrbact_WebscrapMay2018_Clean.csv", stringsAsFactors = F)

# Table with some attributes 
AttributesClean <- read.csv2("DataProd/GoodPracticesURBACT_Corpus.csv", stringsAsFactors = F,sep = ",")
# Code GN for cities
DicoGN <- read.csv2("~/Chap5_TextMining/Data/URBACT/DicoCity_GN_GoodPracticesURBACT.csv", stringsAsFactors = F)
DicoGN <- DicoGN %>% select(- CodePractices, - CountryCode) %>% distinct()



# Geom data
## rec

rec <- st_read("~/Chap5_TextMining/Data/Geometry/rec_3035.geojson")

#EU 
sfEU <- st_read("~/Chap5_TextMining/Data/Geometry/fondEuropeLarge.geojson", stringsAsFactors = FALSE,crs = 3035)



corpus <- import_corpus("DataProd/GoodPracticesURBACT_Corpus.csv", textcolumn = 20, format = "csv", language = "en" )


############# ====  TEXT MINING ===========###############


###############""" SpacyR and tidy approach
# install.packages("spacyr")
library(spacyr)

# spacy_install()
#spacy_download_langmodel(model = "en_core_web_lg" )

# Choose model
spacy_initialize(model = "en_core_web_lg")
#spacyr::spacy_finalize()


# process documents and obtain a data.table
textPractices <- AttributesClean$AllText
names(textPractices) <- AttributesClean$CodePractices
parsedtxt <- spacy_parse(textPractices)




# Remove stopwords

CleanParsedText<- unnest_tokens(parsedtxt, word, token) %>%
  dplyr::anti_join(stop_words)

################ ====~ BASIC TEXT MINING ==== ##########

# Size of the corpus

DocSize <- parsedtxt %>% group_by(doc_id)%>% summarise(NWords = as.numeric(n()))%>% arrange(NWords)  %>% as.data.frame()
ggplot(DocSize) + 
  geom_bar(aes(x= reorder(doc_id,-NWords), y = NWords), stat = "identity") + 
  theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1)) + 
  labs(title = "Nombre de mots par document du corpus des bonnes pratiques URBACT", x = "Document ID") + 
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

stats <- txt_freq(CleanParsedText$pos)
ggplot(stats) +
  geom_bar(aes(x = reorder(key, -freq_pct), y = freq_pct), stat = "identity") +
  coord_flip() + theme_bw()

## nouns
stats <- subset(CleanParsedText, pos %in% c("NOUN")) 
stats <- txt_freq(stats$lemma) 
g1 <- ggplot(stats[1:30, ]) +
  geom_bar(aes(x = reorder(key, freq), y = freq), stat = "identity") +
  coord_flip() + theme_bw() +  labs(title = "Noms",x = NULL, y = "Occurrences") + scale_y_continuous(n.breaks = 10)
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
cooctermsCorpus %>% filter(term1 == "create" & cooc > 5)


## Plot
gridVerbNom <- g1/g3

gridVerbNom
ggsave("OUT/GdPractURBACT_Top30_Verb_Noun.pdf", width = 8.3, height = 8.3 , units = "in" )


################ Key words

## Using RAKE

stats <- keywords_rake(x = parsedtxt, term = "lemma", group = "doc_id", 
                       relevant = parsedtxt$pos %in% c("NOUN", "ADJ"), ngram_max = 3,n_min = 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))

statsTop <- stats %>% filter(ngram >1)%>% top_n(20, freq)
summary(stats$freq)
summary(stats$rake)
statsTop <- stats %>% filter(ngram >1)%>% filter(freq>10 & rake > 1.8)
ggplot(statsTop) +
  geom_bar(aes(x = reorder(key, freq), y = freq), stat = "identity") +
  # scale_y_log10(n.breaks = 8)+
  scale_y_continuous(n.breaks = 8)+
  coord_flip() + theme_bw() +  labs(x = "Mots-clés identifiés par RAKE", y = "Occurrences dans le corpus", 
                                    caption = "Note : les mots-clés représentés sont issus du triple filtrage suivant : n-gram > 1 & occurences > 10 & Score 'Rake' > 1.8\nSources : URBACT site web / P.Gourdon 2020")

ggsave("OUT/KW_GdPractURBACT_RAKE_ngram2_freq10_Rake1.8.pdf", width = 8.3, height = 5.8 , units = "in" )
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
statsTop <- stats %>% filter(freq > 15 & pmi >4.8)
ggplot(statsTop) +
  geom_bar(aes(x = reorder(key, freq), y = freq), stat = "identity") +
  coord_flip() + theme_bw() +  labs(x = "Mots-Clés identifiés avec la PMI Collocation", y = "Nombre d'occurrences dans le corpus",
                                    caption = "Note : les mots-clés représentés sont issus du double filtrage suivant : occurences > 15 & Score 'PMI' > 4.8\nSources : URBACT site web / P.Gourdon 2020")

ggsave("OUT/KW_GdPractURBACT_PMI_Top30.pdf", width = 8.3, height = 5.8 , units = "in" )
## Tf idf 

Ntermdoc <- CleanParsedText %>% 
  filter(!pos == "VERB" ) %>% 
  filter(!pos == "PROPN" & !pos == "NUM" & !pos == "X") %>% 
  filter(entity == "")%>%
  count(lemma, doc_id)
TfIdfDoc <- Ntermdoc %>% bind_tf_idf(lemma, doc_id, n)

TopTfIdf <- TfIdfDoc %>% group_by(doc_id)%>% top_n(10, tf_idf)

TfIdMean <- TfIdfDoc %>% group_by(lemma) %>% summarise(MeanTfIdf = mean(tf_idf), Freq = sum(n), MaxTfIdf = max(tf_idf), idf = max(idf))

## mots frequent terms with low idf
Top1 <- TfIdMean %>% filter(idf<1) %>% arrange(desc(Freq))%>% ungroup() %>% top_n(50, Freq)

ga <-ggplot(Top1) + 
  geom_point(aes(x=Freq,y = idf, size = MeanTfIdf), shape = 21, fill = "orange", color = "grey80", alpha= 0.7) + 
  # scale_size(range = c(2:10), breaks = c(min(TopTfIdfFreq$MeanTfIdf): max(TopTfIdfFreq$MeanTfIdf)))+
  # scale_y_log10()+ 
  scale_x_log10() + 
  geom_text_repel(aes(label = lemma,x=Freq,y = idf ), size = 2.5, segment.alpha= 0.5,segment.size = 0.3)+
  labs(x = "Nombre d'occurrences dans le corpus (log10)", y = "IDF", size = "Moyenne du Tf-Idf", 
       title = "A")+
  theme(legend.position="bottom")

## mots frequent terms with high idf
Top2 <- TfIdMean %>% filter(idf>1) %>% arrange(desc(Freq))%>% ungroup() %>% top_n(50, Freq)

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

ggsave("OUT/GdPracticeURBACT_topterms_Idf_N.pdf", width = 8.3, height = 8.3 , units = "in" )


### Variable
#### tf idf by category



CleanParsedText <- CleanParsedText %>% mutate(doc_id = as.numeric(doc_id))%>%
  left_join(select(AttributesClean, CountryCode,Region,ClassePop,
                   ClasseStart,SizeClassURBACT, doc_id = CodePractices))

Politics <- read.csv2("~/Chap5_TextMining/Data/CountryPolitical/CountryInfo_PoliticalTypo.csv", 
                      encoding = "UTF-8", stringsAsFactors = FALSE)
colnames(Politics)
CleanParsedText <- CleanParsedText %>% left_join(select(Politics, CountryCode = iso_a2,
                                                        LocGovType_HorizontalPwrRelation, 
                                                        LocGovType_VerticalPwrRelation,
                                                        LocGovType_PoliticalLeadership,
                                                        LocGovType_MunicipalAdmin))

skim(CleanParsedText$LocGovType_MunicipalAdmin)

### Region
cat <- "CountryCode"
Cat_words <- CleanParsedText %>% 
  filter(!pos == "PROPN" & !pos == "NUM" & !pos == "X") %>% 
  filter(entity == "") %>%
  count(!!as.name(cat), lemma, sort = TRUE)


plot_cat <- Cat_words %>%
  bind_tf_idf(lemma,!!as.name(cat), n) %>%
  mutate(lemma = fct_reorder(lemma, tf_idf)) 

n_cat <- AttributesClean %>% group_by(!!as.name(cat)) %>% count()

#specific filter for country (do not run for region)
countryfilter <- n_cat %>% filter(n>3) %>% select(CountryCode) %>% deframe()

plot_cat <- plot_cat %>% filter(CountryCode %in% countryfilter)
n_cat <- n_cat %>% filter(CountryCode %in% countryfilter)
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
  geom_text( data    = n_cat,
             mapping = aes(x = -Inf, y = -Inf, label = paste("Nb docs = ", n, sep = "")),
             hjust   = -4.2,
             vjust   = -1.2, size = 2.5)+
  theme(legend.position = "none")

ggsave("OUT/GdPracticeURBACT_Country_Idf_N.pdf", width = 8.3, height = 8.3 , units = "in" )
## Political typo

cat <- "LocGovType_PoliticalLeadership"
Cat_words <- CleanParsedText %>% 
  filter(!pos == "PROPN" & !pos == "NUM" & !pos == "X") %>% 
  filter(entity == "") %>%
  count(!!as.name(cat), lemma, sort = TRUE)


plot_cat <- Cat_words %>%
  bind_tf_idf(lemma,!!as.name(cat), n) %>%
  mutate(lemma = fct_reorder(lemma, tf_idf)) 

n_cat <- AttributesClean %>% left_join(select(Politics, CountryCode = iso_a2,
                                              LocGovType_HorizontalPwrRelation, 
                                              LocGovType_VerticalPwrRelation,
                                              LocGovType_PoliticalLeadership,
                                              LocGovType_MunicipalAdmin)) %>%  
            group_by(!!as.name(cat)) %>% count()

plot_cat %>% 
  group_by(!!as.name(cat)) %>% 
  top_n(10, tf_idf) %>% 
  ungroup() %>%
  ggplot(aes(reorder_within(lemma, tf_idf, !!as.name(cat)), tf_idf, fill = !!as.name(cat)), show.legend = FALSE) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  scale_x_reordered() +
  facet_wrap(as.formula(paste("~", cat)), ncol = 2, scales = "free") +
  coord_flip()+ 
  geom_label(aes(label = n, x= reorder_within(lemma, -tf_idf, !!as.name(cat)), y = tf_idf), position = position_stack(0.5), color = "black", size = 2)+
  geom_text( data    = n_cat,
             mapping = aes(x = -Inf, y = -Inf, label = paste("Nb docs = ", n, sep = "")),
             hjust   = -3,
             vjust   = -1.2, size = 2.5)+
  theme(legend.position = "none")

## Size

cat <- "SizeClassURBACT"
Cat_words <- CleanParsedText %>% 
  filter(!pos == "PROPN" & !pos == "NUM" & !pos == "X") %>% 
  filter(entity == "") %>%
  count(!!as.name(cat), lemma, sort = TRUE)


plot_cat <- Cat_words %>%
  bind_tf_idf(lemma,!!as.name(cat), n) %>%
  mutate(lemma = fct_reorder(lemma, tf_idf)) %>% 
  filter(! lemma == "http://aviles.es/web/ayuntamiento/inmigracion")

n_cat <- AttributesClean %>% group_by(!!as.name(cat)) %>% count()
plot_cat %>% 
  group_by(!!as.name(cat)) %>% 
  top_n(10, tf_idf) %>% 
  ungroup() %>%
  mutate(lemma = reorder(lemma, tf_idf)) %>%
  ggplot(aes(lemma, tf_idf, fill = !!as.name(cat))) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(as.formula(paste("~", cat)), ncol = 2, scales = "free") +
  coord_flip()+ 
  geom_label(aes(label = n, x= lemma, y = tf_idf), position = position_stack(0.5), color = "black", size = 2)+
  geom_text( data    = n_cat,
             mapping = aes(x = -Inf, y = -Inf, label = paste("Nb docs = ", n, sep = "")),
             hjust   = -3,
             vjust   = -1.2, size = 2.5)+
  theme(legend.position = "none")


ggsave("OUT/GdPracticeURBACT_tfIdf_SizeClass.pdf", width = 8.3, height = 8.3 , units = "in" )
## Period


CleanParsedText$ClasseStart <- fct_recode(CleanParsedText$ClasseStart,
                                          "Before 2010" = "Before 1990",
                                          "Before 2010" = "1990-2000",
                                          "Before 2010" = "2000-2007",
                                          "Before 2010" = "2007-2010")
CleanParsedText$ClasseStart <- as.character(CleanParsedText$ClasseStart)

cat <- "ClasseStart"
Cat_words <- CleanParsedText %>% filter(!is.na(ClasseStart)) %>%
  filter(!pos == "PROPN" & !pos == "NUM" & !pos == "X") %>% 
  filter(entity == "") %>%
  count(!!as.name(cat), lemma, sort = TRUE)


plot_cat <- Cat_words %>%
  bind_tf_idf(lemma,!!as.name(cat), n) %>%
  mutate(lemma = fct_reorder(lemma, tf_idf)) 

plot_cat %>% 
  group_by(!!as.name(cat)) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>%
  mutate(lemma = reorder(lemma, tf_idf)) %>%
  ggplot(aes(lemma, tf_idf, fill = !!as.name(cat))) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(as.formula(paste("~", cat)), ncol = 2, scales = "free") +
  coord_flip()

# KW by caterory
cat <- "LocGovType_MunicipalAdmin"
cat_KW <- parsedtxt %>% mutate(doc_id = as.numeric(doc_id)) %>% 
  left_join(select(AttributesClean, CountryCode,Region,ClassePop,ClasseStart,SizeClassURBACT,doc_id = CodePractices))%>%
  left_join(select(Politics, CountryCode = iso_a2,
                   LocGovType_HorizontalPwrRelation, 
                   LocGovType_VerticalPwrRelation,
                   LocGovType_PoliticalLeadership,
                   LocGovType_MunicipalAdmin))
cat_KW <- cat_KW %>% 
  group_by(!!as.name(cat))%>%
  do(keywords_rake(x = ., term = "lemma", group = "doc_id", 
                   relevant = .$pos %in% c("NOUN", "ADJ"), ngram_max = 4,n_min = 3) )


statsTop <- cat_KW %>% group_by(!!as.name(cat)) %>% filter(freq>5 & rake > 1.5) %>% top_n(8, freq)
statsTop <- cat_KW %>% group_by(!!as.name(cat)) %>% top_n(5, rake)
ggplot(statsTop) +
  geom_bar(aes(x = reorder_within(keyword, freq, !!as.name(cat)), y = freq), stat = "identity") + scale_x_reordered()+
  coord_flip() + theme_bw() +  labs(x = "Keywords identified by RAKE", y = "Fréquence")+facet_wrap(as.formula(paste("~", cat)),scales = "free")


## Using Pointwise Mutual Information Collocations
# remove stop words

cat_KW <- CleanParsedText %>% filter(!pos == "PROPN" & !pos == "NUM" & !pos == "X") %>% 
  filter(entity == "") %>%
  group_by(!!as.name(cat))%>%
  do(keywords_collocation(x = ., term = "lemma", group = "sentence_id", ngram_max = 4, n_min = 3) )

statsTop <- cat_KW %>% group_by(!!as.name(cat)) %>% top_n(5, freq)
statsTop <- cat_KW %>% group_by(!!as.name(cat)) %>% filter(freq>5 & pmi> 6) %>% top_n(5, freq)

ggplot(statsTop) +
  geom_bar(aes(x = reorder_within(keyword, freq, !!as.name(cat)), y = freq), stat = "identity") +
  coord_flip() + theme_bw() +  labs(x = "Keywords identified by PMI Collocation", y = "Fréquence") +scale_x_reordered()+
  facet_wrap(as.formula(paste("~", cat)),scales = "free")



######DTM
## Note keep the doc id and the vector variable in the SAME order, because specific_terms(dtm2, SizeVec) does not perform a merge

data <- CleanParsedText %>%  filter(!pos == "PROPN" & !pos == "NUM" & !pos == "X") %>% 
  filter(entity == "") %>% 
  arrange(doc_id)


x4 <- document_term_frequencies(data[,c("doc_id","lemma")])

x5 <- document_term_frequencies_statistics(x4)

### DTM
dtm <- document_term_matrix(x4)


saveRDS(dtm,"DataProd/dtm_test.rds")

# some words in context
dtmTemis <- build_dtm(corpus)


concordances(corpus, dtmTemis, c("create", "citizen") )
concordances(corpus, dtmTemis, "created")
concordances(corpus, dtmTemis, "immigrants")


### Specific terms

library(R.temis)

dtm2<- as.DocumentTermMatrix(dtm, weighting = "weightTfIdf" )
SizeVec <- data %>% select(doc_id,SizeClassURBACT) %>% arrange(doc_id) %>% distinct() %>% select(SizeClassURBACT) %>% deframe
SizeSpec <- specific_terms(dtm2, SizeVec) 



RegionVec <- data %>% select(doc_id,Region) %>% arrange(doc_id) %>% distinct() %>% select(Region) %>% deframe
RegionSpec <- specific_terms(dtm2, RegionVec)

PoliticVec <- data %>% select(doc_id,LocGovType_MunicipalAdmin) %>% 
  arrange(doc_id) %>% distinct() %>% 
  select(LocGovType_MunicipalAdmin) %>% deframe
PoliticSpec <- specific_terms(dtm2, PoliticVec)

dfSpecTerms<- function(TemisObj){
  result <- list()
  for(i in 1: length(TemisObj)){
    df <- TemisObj[i] %>% as.data.frame()%>%rownames_to_column(var = "lemma")%>% mutate(Variable = names(TemisObj[i]) ) 
    colnames(df) <- c( "lemma", "Pct_Term/Level", "Pct_Level/Term",  "Pct_Global", "N_Level", "N_Global",  "tvalue", "Prob","Variable")
    result[[i]] <- df
    
  }
  
  dfResults <- bind_rows(result)
  dfResults <- dfResults %>% filter(!is.na(!!as.name("Pct_Term/Level")))
  
  return(dfResults)
}

dfSizeSpec <- dfSpecTerms(TemisObj = SizeSpec )


dfSizeSpec %>% group_by(Variable)%>%  mutate(lemma = reorder(lemma, tvalue)) %>%
  ggplot(aes(lemma, tvalue, fill = Variable)) +
  geom_bar(aes(x= reorder_within(lemma, tvalue, Variable), y = tvalue, fill = Variable), stat = "identity", show.legend = FALSE) + 
  scale_x_reordered()+
  labs(x = NULL, y = "tvalue") +
  facet_wrap(~Variable, ncol = 2, scales = "free") +
  coord_flip()

dfRegionSpec <- dfSpecTerms(TemisObj =RegionSpec)

dfRegionSpec %>% group_by(Variable)%>%  mutate(lemma = reorder(lemma, tvalue)) %>%
  ggplot(aes(lemma, tvalue, fill = Variable)) +
  geom_bar(aes(x= reorder_within(lemma, tvalue, Variable), y = tvalue, fill = Variable), stat = "identity", show.legend = FALSE) + 
  scale_x_reordered()+
  labs(x = NULL, y = "tvalue") +
  facet_wrap(~Variable, ncol = 2, scales = "free") +
  coord_flip()

dfPoliticSpec <- dfSpecTerms(TemisObj =PoliticSpec )

dfPoliticSpec %>% group_by(Variable)%>%  mutate(lemma = reorder(lemma, tvalue)) %>%
  ggplot(aes(lemma, tvalue, fill = Variable)) +
  geom_bar(aes(x= reorder_within(lemma, tvalue, Variable), y = tvalue, fill = Variable), stat = "identity", show.legend = FALSE) + 
  scale_x_reordered()+
  labs(x = NULL, y = "tvalue") +
  facet_wrap(~Variable, ncol = 2, scales = "free") +
  coord_flip()


################## Text similarities
AttributesClean <- AttributesClean %>% arrange(CodePractices)
library(LSAfun)
tvectors <- lsa(dtm)
tvec <- as.matrix(tvectors$dk)
docsimilarities <- multidocs(AttributesClean$AllText, tvectors = tvec, breakdown = TRUE)

MatrixSimiText <- docsimilarities$cosmat
rownames(MatrixSimiText) <- as.numeric(AttributesClean$CodePractices)
colnames(MatrixSimiText) <- as.numeric(AttributesClean$CodePractices)

diag(MatrixSimiText) <- NA
PairSimilText <- MatrixSimiText %>% as.data.frame()

PairSimilText <- PairSimilText %>% mutate(doc1 = rownames(.)) %>% 
  pivot_longer(-doc1, names_to = "doc2", values_to = "CosineSimilarity") %>%
  filter(!is.na(CosineSimilarity))

PairSimilText <- PairSimilText %>% mutate(doc1 = as.numeric(doc1), doc2 = as.numeric(doc2))  %>% 
  mutate(Pair = paste(pmin(doc1,doc2), pmax(doc1,doc2), sep = "_")) %>% filter(!duplicated(Pair))

PairSimilText <- as.data.frame(PairSimilText)
stargazer(PairSimilText, median = TRUE, )
summary(PairSimilText$CosineSimilarity)
# geographical distance

GN_info <- readRDS("~/Chap5_TextMining/Data/DBCity/UniqueGN_info_AllDB_Corr.rds")
UniqueGNGoodPractice <-data.frame(geonameId =  as.character(unique(DicoGN$geonameId)))

sf_CityGoodPractice <- UniqueGNGoodPractice %>% left_join(select(GN_info, geonameId, countryCode, lng_GN, lat_GN))
NAvalue <- sf_CityGoodPractice %>% filter(is.na(countryCode))
dfNa_info <- AttributesClean %>% filter(geonameId %in% NAvalue$geonameId) %>% 
  select(geonameId, countryCode = CountryCode, lng_GN = Long, lat_GN = Lat)%>%  mutate(geonameId = as.character(geonameId))

sf_CityGoodPractice <- sf_CityGoodPractice %>% filter(!is.na(countryCode)) %>% bind_rows(., dfNa_info)

sf_CityGoodPractice <- st_as_sf(sf_CityGoodPractice, coords = c("lng_GN", "lat_GN"), crs = 4326) %>% st_transform(crs = 3035)
geoDist <- st_distance(sf_CityGoodPractice, by_element = FALSE) 

geoDist <- as.data.frame(unclass(geoDist))
colnames(geoDist) <- rownames(geoDist)

PairGeoDist <- geoDist %>% mutate(city1 = rownames(.)) %>% 
  pivot_longer(-city1, names_to = "city2", values_to = "GeoDist") 

sf_CityGoodPracticeINFO <- sf_CityGoodPractice %>% mutate(ID = rownames(.)) %>% st_drop_geometry()

PairGeoDist <- PairGeoDist %>% left_join(select(sf_CityGoodPracticeINFO, City1GN = geonameId, city1 = ID)) %>% 
  left_join(select(sf_CityGoodPracticeINFO, City2GN = geonameId, city2 = ID))

AttributesClean <- AttributesClean %>% mutate(geonameId = as.character(geonameId))
PairGeoDist <- PairGeoDist  %>% left_join(select(AttributesClean, City1GN = geonameId, doc1 = CodePractices, Region1 = Region, Class1 = SizeClassURBACT, Pop1 = ClassePop)) %>% 
  left_join(select(AttributesClean, City2GN = geonameId, doc2 = CodePractices, Region2 = Region, Class2 = SizeClassURBACT,  Pop2 = ClassePop))

PairGeoDist <- PairGeoDist %>% mutate(Pair = paste(pmin(doc1,doc2), pmax(doc1,doc2), sep = "_"))

PairSimilText <- PairSimilText %>%left_join(select(PairGeoDist, Pair, GeoDist, Region1, Region2,Class1, Class2, Pop1,Pop2)) %>% 
  filter(!duplicated(Pair))

SizeSimil <- PairSimilText %>% group_by(Pop1, Pop2) %>% summarise(mean = mean(CosineSimilarity), med = median(CosineSimilarity))
RegionSimil <- PairSimilText %>% group_by(Region1, Region2) %>% summarise(mean = mean(CosineSimilarity), med = median(CosineSimilarity))
ggplot(PairSimilText) + geom_point(aes(x = GeoDist, y = CosineSimilarity )) 



############" Topic modeling

#### LDA

## Filter DTM

# dtm <- dtm_remove_tfidf(dtm, top = 1500)

dtmf <- dtm_remove_tfidf(dtm, prob = 0.15)

# #frq filter
# lexiquecat <- data %>%
#   group_by(lemma) %>%
#   mutate(word_total = n()) %>%
#   ungroup() %>%
#   filter(word_total > 20)
# 
# #tf idf filter
# 
# Ntermdoc <- data %>% 
#   count(lemma, sentence_id) 
# 
# TfIdfDoc <- Ntermdoc %>% bind_tf_idf(lemma, sentence_id, n)
# 
# lexiquecat <- TfIdfDoc %>% top_frac(0.8, tf_idf)
# # convert into a document-term matrix
# # with document names such as sci.crypt_14147
# cat_dtm <- lexiquecat %>%
#   count(sentence_id, lemma) %>%
#   cast_dtm(sentence_id, lemma, n)

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

cat_lda <- LDA(dtmf, k = 22, control = list(seed = 2016), method = "Gibbs")




cat_lda %>%
  tidy() %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered()+labs(x = "termes")

ggsave("OUT/GdPractUrbact_LDA_DTM015_K22.pdf", width = 11.7, height = 8.3, units = "in")

# same topic plot but with interpretation (title)

## Naming topics

TableCorrespondanceTopic <- data.frame(topic = 1:22, Interpretation =
                                        c("1. Local Expertise, Data","2. Long-term Immigrant Integration", "3. Job Market",
                                           "4. School & Childhood","5. Gender Equality",
                                           "6. Art & Climate Change Awareness ","7. Tourism & Communication Campaign", "8. Food & Health",
                                           "9. Participatory Budget ", "10. Transportation & Mobility","11. Welcoming Refugees",
                                           "12. Innovation & Start-ups","13. Social Housing","14. Public Procurement",
                                           "15. Project Methodology", "16. Public Services & Information", "17. Energy Expertise",
                                           "18. Urban Renewal & Housing", "19. Shop, Entrepreneurship & Regeneration", "20. Events & Volunteering",
                                           "21. Biodiversity" ,"22. Town Administration"))



cat_lda_tidy <- cat_lda %>%
  tidy() %>% left_join(TableCorrespondanceTopic)

# deal with facet title too large
swr = function(string, nwrap=20) {
  paste(strwrap(string, width=nwrap), collapse="\n")
}
swr = Vectorize(swr)

cat_lda_tidy <- cat_lda_tidy %>% mutate(Interpretation = swr(Interpretation))

unique(cat_lda_tidy$Interpretation)

cat_lda_tidy$Interpretation <- factor(cat_lda_tidy$Interpretation, levels=c("1. Local Expertise,\nData", "2. Long-term\nImmigrant\nIntegration"  ,"3. Job Market",                          
                                                                            "4. School &\nChildhood"  ,"5. Gender Equality" , "6. Art & Climate\nChange Awareness" ,        
                                                                            "7. Tourism &\nCommunication\nCampaign", "8. Food & Health",  "9. Participatory\nBudget" ,                  
                                                                           "10. Transportation\n& Mobility", "11. Welcoming\nRefugees", "12. Innovation &\nStart-ups",                
                                                                           "13. Social Housing" , "14. Public\nProcurement" ,"15. Project\nMethodology"  ,                 
                                                                           "16. Public Services\n& Information" , "17. Energy\nExpertise" ,"18. Urban Renewal &\nHousing" ,
                                                                           "19. Shop,\nEntrepreneurship &\nRegeneration", "20. Events &\nVolunteering"  ,
                                                                           "21. Biodiversity",                           
                                                                            "22. Town\nAdministration" ))


# Color vector for topic
library(randomcoloR)
TopicColor <- distinctColorPalette(k = length(unique(cat_lda_tidy$Interpretation)))

names(TopicColor) <- unique(cat_lda_tidy$Interpretation)

# Plot with facet titles

cat_lda_tidy %>%
  group_by(Interpretation) %>%
  top_n(8, beta) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, Interpretation)) %>%
  ggplot(aes(term, beta, fill = Interpretation)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = TopicColor)+
  facet_wrap(~ Interpretation, scales = "free_y") +
  coord_flip() +
  scale_x_reordered()+labs(x = "termes", caption = "Source : URBACT site web / P. Gourdon 2020" )

ggsave("OUT/GdPractUrbact_LDA_DTM015_K22.pdf", width = 11.7, height = 8.3, units = "in")

#### Topics per document


Practice_lda_gamma <- tidy(cat_lda, matrix = "gamma")
Practice_lda_gamma

ggplot(Practice_lda_gamma,aes(x= as.factor(topic), y=document))+ 
  geom_tile(aes(fill = gamma*100),
            colour = "grey") + scale_fill_gradient(low = "white",
                                                   high = "steelblue")


summary(Practice_lda_gamma$gamma)

Lda_GammaDoc <- Practice_lda_gamma %>% mutate(PctGamma = gamma*100)%>% 
  # left_join(TableCorrespondanceTopic, by = c("topic"="CodeTopic")) %>%
  # select(-gamma, -topic)%>%
  select(-gamma)%>%
  spread(key = topic, value = PctGamma)



LdaDisjonct <- Lda_GammaDoc %>% mutate_at(.vars = vars(2:ncol(Lda_GammaDoc)), .funs = funs(ifelse(.>5,1,0)))

# Prepare info on good practice
AttributesClean <- AttributesClean %>% left_join(Politics, by = c("CountryCode" = "iso_a2"))

TableCorrespondanceTopic$topic <- as.character(TableCorrespondanceTopic$topic)

names(TopicColor) <- TableCorrespondanceTopic$Interpretation

## Explore gamma (mean and  median) by variable
# function

gammatopbycat <- function(Lda_GammaDoc, cat, DfInfoDoc,InterpretationTopic, TopicColor){
  require(tidyverse)
  require(directlabels)
  
  plots <- list()
  
  #Ndoc 
  Ndoc <- DfInfoDoc %>% group_by(!!as.name(cat))%>% summarise(N= n())
  
  
  # Compute mean and med of gamma for each modalities of categorical variable
  CatGamma <- Lda_GammaDoc %>% mutate(document = as.numeric(document)) %>% 
    left_join(select(DfInfoDoc,document = CodePractices, !!as.name(cat))) %>% 
    select(-document) %>% group_by(!!as.name(cat)) %>% summarise_all(list(mean = ~mean(.), med = ~median(.)))
  
  # Prepare df for plot and add rank
  CatGammaRank <- CatGamma %>% 
    pivot_longer(-!!as.name(cat), names_to = "Topics", values_to = "value") %>% 
    separate(Topics, sep= "_", remove = TRUE, into= c("topic", "variable")) %>% 
    group_by(!!as.name(cat),variable) %>%
    mutate(rank = rank(desc(value), ties.method = 'min', na.last = "keep") )
  
  # Add interpretation of topic
  CatGammaRank <- CatGammaRank %>% left_join(InterpretationTopic)
  CatGammaRank <- CatGammaRank %>% mutate(Interpretation = as.character(Interpretation))
  

  # Prepare top 3 by variable (mean or median)
  TopMean <- CatGammaRank %>% filter(variable == "mean" & rank < 6) %>% filter(!is.na(!!as.name(cat)))
  

  TopMed <-  CatGammaRank %>% filter(variable == "med" & rank < 6) %>% filter(!is.na(!!as.name(cat)))
  
  
  #Colorvector
  
  FilterTopicColor <- TopicColor[names(TopicColor) %in% unique(TopMean$Interpretation)]
  
  # plot mean
  
  GammaMeanPlot <- ggplot(TopMean, aes(reorder_within(Interpretation, value, !!as.name(cat)), value, fill = Interpretation)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = FilterTopicColor) +
    geom_text(data  = Ndoc,
              mapping = aes(x = -Inf, y = Inf, label = paste("Nb docs = ", N, sep = "")), 
              inherit.aes = FALSE, size = 2.5, vjust = -1, hjust = 1.2)+
      facet_wrap(as.formula(paste("~", cat)), scales = "free_y", ncol = 2) +
      coord_flip() +
      scale_x_reordered() +
      labs(x = "Thèmes", y = "Moyenne Gamma (%)", caption = "Source : URBACT site web / P. Gourdon 2020" )
      
  
  #Colorvector
  
  FilterTopicColor <- TopicColor[names(TopicColor) %in% unique(TopMed$Interpretation)]
  
  # plot med

  GammaMedPlot <- ggplot(TopMed, aes(reorder_within(Interpretation, value, !!as.name(cat)), value, fill = Interpretation)) +
    geom_col(show.legend = FALSE) +
    scale_fill_manual(values = FilterTopicColor) +
    geom_text(data  = Ndoc,
              mapping = aes(x = -Inf, y = Inf, label = paste("Nb docs = ", N, sep = "")), 
              inherit.aes = FALSE, size = 2.5, vjust = -1, hjust = 1.2)+
    facet_wrap(as.formula(paste("~", cat)), scales = "free_y", ncol = 2) +
    coord_flip() +
    scale_x_reordered() +
    labs(x = "Thèmes", y = "Médiane Gamma (%)", caption = "Source : URBACT site web / P. Gourdon 2020" ) 
    
  ## store plots
  
  plots[["GammaMeanPlot"]] <- GammaMeanPlot
  plots[["GammaMedPlot"]] <- GammaMedPlot
  
  return(plots)
}

# results exploration
RegionPlot <- gammatopbycat(Lda_GammaDoc, cat = "Region", DfInfoDoc = AttributesClean, InterpretationTopic = TableCorrespondanceTopic, TopicColor )

SizeClassPlot <- gammatopbycat(Lda_GammaDoc, cat = "SizeClassURBACT", DfInfoDoc = AttributesClean, InterpretationTopic = TableCorrespondanceTopic, TopicColor )

SizeClassPlot2 <- gammatopbycat(Lda_GammaDoc, cat = "ClassePop", DfInfoDoc = AttributesClean, InterpretationTopic = TableCorrespondanceTopic, TopicColor )

PoliticsPlot1 <- gammatopbycat(Lda_GammaDoc, cat = "LocGovType_MunicipalAdmin", DfInfoDoc = AttributesClean, InterpretationTopic = TableCorrespondanceTopic, TopicColor )

PoliticsPlot2 <- gammatopbycat(Lda_GammaDoc, cat = "LocGovType_HorizontalPwrRelation", DfInfoDoc = AttributesClean, InterpretationTopic = TableCorrespondanceTopic, TopicColor )

PoliticsPlot3 <- gammatopbycat(Lda_GammaDoc, cat = "LocGovType_VerticalPwrRelation", DfInfoDoc = AttributesClean, InterpretationTopic = TableCorrespondanceTopic, TopicColor )

PoliticsPlot4 <- gammatopbycat(Lda_GammaDoc, cat = "LocGovType_PoliticalLeadership", DfInfoDoc = AttributesClean, InterpretationTopic = TableCorrespondanceTopic, TopicColor )

# poiitic4 withou ceremonial mayors (IE)
dfPolitics4 <- AttributesClean %>% filter(!LocGovType_PoliticalLeadership == "Ceremonial Mayors")

PoliticsPlot4bis <- gammatopbycat(Lda_GammaDoc, cat = "LocGovType_PoliticalLeadership", DfInfoDoc = dfPolitics4, InterpretationTopic = TableCorrespondanceTopic, TopicColor )
# Get theme by city
CityGamma <- Lda_GammaDoc %>% left_join(select(AttributesClean,document = doc_id, City)) %>% select(-document) %>% group_by(City) %>% summarise_all( ~mean(.))

### Typology by city
## Top topic by City


CityGammaTop <- CityGamma %>% pivot_longer(-City, names_to = "Topics", values_to = "value") %>% group_by(City) %>% top_n(1, value)

CityGammaTop <- CityGammaTop %>% filter(!duplicated(City))


## Map 
SfcityGoodPractice <- AttributesClean %>% 
  select(City, SizeClassURBACT,Pop, Long, Lat) %>% left_join(CityGammaTop) %>%
  st_as_sf(., coords = c("Long", "Lat"), crs = 4326) %>%
  st_transform(3035) %>% filter(!duplicated(City))

myScaleBar <- data.frame(X = c(c(st_bbox(rec)[3]-900000), c(st_bbox(rec)[3]-400000)),
                         Y = c(c(st_bbox(rec)[2]+200000), c(st_bbox(rec)[2]+200000)))

s <-summary(SfcityGoodPractice$Pop)
s[[1]]
bks <- c(s[[1]],s[[2]],s[[3]], s[[5]], s[[6]])
lbs <- c("5000", "75 000","200 000", "500 000", "7M" )

paletteTopics <- distinctColorPalette(k = length(unique(SfcityGoodPractice$Topics)))

CityTopicMap <- ggplot() + 
  geom_sf(data = sfEU, fill = "#bfbfbf", color = "white", size = 0.5) +
  geom_sf(data = SfcityGoodPractice ,
          mapping = aes( fill = Topics, size = Pop), shape= 21  , color = "gray80",show.legend = NA) +
  scale_size(name = "Population déclarée par URBACT (2018)",
             breaks = bks,
             labels = lbs,
             range = c(3, 12))+
  scale_fill_manual(values = paletteTopics)+
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
  labs(fill= "Thèmes Modélisés") +
  theme_void() +
  theme(legend.position =  c(0.25, 0.60), 
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 7.5))+
  guides(fill = guide_legend(override.aes = list(size = 3)))


##PCA
# DfACP <- CityGamma 
# 
# 
# 
# rownames(DfACP) <- DfACP$City
# DfACP <- DfACP %>% select(-City)
# res.pca <- PCA(DfACP,
#                scale.unit = TRUE,
#                #ind.sup = 7,
#                #quanti.sup = 7,
#                # quali.sup = 5,
#                graph = FALSE)
# 
# 
# 
# explor(res.pca)
# 
# CoordPCA<- as.data.frame(res.pca$ind$coord)
# 
# DfACP <- cbind(DfACP , CoordPCA)
# 
# DfACP <- DfACP %>% mutate(name = rownames(.)) 

## add PCA results


## CAH on dim ACP 
library(cluster)
library(ggdendro)
library(reshape2)

# compute classification ----

suppressWarnings(source("~/Chap5_TextMining/multi_bivariateFuntions_exploratR/Functions_anova_cah.R"))


myVar <- colnames(CityGamma[2:17])
cah <- ComputeClassif(df = CityGamma,
                      varquanti = myVar, method = "weighted", stand = TRUE)


dendro <- PlotDendro(classifobj = cah)
dendro
inert <- PlotHeight(classifobj = cah)
inert
myProfiles <- PlotProfile(classifobj = cah, nbclus = 10)
myProfiles$PROFILE
table(myProfiles$CLUSID)
#egocity.em2nw <- egocity.em2nw %>% mutate(CAH = myProfiles$CLUSID)
egocity.emnw <- egocity.emnw %>% mutate(CAH = myProfiles$CLUSID)

## plot profile of CAH with original variables
PlotCAHProfile <- egocity.emnw %>% select(c(4:10)| CAH) %>%  mutate(reinf = replace(reinf, is.nan(reinf), 0)) %>% pivot_longer(-CAH)

# library(questionr)
# irec(PlotCAHProfile)
## Recodage de PlotCAHProfile$name en PlotCAHProfile$name_rec
PlotCAHProfile$name_rec <- fct_recode(PlotCAHProfile$name,
                                      "Nb Projets" = "nbProjects",
                                      "Nb Villes" = "nbVilles",
                                      "Nb Liens" = "nbliens",
                                      "Densité" = "densite",
                                      "Nb Composantes\nConnexes" = "nbcomp",
                                      "Taille de la\nplus grande\ncomposante (%)" = "sizecomp",
                                      "Reinforcement" = "reinf")
PlotCAHProfile$name_rec <- as.character(PlotCAHProfile$name_rec)
ggplot(PlotCAHProfile)+ 
  geom_boxplot(aes(x =CAH, fill = CAH, y = value), axes = TRUE, outlier.colour="black", outlier.size=0.5, outlier.alpha = 0.6)+ 
  facet_wrap(~ name_rec, scales = "free_y")+ 
  labs(y = "", x = "", fill = "Typologies\ndes ego-networks\ndes villes URBACT\n(CAH)", 
       caption = "NB : matrice emnw\nSources : EUCICOP 2019 ; KEEP Closed Projects 2000-2019 / PG. 2020")+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 12))

ggsave( filename = "OUT/URBACTEGONetwork_CAH4Profiles_BoxPlot_emnw.pdf", width = 8.3, height = 5.8 , units = "in" )
# saveRDS(cah , "DataProd/cah_ego_eucicop_em2nw.rds")
# saveRDS(cah , "DataProd/cah_ego_eucicop_emnw.rds")

## plot profile of CAH with centralities indexes

# Add centralities mesures

CentralCities_emnw <- Centralities[[1]] %>% 
  select(-ends_with("2"))%>% 
  as.data.frame()%>% select(1:8) %>% rename(name = geonameId)

egocity.emnw <- egocity.emnw %>% left_join(CentralCities_emnw)

PlotCAHProfile <- egocity.emnw %>% select(CAH, ND,NB,NC,N_Eigen_C) %>% pivot_longer(-CAH)



ggplot(PlotCAHProfile)+ 
  geom_boxplot(aes(x =CAH, fill = CAH, y = value), axes = TRUE, outlier.colour="black", outlier.size=0.5, outlier.alpha = 0.6)+ 
  facet_wrap(~ name, scales = "free_y")+ 
  labs(y = "", x = "", fill = "Typologies\ndes ego-networks\ndes villes URBACT\n(CAH)", 
       caption = "Note : ces variables de centralités\nne sont pas prises en compte\ndans la CAH.\nSources : EUCICOP 2019 ; KEEP Closed Projects 2000-2019 / PG. 2020")+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 12))

ggsave( filename = "OUT/URBACTEGONetwork_CAH4Profiles_BoxPlotCentralities_emnw.pdf", width = 8.3, height = 5.8 , units = "in" )
### Explore clustering outcomes in relation to population and administrative status of cities


##" Topic by cities

CityText <- AttributesClean %>% group_by(City) %>% mutate(TextCity = paste(AllText, sep = ". ")) %>% filter(!duplicated(City)) %>% 
  select(City, Pop,  CountryCode, Region, SizeClassURBACT, Long, Lat , TextCity)

parsedtxtCity <- spacy_parse(CityText$TextCity)

CleanParsedTextCity<- unnest_tokens(parsedtxtCity, word, token) %>%
  dplyr::anti_join(stop_words)




data <- CleanParsedTextCity %>%  filter(!pos == "PROPN" & !pos == "NUM" & !pos == "X")%>% 
  filter(entity == "") 


x4 <- document_term_frequencies(data[,c("doc_id","lemma")])

x5 <- document_term_frequencies_statistics(x4)

### DTM filter
dtm <- document_term_matrix(x4)


# dtm <- dtm_remove_tfidf(dtm, top = 1500)

dtm<- dtm_remove_tfidf(dtm, prob = 0.20)

saveRDS(dtm,"DataProd/dtm_testcity.rds")

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
cat_lda <- LDA(dtm, k = 18, control = list(seed = 2016), method = "Gibbs")


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
