###############################  URBACT Networks reports  ##################################
#                               
#                          
# DESCRIPTION : Analyse textuelle des rapports de projets URBACT
#
# 
############################################################################## PG novembre 2020

setwd("~/Chap5_TextMining/URBACT_Network_Report/")

# Packages

# Packages

library(tidyverse)
library(tidytext)
library(skimr)
# library(GGally)
library(stargazer)
library(patchwork)
library(wordcloud)
library(udpipe)
library(ggrepel)
library(topicmodels)
library(R.temis)

### Data

# 
URBACT_project <- read.csv2("~/Chap5_TextMining/Data/URBACT/UrbactNetworks_complete_V2.csv", stringsAsFactors = F)


# Geom data
## rec

# rec <- st_read("~/Chap5_TextMining/Data/Geometry/rec_3035.geojson")
# 
# #EU 
# sfEU <- st_read("~/Chap5_TextMining/Data/Geometry/fondEuropeLarge.geojson", stringsAsFactors = FALSE,crs = 3035)
# 

####################### Create a corpus from text


dir <- "~/Chap5_TextMining/Data/URBACT/TextFilesUrbactNetwork/TexturbactNet/"
all_txts <- list.files(dir,pattern = '*.txt', full.names = T)

df<- map_df(all_txts, ~ data_frame(txt = read_file(.x)) %>% mutate(doc_id = basename(.x)))

df <- df %>% mutate(Code_Network = str_remove(doc_id, ".txt"))

# saveRDS(df, "DataProd/textUrbactNetwork.rds")
write.csv2(df, "DataProd/textUrbactNetwork.csv", fileEncoding = "UTF-8", row.names = F)
############# ====  TEXT MINING ===========###############


###############""" SpacyR and tidy approach
# install.packages("spacyr")
library(spacyr)

# spacy_install()
#spacy_download_langmodel(model = "en_core_web_lg" )

# Choose model
spacy_initialize(model = "en_core_web_lg")

#spacyr::spacy_finalize()

# test with one sentence
textReport <- "Berlin provides a case study of two practical interventions which can arrest and ultimately reverse decline"
parsedtxt <- spacy_parse(textReport)

stargazer(parsedtxt, summary = FALSE, rownames = FALSE)

# process documents and obtain a data.table
textReport <- df$txt
names(textReport) <- df$Code_Network
parsedtxt <- spacy_parse(textReport)




# Remove stopwords

CleanParsedText <- unnest_tokens(parsedtxt, word, token) %>%
  dplyr::anti_join(stop_words)

################ ====~ BASIC TEXT MINING ==== ##########

# Size of the corpus

DocSize <- parsedtxt %>% group_by(doc_id)%>% summarise(NWords = as.numeric(n()))%>% arrange(NWords)  %>% as.data.frame()
ggplot(DocSize) + 
  geom_bar(aes(x= reorder(doc_id,-NWords), y = NWords), stat = "identity") + 
  theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1)) + 
  labs(title = "Nombre de mots par document du corpus des rapports de projet URBACT", x = "Document ID") + 
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
cooctermsCorpus %>% filter(term1 == "create" & cooc > 5)


## Plot
gridVerbNom <- g1/g3

gridVerbNom
ggsave("OUT/URBACTreport_Top30_Verb_Noun.pdf", width = 8.3, height = 8.3 , units = "in" )


################ Key words

## Using RAKE

stats <- keywords_rake(x = parsedtxt, term = "lemma", group = "doc_id", 
                       relevant = parsedtxt$pos %in% c("NOUN", "ADJ"), ngram_max = 3,n_min = 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))

statsTop <- stats %>% filter(ngram >1)%>% top_n(30, freq)
summary(stats$freq)
summary(stats$rake)
statsTop <- stats %>% filter(ngram >1)%>% filter(freq>30 & rake > 2)
ggplot(statsTop) +
  geom_bar(aes(x = reorder(key, freq), y = freq), stat = "identity") +
  # scale_y_log10(n.breaks = 8)+
  scale_y_continuous(n.breaks = 8)+
  coord_flip() + theme_bw() +  labs(x = "Mots-clés identifiés par RAKE", y = "Occurrences dans le corpus", 
                                    caption = "Note : les mots-clés représentés sont issus du triple filtrage suivant : n-gram > 1 & occurences > 30 & Score 'Rake' > 2\nSources : URBACT site web / P.Gourdon 2020")

ggsave("OUT/KW_URBACTreport_RAKE_ngram2_freq30_Rake2.pdf", width = 8.3, height = 5.8 , units = "in" )

statsTop <- stats %>% filter(ngram >1) %>% filter(freq > 30 & rake > 1.5)
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
statsTop <- stats %>% filter(freq > 30 & pmi > 7)
ggplot(statsTop) +
  geom_bar(aes(x = reorder(key, freq), y = freq), stat = "identity") +
  coord_flip() + theme_bw() +  labs(x = "Mots-Clés identifiés avec la PMI Collocation", y = "Nombre d'occurrences dans le corpus",
                                    caption = "Note : les mots-clés représentés sont issus du double filtrage suivant : occurences > 30 & Score 'PMI' > 7\nSources : URBACT site web / P.Gourdon 2020")

ggsave("OUT/KW_URBACTreport_PMI_Top30.pdf", width = 8.3, height = 5.8 , units = "in" )
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

ggsave("OUT/URBACTreport_topterms_Idf_N.pdf", width = 8.3, height = 8.3 , units = "in" )


### Variable
#### tf idf by category



CleanParsedText <- CleanParsedText %>% 
  left_join(select(URBACT_project, LeadPartnerCountry,Region,Phase, Type,
                   Start, doc_id = Code_Network))

Politics <- read.csv2("~/Chap5_TextMining/Data/CountryPolitical/CountryInfo_PoliticalTypo.csv", 
                      encoding = "UTF-8", stringsAsFactors = FALSE)

Politics <- Politics %>% mutate(iso_a2 = recode(iso_a2,  "GB" = "UK"))
colnames(Politics)
CleanParsedText <- CleanParsedText %>% left_join(select(Politics, LeadPartnerCountry = iso_a2,
                                                        LocGovType_HorizontalPwrRelation, 
                                                        LocGovType_VerticalPwrRelation,
                                                        LocGovType_PoliticalLeadership,
                                                        LocGovType_MunicipalAdmin))

skim(CleanParsedText$LocGovType_MunicipalAdmin)

URBACT_project_text <- URBACT_project %>% filter(Code_Network %in% df$Code_Network)
### Region
cat <- "LeadPartnerCountry"
Cat_words <- CleanParsedText %>% 
  filter(!pos == "PROPN" & !pos == "NUM" & !pos == "X") %>% 
  filter(entity == "") %>%
  count(!!as.name(cat), lemma, sort = TRUE)


plot_cat <- Cat_words %>%
  bind_tf_idf(lemma,!!as.name(cat), n) %>%
  mutate(lemma = fct_reorder(lemma, tf_idf)) 

n_cat <- URBACT_project_text %>% group_by(!!as.name(cat)) %>% count()

#specific filter for country (do not run for region)
countryfilter <- n_cat %>% filter(n>3) %>% select(LeadPartnerCountry) %>% deframe()

plot_cat <- plot_cat %>% filter(LeadPartnerCountry %in% countryfilter)
n_cat <- n_cat %>% filter(LeadPartnerCountry %in% countryfilter)
#

plot_cat %>% 
  group_by(!!as.name(cat)) %>% 
  top_n(10, tf_idf) %>% 
  ungroup() %>%
  ggplot(aes(reorder_within(lemma, tf_idf,!!as.name(cat) ), tf_idf, fill = !!as.name(cat))) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf", caption = "Source : URBACT site web / P. Gourdon 2020") +
  scale_x_reordered()+
  facet_wrap(as.formula(paste("~", cat)), ncol = 2, scales = "free") +
  coord_flip()+ 
  geom_label(aes(label = n, x= reorder_within(lemma, tf_idf,!!as.name(cat) ), y = tf_idf), position = position_stack(0.5), color = "black", size = 2)+
  geom_text( data    = n_cat,
             mapping = aes(x = -Inf, y = Inf, label = paste("Nb docs = ", n, sep = "")),
             vjust = -1, hjust = 1.2, size = 2.5)+
  theme(legend.position = "none")

ggsave("OUT/URBACTreport_Country_Idf_N.pdf", width = 8.3, height = 8.3 , units = "in" )
## Political typo

cat <- "LocGovType_MunicipalAdmin"
Cat_words <- CleanParsedText %>% 
  filter(!pos == "PROPN" & !pos == "NUM" & !pos == "X") %>% 
  filter(entity == "") %>%
  count(!!as.name(cat), lemma, sort = TRUE) 

plot_cat <- Cat_words %>%
  bind_tf_idf(lemma,!!as.name(cat), n) %>%
  mutate(lemma = fct_reorder(lemma, tf_idf)) 

n_cat <- URBACT_project_text %>% left_join(select(Politics, LeadPartnerCountry = iso_a2,
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
             mapping = aes(x = -Inf, y = Inf, label = paste("Nb docs = ", n, sep = "")),
             vjust = -1, hjust = 1.2, size = 2.5)+
  theme(legend.position = "none")

## Phase

cat <- "Phase"
Cat_words <- CleanParsedText %>% 
  filter(!pos == "PROPN" & !pos == "NUM" & !pos == "X") %>% 
  filter(entity == "") %>%
  count(!!as.name(cat), lemma, sort = TRUE)


plot_cat <- Cat_words %>%
  bind_tf_idf(lemma,!!as.name(cat), n) %>%
  mutate(lemma = fct_reorder(lemma, tf_idf))

n_cat <- URBACT_project_text %>% group_by(!!as.name(cat)) %>% count()
plot_cat %>% 
  group_by(!!as.name(cat)) %>% 
  top_n(30, tf_idf) %>% 
  ungroup() %>%
  mutate(lemma = reorder(lemma, tf_idf)) %>%
  ggplot(aes(lemma, tf_idf, fill = !!as.name(cat))) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(as.formula(paste("~", cat)), ncol = 2, scales = "free") +
  coord_flip()+ 
  geom_label(aes(label = n, x= lemma, y = tf_idf), position = position_stack(0.5), color = "black", size = 2)+
  geom_text( data    = n_cat,
             mapping = aes(x = -Inf, y = Inf, label = paste("Nb docs = ", n, sep = "")),
             vjust = -1, hjust = 1.2, size = 2.5)+
  theme(legend.position = "none")


ggsave("OUT/URBACTReport_tfIdf_phase.pdf", width = 8.3, height = 8.3 , units = "in" )
## Period

# library(questionr)
# irec(CleanParsedText)
## Recodage de CleanParsedText$Start en CleanParsedText$Start_period
CleanParsedText$Start_period <- as.character(CleanParsedText$Start)
CleanParsedText$Start_period <- fct_recode(CleanParsedText$Start_period,
               "2007-2008" = "2008",
               "2009-2010" = "2009",
               "2009-2010" = "2010",
               "2007-2008" = "2007",
               "2015-2016" = "2015",
               "2015-2016" = "2016")


cat <- "Start_period"
Cat_words <- CleanParsedText %>% filter(!is.na(!!as.name(cat))) %>%
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

data <- CleanParsedText %>% filter(!pos == "PROPN" & !pos == "X" & !pos == "NUM") %>% 
  filter(entity == "") %>% 
  arrange(doc_id)



x4 <- document_term_frequencies(data[,c("doc_id","lemma")])

x5 <- document_term_frequencies_statistics(x4)

### DTM
dtm <- document_term_matrix(x4)


saveRDS(dtm,"DataProd/dtm_test.rds")

# some words in context

corpus <- import_corpus("DataProd/textUrbactNetwork.csv", textcolumn = 1, format = "csv", language = "en" )
dtmTemis <- build_dtm(corpus)


concordances(corpus, dtmTemis, c("sustainable", "regeneration") )
concordances(corpus, dtmTemis, "roma")
concordances(corpus, dtmTemis, "sustainable regeneration")

df %>% filter(str_detect(df$txt, "sustainable regeneration"))
df %>% filter(str_detect(df$txt, "urban fringe"))
df %>% filter(str_detect(df$txt, "women"))

URBACT_MembershipF %>% 
  filter(asciiName %in% c("Eidhoven", "Manchester", "Limoges", "Bilbao", "York", "Gdansk")) %>% 
  filter(City.Statut == "Lead Partner")

URBACT_project_text %>% filter(Code_Network %in% c("7", "18", "32", "36", "65", "73"))

URBACT_MembershipF %>% 
  filter(asciiName %in% c("Leoben", "Basingstoke", "Cesena", "Barnsley", "Gela")) %>% 
  filter(City.Statut == "Lead Partner")

URBACT_Project_Report %>% filter(Code_Network %in% c("23", "24", "39", "50", "70", "77"))
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
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered()+labs(x = "termes")

ggsave("OUT/URBACTreports_LDA_DTM015_K22.pdf", width = 11.7, height = 8.3, units = "in")


# same topic plot but with interpretation (title)

## Naming topics

TableCorrespondanceTopic <- data.frame(topic = 1:22, Interpretation =
                                         c("1. Territorial Cooperation","2. Retail & City Centre", "3. University",
                                           "4. Port & Hub","5. Heritage",
                                           "6. Mobility","7. Abandoned Spaces", "8. School & Youth Employment",
                                           "9. Methodology", "10. Innovation","11. Agro-food",
                                           "12. Funding","13. Tourism","14. Creative City",
                                           "15. Regeneration & Temporary use", "16. Housing & Neighbourhood", "17. City Branding",
                                           "18. Health & Care", "19. Migrants & Minorities", "20. Transport & Urban Sprawl",
                                           "21. Resilience & Transition" ,"22. Target groups"))



cat_lda_tidy <- cat_lda %>%
  tidy() %>% left_join(TableCorrespondanceTopic)

# deal with facet title too large
swr = function(string, nwrap=20) {
  paste(strwrap(string, width=nwrap), collapse="\n")
}
swr = Vectorize(swr)

cat_lda_tidy <- cat_lda_tidy %>% mutate(Interpretation = swr(Interpretation))

dput(unique(cat_lda_tidy$Interpretation))

cat_lda_tidy$Interpretation <- factor(cat_lda_tidy$Interpretation, levels=dput(unique(cat_lda_tidy$Interpretation)))


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

ggsave("OUT/URBACTreports_LDA_DTM015_K22.pdf", width = 11.7, height = 8.3, units = "in")

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



LdaDisjonct <- Lda_GammaDoc %>% mutate_at(.vars = vars(2:ncol(Lda_GammaDoc)), ~ifelse(.>5,1,0))

# Prepare info on good practice
URBACT_project_text <- URBACT_project_text %>% left_join(Politics, by = c("LeadPartnerCountry" = "iso_a2"))

TableCorrespondanceTopic$topic <- as.character(TableCorrespondanceTopic$topic)

names(TopicColor) <- TableCorrespondanceTopic$Interpretation

URBACT_project_text <- URBACT_project_text %>% mutate(Code_Network = as.numeric(Code_Network))
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
    left_join(select(DfInfoDoc,document = Code_Network, !!as.name(cat))) %>% 
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
RegionPlot <- gammatopbycat(Lda_GammaDoc, cat = "Region", DfInfoDoc = URBACT_project_text, InterpretationTopic = TableCorrespondanceTopic, TopicColor )

# SizeClassPlot <- gammatopbycat(Lda_GammaDoc, cat = "SizeClassURBACT", DfInfoDoc = URBACT_project_text, InterpretationTopic = TableCorrespondanceTopic, TopicColor )
# 
# SizeClassPlot2 <- gammatopbycat(Lda_GammaDoc, cat = "ClassePop", DfInfoDoc = URBACT_project_text, InterpretationTopic = TableCorrespondanceTopic, TopicColor )

PoliticsPlot1 <- gammatopbycat(Lda_GammaDoc, cat = "LocGovType_MunicipalAdmin", DfInfoDoc = URBACT_project_text, InterpretationTopic = TableCorrespondanceTopic, TopicColor )

PoliticsPlot2 <- gammatopbycat(Lda_GammaDoc, cat = "LocGovType_HorizontalPwrRelation", DfInfoDoc = URBACT_project_text, InterpretationTopic = TableCorrespondanceTopic, TopicColor )

PoliticsPlot3 <- gammatopbycat(Lda_GammaDoc, cat = "LocGovType_VerticalPwrRelation", DfInfoDoc = URBACT_project_text, InterpretationTopic = TableCorrespondanceTopic, TopicColor )

PoliticsPlot4 <- gammatopbycat(Lda_GammaDoc, cat = "LocGovType_PoliticalLeadership", DfInfoDoc = URBACT_project_text, InterpretationTopic = TableCorrespondanceTopic, TopicColor )

# poiitic4 withou ceremonial mayors (IE)