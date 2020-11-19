###############################   URBACT    ##################################
#                               
#                          
# DESCRIPTION : Lexique et mots-clés des textes institutionnels
#
# 
############################################################################## PG aout 2020

setwd("~/Chap5_TextMining/URBACT_Keywords_goodpractices/")

# Packages
library(tidyverse)
library(udpipe)
library(readr)
library(lattice)
library(scales)
library(tidytext)

# Data

ProjectURBACT <- read.csv2("~/Chap5_TextMining/Data/URBACT/UrbactNetworks_complete.csv", 
                           stringsAsFactors = F)


LeipzigCHarter <- read_file("~/Chap5_TextMining/Data/URBACT/InstitutionalTexts/LeipzigCharter_2007.txt")


Objetctives0713 <- read_file("~/Chap5_TextMining/Data/URBACT/InstitutionalTexts/Art6_EuropeanTerritorialCooperation.txt")





##Language model

ud_model <- udpipe_download_model(language = "english-partut")
ud_model <- udpipe_load_model(ud_model)



### ==== LeipzigCharter ======

LeipzigCHarter <- tolower(LeipzigCHarter)

LeipzigCHarter <- str_replace_all(LeipzigCHarter, regex("\\W+"), " ")

x <- udpipe_annotate(ud_model, x = LeipzigCHarter)
x2 <- as.data.frame(x)


####### Lexique et taille des documents

stats <- txt_freq(x2$upos)
ggplot(stats) +
  geom_bar(aes(x = reorder(key, -freq_pct), y = freq_pct), stat = "identity") +
  coord_flip() + theme_bw()

# nouns
stats <- subset(x2, upos %in% c("NOUN")) 
stats <- txt_freq(stats$lemma) 
ggplot(stats[1:20, ]) +
  geom_bar(aes(x = reorder(key, freq_pct), y = freq_pct), stat = "identity") +
  coord_flip() + theme_bw() + labs(x = "Noms", y = "Fréquence (%)")

## adjectives
stats <- subset(x2, upos %in% c("ADJ")) 
stats <- txt_freq(stats$lemma)
ggplot(stats[1:20, ]) +
  geom_bar(aes(x = reorder(key, freq_pct), y = freq_pct), stat = "identity") +
  coord_flip() + theme_bw() + labs(x = "Adjectifs", y = "Fréquence (%)")

## Verb
stats <- subset(x2, upos %in% c("VERB")) 
stats <- txt_freq(stats$lemma)
ggplot(stats[1:20, ]) +
  geom_bar(aes(x = reorder(key, freq_pct), y = freq_pct), stat = "identity") +
  coord_flip() + theme_bw() +  labs(x = "Verbes", y = "Fréquence (%)")




################ Key words

## Using RAKE

stats <- keywords_rake(x = x2, term = "lemma", group = "sentence_id", 
                       relevant = x2$upos %in% c("NOUN", "ADJ"), ngram_max = 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 1), 20), col = "cadetblue", 
         main = "Keywords identified by RAKE", 
         xlab = "Rake",
         ylab=list(label="Keywords", cex=1))


## Using Pointwise Mutual Information Collocations
# remove stop words
data(stop_words)
x3 <- x2  %>%  filter(!token %in% stop_words$word)
  
stats <- keywords_collocation(x = x3, term = "token", group = "sentence_id", ngram_max = 4, n_min = 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(subset(stats, freq > 3), 20), col = "cadetblue", 
         main = "Keywords identified by PMI Collocation", 
         xlab = "PMI (Pointwise Mutual Information)")

statsTop <- stats %>% top_n(10, freq)
ggplot(statsTop) +
  geom_bar(aes(x = reorder(key, freq), y = freq), stat = "identity") +
  coord_flip() + theme_bw() +  labs(x = "Keywords identified by PMI Collocation", y = "Fréquence")
### ==== Objective ERDF Territorial coop 2007-2013 ======

Objetctives0713 <- tolower(Objetctives0713)



x <- udpipe_annotate(ud_model, x = Objetctives0713)
x2 <- as.data.frame(x)


####### Lexique et taille des documents

stats <- txt_freq(x2$upos)
ggplot(stats) +
  geom_bar(aes(x = reorder(key, -freq_pct), y = freq_pct), stat = "identity") +
  coord_flip() + theme_bw()

# nouns
stats <- subset(x2, upos %in% c("NOUN")) 
stats <- txt_freq(stats$lemma) 
ggplot(stats[1:20, ]) +
  geom_bar(aes(x = reorder(key, freq_pct), y = freq_pct), stat = "identity") +
  coord_flip() + theme_bw() + labs(x = "Noms", y = "Fréquence (%)")

## adjectives
stats <- subset(x2, upos %in% c("ADJ")) 
stats <- txt_freq(stats$lemma)
ggplot(stats[1:20, ]) +
  geom_bar(aes(x = reorder(key, freq_pct), y = freq_pct), stat = "identity") +
  coord_flip() + theme_bw() + labs(x = "Adjectifs", y = "Fréquence (%)")

## Verb
stats <- subset(x2, upos %in% c("VERB")) 
stats <- txt_freq(stats$lemma)
ggplot(stats[1:20, ]) +
  geom_bar(aes(x = reorder(key, freq_pct), y = freq_pct), stat = "identity") +
  coord_flip() + theme_bw() +  labs(x = "Verbes", y = "Fréquence (%)")




################ Key words

## Using RAKE

stats <- keywords_rake(x = x2, term = "lemma", group = "doc_id", 
                       relevant = x2$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 1), 15), col = "cadetblue", 
         main = "Keywords identified by RAKE", 
         xlab = "Rake",
         ylab=list(label="Keywords", cex=1))





