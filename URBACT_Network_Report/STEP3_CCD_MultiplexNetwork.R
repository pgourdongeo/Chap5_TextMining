###############################  CCD project multinet ##################################
#                               
#                          
# DESCRIPTION : Création et analyse du multigraph des relations 
#entre les villes partenaires du projet CCD
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
library(multinet)
library(GGally)
library(stargazer)

## Data 


CCD_cities <- read.csv("~/Chap5_TextMining/URBACT_Network_Report/DataProd/MetadataCortext_CCD_LAP_Cities.csv", stringsAsFactors = F, sep="\t")

MultinetCCD <- read.csv2("~/Chap5_TextMining/Data/URBACT/MultiNetEdges_CCD.csv")

## test data

netml <- ml_florentine()
plot(netml)

 l <- layout_multiforce_ml(netml, w_inter = 0, gravity = 1)
 bk <- par("mar")
par(mar=c(0,0,0,0)) 
plot(netml, layout = l, grid = c(1, 2), vertex.labels = "",
                         legend.x = "bottomright", legend.inset = c(.05, .05))
 
par(mar=bk)
 
 summary(netml)
 layer_comparison_ml(netml, method = "jeffrey.degree")
 
 layer_comparison_ml(netml, method = "jaccard.actors")
 
 
 # Create igraph objects (layers for multigraph)
 # graph with label
Namedf <- CCD_cities%>% select(geonameId, asciiName)


MultinetCCDName <- MultinetCCD %>% left_join(select(Namedf, Source = geonameId, Name1 = asciiName)) %>%
                   left_join(select(Namedf, Target = geonameId, Name2 = asciiName)) %>% select(-Source, -Target)%>%
   rename(Source = Name1, Target = Name2) %>% relocate(Source, Target)

 
# similarity declared
SimilDeclared <- graph_from_data_frame(MultinetCCDName %>% filter(Type == "Similarity"), directed = TRUE)

# Contacts
Contacts <- graph_from_data_frame(MultinetCCDName %>% filter(Type == "Contact"), directed = TRUE)

# study visits
Visits <-  graph_from_data_frame(MultinetCCDName %>% filter(Type == "StudyVisit"), directed = TRUE)

# semantic proximity (cosine_het) inverse weight following scarcity of concepts in common
SemanticField2 <- graph_from_data_frame(MultinetCCDName %>% filter(Type == "SemanticK2Field2"), directed = FALSE)

E(SemanticField2)$weight <- E(SemanticField2)$Weight 

# semantic proximity (cosine_het) concepts in common
Semantic <- graph_from_data_frame(MultinetCCDName %>% filter(Type == "SemanticK2" ), directed = FALSE)

E(Semantic)$weight <- E(Semantic)$Weight 


### create the multigraph


multiCCD_g <- ml_empty()

add_igraph_layer_ml(multiCCD_g, SimilDeclared, "SimilDeclared")
add_igraph_layer_ml(multiCCD_g, Contacts, "Contacts")
add_igraph_layer_ml(multiCCD_g, Visits, "Visits")
add_igraph_layer_ml(multiCCD_g, SemanticField2, "SemanticField2")
add_igraph_layer_ml(multiCCD_g, Semantic, "Semantic")

write_ml(multiCCD_g, "DataProd/multinetCCD", format = "multilayer", 
         sep = ',')


multiCCD_Align <- read_ml("DataProd/multinetCCD", aligned = TRUE)

## plot network

l <- layout_multiforce_ml(multiCCD_Align, w_inter = 0, gravity = 1)
bk <- par("mar")
par(mar=c(0,0,0,0)) 
plot(multiCCD_Align, layout = l, grid = c(2, 3),
     legend.x = "bottomright", legend.inset = c(.05, .05) )


# attr_values <- get_values_ml(multiCCD_Align, vertices = 50,
#                               attribute = "layer")
# vertex_layer <- as.factor(attr_values[[1]])
#  num_distinct_roles <- length(levels(vertex_roles))
# levels(vertex_roles)

# par(mar=bk)


## summary and comparison
summary(multiCCD_g)
sumMulti <- summary(multiCCD_Align)
sumMulti

# all degree
layer_comparison_ml(multiCCD_Align, method = "jeffrey.degree")

AllJacEdge <- layer_comparison_ml(multiCCD_Align, method = "jaccard.edges")


AllSMEdge <- layer_comparison_ml(multiCCD_Align, method = "sm.edges")

AllHamEdge <- layer_comparison_ml(multiCCD_Align, method = "hamann.edges")


AllJacTrian <- layer_comparison_ml(multiCCD_Align, method = "sm.triangles")

layer_comparison_ml(multiCCD_Align, method = "pearson.degree")

AllJacEdge <- AllJacEdge %>% mutate_all(~round(., 2))
stargazer(AllJacEdge, summary = FALSE, digit.separator = " ")
library(kableExtra)

kable(AllJacEdge, booktabs = T, "latex", align = "c") %>% 
   kable_styling(position = "center", latex_options = "striped")
# in-degree

layer_comparison_ml(multiCCD_Align, method = "jeffrey.degree", mode = "in")

dfinEdgeJacc <- layer_comparison_ml(multiCCD_Align, method = "jaccard.edges", mode = "in")

layer_comparison_ml(multiCCD_Align, method = "pearson.degree", mode = "in")


# out-degree

layer_comparison_ml(multiCCD_Align, method = "jeffrey.degree", mode = "out")

dfoutEdgeJacc <- layer_comparison_ml(multiCCD_Align, method = "jaccard.edges", mode = "out")

layer_comparison_ml(multiCCD_Align, method = "pearson.degree", mode = "out")

# highest-degree actors on the whole multiplex network

deg <- degree_ml(multiCCD_Align, actors = CCD_cities$asciiName)
names(deg) <- CCD_cities$asciiName


degdf <-data.frame( City = names(deg),
                    SimilDeclared_in = degree_ml(multiCCD_Align, actors = names(deg), layers = "SimilDeclared", mode = "in"),
                    SimilDeclared_out = degree_ml(multiCCD_Align, actors = names(deg), layers = "SimilDeclared", mode = "out"),
                  Contacts_in = degree_ml(multiCCD_Align, actors = names(deg), layers = "Contacts", mode = "in"),
                  Contacts_out = degree_ml(multiCCD_Align, actors = names(deg), layers = "Contacts", mode = "out"),
                  Visits_out = degree_ml(multiCCD_Align, actors = names(deg), layers = "Visits", "out"),
                  Visits_in = degree_ml(multiCCD_Align, actors = names(deg), layers = "Visits", "in"),
                  SemanticField2 = degree_ml(multiCCD_Align, actors = names(deg), layers = "SemanticField2"),
                  Semantic = degree_ml(multiCCD_Align, actors = names(deg), layers = "Semantic"),
   flat = deg)


degdf <- degdf %>% arrange(desc(flat))%>% rowwise()  %>% 
   mutate(allIn =  sum(c_across(ends_with("in")),na.rm = TRUE),
         allOut =  sum(c_across(ends_with("out")),na.rm = TRUE)) %>%
   ungroup()


degdfRank <- degdf %>% mutate_if(is.numeric, list(rank= ~rank(desc(.), ties.method = "min")))
ggpairs(degdfRank %>% select(ends_with("rank"))%>% select(7:11))

ggpairs(degdf[,8:12])
### relevance of actors base on layers

relevance <-data.frame(City = names(deg), SimilDeclared_in = relevance_ml(multiCCD_Align, actors = names(deg), layers = "SimilDeclared", mode = "in"),
                    Contacts_in = relevance_ml(multiCCD_Align, actors = names(deg), layers = "Contacts", mode = "in"),
                    Visits_out = relevance_ml(multiCCD_Align, actors = names(deg), layers = "Visits", "out"),
                    Visits_in = relevance_ml(multiCCD_Align, actors = names(deg), layers = "Visits", "in"),
                    SemanticField2 = relevance_ml(multiCCD_Align, actors = names(deg), layers = "SemanticField2"),
                    Semantic = relevance_ml(multiCCD_Align, actors = names(deg), layers = "Semantic"))


xrelevance <-data.frame(City = names(deg), SimilDeclared_in = xrelevance_ml(multiCCD_Align, actors = names(deg), layers = "SimilDeclared", mode = "in"),
                       Contacts_in = xrelevance_ml(multiCCD_Align, actors = names(deg), layers = "Contacts", mode = "in"),
                       Contacts_out = xrelevance_ml(multiCCD_Align, actors = names(deg), layers = "Contacts", mode = "out"),
                       Contact_All = xrelevance_ml(multiCCD_Align, actors = names(deg), layers = "Contacts"),
                       Visits_out = xrelevance_ml(multiCCD_Align, actors = names(deg), layers = "Visits", "out"),
                       Visits_in = xrelevance_ml(multiCCD_Align, actors = names(deg), layers = "Visits", "in"),
                       SemanticField2 = xrelevance_ml(multiCCD_Align, actors = names(deg), layers = "SemanticField2"),
                       Semantic = xrelevance_ml(multiCCD_Align, actors = names(deg), layers = "Semantic"))



### community detection


ml_clust <-  mdlp_ml(multiCCD_g)
