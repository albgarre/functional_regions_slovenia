
library(tidyverse)
library(readxl)
library(igraph)
library(GGally)
library(network)
library(intergraph)
library(RColorBrewer)

## Load data

my_data <- read_excel("data/Commuting 2017.xlsx")

summary(my_data)

## Make network

n <- my_data %>%
  select(-ORIG_DEST) %>%
  filter(TOGETHER_17 > 100) %>%
  graph_from_data_frame() %>%
  asNetwork()

# my_data %>%
#   select(-ORIG_DEST) %>%
#   filter(TOGETHER_17 > 100) %>%
#   ggnet2(label = TRUE, size = 0, arrow.size = 5, arrow.gap = .01,
#          color = "TOGETHER_17")

n %e% "number" <- my_data %>%
  select(-ORIG_DEST) %>%
  filter(TOGETHER_17 > 100) %>%
  mutate(size = log(TOGETHER_17)/10) %>%
  .$size

my_colors <- brewer.pal(8, "Blues")

my_data %>%
  select(-ORIG_DEST) %>%
  filter(TOGETHER_17 > 100) %>%
  mutate(color = as.numeric(cut(log(TOGETHER_17), 8))) %>%
  .$color -> bb

n %e% "color" <- my_colors[bb]

n %e% "weight" <- my_data %>%
  select(-ORIG_DEST) %>%
  filter(TOGETHER_17 > 100) %>%
  .$TOGETHER_17

ggnet2(n, mode = "fruchtermanreingold",
       label = FALSE, size = 3, arrow.size = 5, arrow.gap = .01, 
       edge.color = "color", edge.size = 1, edge.alpha = 1)

## Hub and authority scores

  # authority_score %>% .$vector
  # hub_score() %>% .$vector
data.frame(
  hub = hub_score(asIgraph(n))$vector,
  auth = authority_score(asIgraph(n))$vector
) %>%
  mutate(id = 1:nrow(.)) %>%
  ggplot() +
    geom_text(aes(x = hub, y = auth, label = id)) +
    scale_x_log10() + scale_y_log10()

## Some indexes

n %>%
  asIgraph() %>%
  assortativity_degree()

n %>%
  asIgraph() %>%
  mean_distance(., directed = TRUE)

n %>%
  sna::hierarchy(., measure = "krackhardt")

n %>%
  sna::connectedness()

n %>%
  sna::efficiency()

n %>%
  sna::lubness()

## Clustering

n %>%
  asIgraph() %>%
  cluster_walktrap(., steps = 5, weights = NULL) -> aa

# plot(aa, asIgraph(n))
n %v% "cluster" <- aa$membership

data_frame(
  
  name = as.character(V(asIgraph(n))),
  cluster = aa$membership
  
) %>% View()

ggnet2(n, mode = "fruchtermanreingold",
       label = TRUE, # size = "indegree", 
       arrow.size = 5, arrow.gap = .01,
       node.color = "cluster",
       label.size = 2, label.color = "white")

# ggnet2(n, mode = "fruchtermanreingold",
#        label = FALSE, size = "outdegree", arrow.size = 5, arrow.gap = .01,
#        node.color = "cluster")
      
