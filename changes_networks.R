
## Load libraries

library(tidyverse)
library(readxl)
library(igraph)
library(GGally)
library(network)
library(intergraph)
library(RColorBrewer)
library(ggdendro)
library(sna)

## Load the commuting ata

data_dir <- "./data/Commuting 2000-2017/"

my_data <- data_frame(files = list.files(data_dir)) %>%
  filter(!grepl(".ini", files)) %>%
  pull(files) %>%
  map(., ~ paste0(data_dir, .)) %>%
  map(., ~ read_excel(., sheet = NULL)) %>%
  map(., ~ set_names(., c("origin", "destination", "fullcode", "commuters"))) %>%
  map2_dfr(., list.files(data_dir), 
           ~ mutate(.x, year = .y)) %>%
  separate(year, c("year", "foo"), sep = 4) %>%
  select(-foo)

## Load the GPS data

gps_data <- read_excel("./data/Municipalities2017.xlsx")

## Build the networks

commute_threshold <- 0

all_networks <- my_data %>%
  split(.$year) %>%
  map(., ~ select(., origin, destination, weight = commuters)) %>%
  map(., ~ filter(., weight > commute_threshold)) %>%
  # map(., ~ mutate(., color = log(weight - 100))) %>%
  map(., ~ graph_from_data_frame(., vertices = gps_data))

my_data %>%
  filter(year == 2018) %>%
  filter(commuters > 50) %>%
  select(origin, destination, weight = commuters) %>%
  graph_from_data_frame(vertices = gps_data) %>%
  ggnet2(.,
         # mode = as.matrix(select(gps_data, Y_C, X_C)),
         # mode = "fruchtermanreingold",
         mode = c("Y_C", "X_C"),
         # label = "MUNICIPAL",
         label = FALSE,
         size = 3, arrow.size = 2, arrow.gap = 0.01,
         # edge.color = "color",
         edge.size = sqrt(E(.)$weight/3000)
         # edge.color = "weight"
  )

## Plot all the networks

# aa <- all_networks %>%
#   map(., ~ ggnet2(., mode = "fruchtermanreingold",
#                   label = FALSE, size = 3, arrow.size = 5, arrow.gap = .01,
#                   # edge.color = "color",
#                   edge.size = 1, edge.alpha = 1))
# 
# 
# 
# cowplot::plot_grid(plotlist = aa, labels = 2000:2020)
# 
# cowplot::plot_grid(aa[[1]], aa[[17]], labels = c(2000, 2017))

## Figure 1

aa <- all_networks %>%
  # map(., ~ igraph::delete.vertices(., degree(., loops = FALSE) == 0)) %>%
  # map(., ~ igraph::simplify(.)) %>%
  # E(.)$weight
  imap(., ~ggnet2(.x,
                 # mode = as.matrix(select(gps_data, Y_C, X_C)),
                 # mode = "fruchtermanreingold",
                 mode = c("Y_C", "X_C"),
                 # label = "MUNICIPAL",
                 label = FALSE,
                 size = 1, arrow.size = 2, arrow.gap = .01,
                 # edge.color = "color",
                 edge.size = E(.)$weight/1000, edge.alpha = 1
                 # edge.color = "weight"
                 ) +
         ggtitle(.y)
  )

all_networks[[17]] %>%
  ggnet2(.,
         # mode = as.matrix(select(gps_data, Y_C, X_C)),
         # mode = "fruchtermanreingold",
         mode = c("Y_C", "X_C"),
         # label = "MUNICIPAL",
         label = FALSE,
         size = 1, arrow.size = 2, arrow.gap = .01,
         # edge.color = "color",
         edge.size = E(.)$weight/10000, edge.alpha = 1
         # edge.color = "weight"
  )

cowplot::plot_grid(plotlist = aa)

## Some global indexes

all_networks %>%
  map_dfr(edge_density) %>%
  gather(year, density) %>%
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(x = year, y = density)) +
    geom_line() + geom_point() +
    geom_vline(xintercept = 2008, linetype = 2, colour = "red") +
    geom_text(aes(x = 2008.5, y = 0.3, label = "2008"), colour = "red")

all_networks %>%
  purrr::map(., ~ igraph::degree_distribution(., cumulative = TRUE, loops = FALSE, mode = "in")) %>%
  # purrr::map(plot)
  purrr::imap_dfr(., ~ data.frame(degree = 1:length(.)-1, cum_degree = .x, year = .y)) %>%
  ggplot(aes(x = degree, y = cum_degree, colour = year)) +
    geom_line() +
    # geom_smooth(method = "lm", se = FALSE) +
    scale_y_log10() +
    ylab("P(degree > x)")

all_indexes <- all_networks %>%
  imap_dfr(., ~ data_frame(year = .y, 
                           n_edges = length(E(.)),
                           n_vertices = length(V(.)),
                           assortativity = assortativity_degree(.),
                           mutuality = mutuality(asNetwork(.)),
                           diameter = diameter(.),
                           diameter_nw = diameter(., weights = NA),
                           e_density = edge_density(.),
                           efficiency = efficiency(asNetwork(.)),
                           hierarchy_recip = sna::hierarchy(asNetwork(., measure = "reciprocity")),
                           hierarchy_krack = sna::hierarchy(asNetwork(., measure = "krackhardt")),
                           lubness = lubness(asNetwork(.)),
                           m_distance = mean_distance(.),
                           reciprocity = reciprocity(.),
                           transitivity = transitivity(.),
                           max_out_degree = max(igraph::degree(., mode = "out")),
                           max_in_degree = max(igraph::degree(., mode = "in")),
                           connectedness = connectedness(asNetwork(.))
  ))

all_indexes %>% 
  gather(index, value, -year) %>%
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line() + geom_point() +
  facet_wrap("index", scales = "free_y") +
  geom_vline(xintercept = 2008, linetype = 2, colour = "red")


## Clustering

my_clusters <- all_networks %>%
  purrr::map(., ~ cluster_walktrap(., steps = 4))

# my_clusters %>%
#   purrr::map(., ~ data_frame(vertex = .$names, cluster = .$membership)) %>%
#   imap_dfr(., ~ mutate(.x, year = .y)) %>%
#   group_by(year, cluster) %>%
#   summarize(n = n()) %>% 
#   filter(n > 1) %>%
#   ggplot(aes(x = as.numeric(year), y = n, colour = factor(cluster))) +
#     geom_line() + geom_point()

clustered_networks <- map2(all_networks, my_clusters,
     ~ set_vertex_attr(graph = .x, name = "community", value = .y$membership))

aa <- clustered_networks %>%
  # map(., ~ igraph::delete.vertices(., degree(., loops = FALSE) == 0)) %>%
  # map(., ~ igraph::simplify(.)) %>%
  # E(.)$weight
  purrr::map(., ~ggnet2(., 
                 # mode = as.matrix(select(gps_data, Y_C, X_C)),
                 # mode = "fruchtermanreingold",
                 mode = c("Y_C", "X_C"),
                 color = "community",
                 # label = "MUNICIPAL", 
                 label = FALSE,
                 size = 1, arrow.size = 2, arrow.gap = .01, 
                 # edge.color = "color", 
                 edge.size = E(.)$weight/1000, edge.alpha = 1
                 # edge.color = "weight"
  )) 

# cowplot::plot_grid(plotlist = aa, labels = 2000:2020)

## Name the clusters

clustered_networks %>%
  map(., ~ data_frame(municipality = V(.)$MUNICIPAL,
                      community = V(.)$community)
      ) %>%
  imap_dfr(., ~ mutate(.x, year = .y)) %>%
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(x = year, y = community, colour= municipality)) +
    geom_point() + geom_line() +
    theme(legend.position = "none")
  

clustered_networks %>%
  map(., ~ data_frame(municipality = V(.)$MUNICIPAL,
                      community = V(.)$community,
                      Y_C = V(.)$Y_C,
                      X_C = V(.)$X_C)
  ) %>%
  imap_dfr(., ~ mutate(.x, year = .y)) %>%
  mutate(year = as.numeric(year)) %>%
  group_by(community, year) %>%
  summarise(m_X = mean(Y_C), m_Y = mean(X_C), n = n()) %>%
  filter(n > 1) %>%
  ggplot() +
    geom_text(aes(x = m_X, y = m_Y, label = community # size = n, 
                  # colour = factor(community)
                  ), 
              alpha = 1) +
    facet_wrap("year")
    
# cluster_map <- data_frame(
#   cluster = 1:9,
#   y_2000 = c("Ljubljana (61)", "Murska Sobota (80)", "Slovenj Gradec (112)", "Celje (11)", "Maribor (70)", "Nazarje (83)", NA, NA, NA),
#   y_2001 = c("Ljubljana (61)", "Murska Sobota (80)", "Slovenj Gradec (112)", "Celje (11)", "Maribor (70)", "Nazarje (83)", NA, NA, NA),
#   y_2002 = c("Ljubljana (61)", "Murska Sobota (80)", "Slovenj Gradec (112)", "Coastal-Kartz", "Celje (11)", "Maribor (70)", "Tolmin (128)", "Nazarje (83)", "Novo mesto (85)"),
#   y_2003 = c("Ljubljana (61)", "Murska Sobota (80)", "Coastal-Kartz", "Slovenj Gradec (112)", "Celje (11)", "Maribor (70)", "Tolmin (128)", "Nazarje (83)", "Novo mesto (85)"),
#   y_2004 = c("Ljubljana (61)", "Coastal-Kartz", "Murska Sobota (80)", "Slovenj Gradec (112)", "Celje (11)", "Maribor (70)", "Nazarje (83)", "Tolmin (128)", NA),
#   y_2005 = c("Ljubljana (61)", "Coastal-Kartz", "Murska Sobota (80)", "Slovenj Gradec (112)", "Celje (11)", "Nazarje (83)", "Maribor (70)", "Tolmin (128)", NA),
#   y_2006 = c("Ljubljana (61)", "Coastal-Kartz", "Slovenj Gradec (112)", "Murska Sobota (80)", "Nazarje (83)", "Celje (11)", "Maribor (70)", "Tolmin (128)", NA),
#   y_2007 = c("Nova Gorica (84)", "Ljubljana (61)", "Murska Sobota (80)", "Slovenj Gradec (112)", "Maribor (70)", "Nazarje (83)", "Celje (11)", NA, NA),
#   y_2008 = c("Nazarje (83)", "Nova Gorica (84)", "Ljubljana (61)", "Novo mesto (85)", "Slovenj Gradec (112)", "Murska Sobota (80)", "Maribor (70)", "Celje (11)", NA),
#   y_2009 = c("Nazarje (83)", "Ljubljana (61)", "Slovenj Gradec (112)", "Murska Sobota (80)", "Novo mesto (85)", "Maribor (70)", "Celje (11)", "Tolmin (128)", "Coastal-Kartz"),
#   y_2010 = c("Nova Gorica (84)", "Nazarje (83)", "Ljubljana (61)", "Slovenj Gradec (112)", "Murska Sobota (80)", "Maribor (70)", "Celje (11)", "Novo mesto (85)", NA),
#   y_2011 = c("Nazarje (83)", "Ljubljana (61)", "Slovenj Gradec (112)", "Murska Sobota (80)", "Maribor (70)", "Novo mesto (85)", "Celje (11)", "Tolmin (128)", "Coastal-Kartz"),
#   y_2012 = c("Celje (11)", "Ljubljana (61)", "Slovenj Gradec (112)", "Novo mesto (85)", "Murska Sobota (80)", "Maribor (70)", "Tolmin (128)", "Coastal-Kartz", NA),
#   y_2013 = c("Celje (11)", "Ljubljana (61)", "Novo mesto (85)", "Slovenj Gradec (112)", "Murska Sobota (80)", "Maribor (70)", "Tolmin (128)", "Coastal-Kartz", NA),
#   y_2014 = c("Celje (11)", "Ljubljana (61)", "Slovenj Gradec (112)", "Murska Sobota (80)", "Novo mesto (85)", "Maribor (70)", "Tolmin (128)", "Coastal-Kartz", NA),
#   y_2015 = c("Celje (11)", "Ljubljana (61)", "Novo mesto (85)", "Slovenj Gradec (112)", "Tolmin (128)", "Murska Sobota (80)", "Maribor (70)", "Coastal-Kartz", NA),
#   y_2016 = c("Celje (11)", "Ljubljana (61)", "Slovenj Gradec (112)", "Novo mesto (85)", "Murska Sobota (80)", "Tolmin (128)", "Maribor (70)", "Coastal-Kartz", NA),
#   y_2017 = c("Celje (11)", "Ljubljana (61)", "Slovenj Gradec (112)", "Novo mesto (85)", "Murska Sobota (80)", "Tolmin (128)", "Maribor (70)", "Coastal-Kartz", NA)
# ) %>%
#   gather(year, cluster_name, -cluster) %>%
#   separate(year, c("foo", "year")) %>%
#   select(-foo)

# cluster_map <- data_frame(
#   cluster = 1:9,
#   y_2000 = c("Ljubljana (61)", "Murska Sobota (80)", "Slovenj Gradec (112)", "Celje (11)", "Maribor (70)", "Nazarje (83)", NA, NA, NA),
#   y_2001 = c("Ljubljana (61)", "Murska Sobota (80)", "Slovenj Gradec (112)", "Celje (11)", "Maribor (70)", "Nazarje (83)", NA, NA, NA),
#   y_2002 = c("Ljubljana (61)", "Murska Sobota (80)", "Slovenj Gradec (112)", "Nova Gorica (84)", "Celje (11)", "Maribor (70)", "Tolmin (128)", "Nazarje (83)", "Novo mesto (85)"),
#   y_2003 = c("Ljubljana (61)", "Murska Sobota (80)", "Nova Gorica (84)", "Slovenj Gradec (112)", "Celje (11)", "Maribor (70)", "Tolmin (128)", "Nazarje (83)", "Novo mesto (85)"),
#   y_2004 = c("Ljubljana (61)", "Nova Gorica (84)", "Murska Sobota (80)", "Slovenj Gradec (112)", "Celje (11)", "Maribor (70)", "Nazarje (83)", "Tolmin (128)", NA),
#   y_2005 = c("Ljubljana (61)", "Nova Gorica (84)", "Murska Sobota (80)", "Slovenj Gradec (112)", "Celje (11)", "Nazarje (83)", "Maribor (70)", "Tolmin (128)", NA),
#   y_2006 = c("Ljubljana (61)", "Nova Gorica (84)", "Slovenj Gradec (112)", "Murska Sobota (80)", "Nazarje (83)", "Celje (11)", "Maribor (70)", "Tolmin (128)", NA),
#   y_2007 = c("Nova Gorica (84)", "Ljubljana (61)", "Murska Sobota (80)", "Slovenj Gradec (112)", "Maribor (70)", "Nazarje (83)", "Celje (11)", NA, NA),
#   y_2008 = c("Nazarje (83)", "Nova Gorica (84)", "Ljubljana (61)", "Novo mesto (85)", "Slovenj Gradec (112)", "Murska Sobota (80)", "Maribor (70)", "Celje (11)", NA),
#   y_2009 = c("Nazarje (83)", "Ljubljana (61)", "Slovenj Gradec (112)", "Murska Sobota (80)", "Novo mesto (85)", "Maribor (70)", "Celje (11)", "Tolmin (128)", "Nova Gorica (84)"),
#   y_2010 = c("Nova Gorica (84)", "Nazarje (83)", "Ljubljana (61)", "Slovenj Gradec (112)", "Murska Sobota (80)", "Maribor (70)", "Celje (11)", "Novo mesto (85)", NA),
#   y_2011 = c("Nazarje (83)", "Ljubljana (61)", "Slovenj Gradec (112)", "Murska Sobota (80)", "Maribor (70)", "Novo mesto (85)", "Celje (11)", "Tolmin (128)", "Nova Gorica (84)"),
#   y_2012 = c("Celje (11)", "Ljubljana (61)", "Slovenj Gradec (112)", "Novo mesto (85)", "Murska Sobota (80)", "Maribor (70)", "Tolmin (128)", "Nova Gorica (84)", NA),
#   y_2013 = c("Celje (11)", "Ljubljana (61)", "Novo mesto (85)", "Slovenj Gradec (112)", "Murska Sobota (80)", "Maribor (70)", "Tolmin (128)", "Nova Gorica (84)", NA),
#   y_2014 = c("Celje (11)", "Ljubljana (61)", "Slovenj Gradec (112)", "Murska Sobota (80)", "Novo mesto (85)", "Maribor (70)", "Tolmin (128)", "Nova Gorica (84)", NA),
#   y_2015 = c("Celje (11)", "Ljubljana (61)", "Novo mesto (85)", "Slovenj Gradec (112)", "Tolmin (128)", "Murska Sobota (80)", "Maribor (70)", "Nova Gorica (84)", NA),
#   y_2016 = c("Celje (11)", "Ljubljana (61)", "Slovenj Gradec (112)", "Novo mesto (85)", "Murska Sobota (80)", "Tolmin (128)", "Maribor (70)", "Nova Gorica (84)", NA),
#   y_2017 = c("Celje (11)", "Ljubljana (61)", "Slovenj Gradec (112)", "Novo mesto (85)", "Murska Sobota (80)", "Tolmin (128)", "Maribor (70)", "Nova Gorica (84)", NA)
# ) %>%
#   gather(year, cluster_name, -cluster) %>%
#   separate(year, c("foo", "year")) %>%
#   select(-foo)

cluster_map <- data_frame(
  cluster = 1:9,
  y_2000 = c("Ljubljana (61)", "Murska Sobota (80)", "Slovenj Gradec (112)", "Celje (11)", "Maribor (70)", "Nazarje (83)", NA, NA, NA),
  y_2001 = c("Ljubljana (61)", "Murska Sobota (80)", "Slovenj Gradec (112)", "Celje (11)", "Maribor (70)", "Nazarje (83)", NA, NA, NA),
  y_2002 = c("Ljubljana (61)", "Murska Sobota (80)", "Slovenj Gradec (112)", "Koper (50)", "Celje (11)", "Maribor (70)", "Tolmin (128)", "Nazarje (83)", "Crnomelj (17)"),
  y_2003 = c("Ljubljana (61)", "Murska Sobota (80)", "Koper (50)", "Slovenj Gradec (112)", "Celje (11)", "Maribor (70)", "Tolmin (128)", "Nazarje (83)", "Crnomelj (17)"),
  y_2004 = c("Ljubljana (61)", "Koper (50)", "Murska Sobota (80)", "Slovenj Gradec (112)", "Celje (11)", "Maribor (70)", "Nazarje (83)", "Tolmin (128)", NA),
  y_2005 = c("Ljubljana (61)", "Koper (50)", "Murska Sobota (80)", "Slovenj Gradec (112)", "Celje (11)", "Nazarje (83)", "Maribor (70)", "Tolmin (128)", NA),
  y_2006 = c("Ljubljana (61)", "Koper (50)", "Slovenj Gradec (112)", "Murska Sobota (80)", "Nazarje (83)", "Celje (11)", "Maribor (70)", "Tolmin (128)", NA),
  y_2007 = c("Big Nova Gorica (84)", "Ljubljana (61)", "Murska Sobota (80)", "Slovenj Gradec (112)", "Maribor (70)", "Nazarje (83)", "Celje (11)", NA, NA),
  y_2008 = c("Nazarje (83)", "Big Nova Gorica (84)", "Ljubljana (61)", "Novo mesto (85)", "Slovenj Gradec (112)", "Murska Sobota (80)", "Maribor (70)", "Celje (11)", NA),
  y_2009 = c("Nazarje (83)", "Ljubljana (61)", "Slovenj Gradec (112)", "Murska Sobota (80)", "Novo mesto (85)", "Maribor (70)", "Celje (11)", "Tolmin (128)", "Nova Gorica (84)"),
  y_2010 = c("Big Nova Gorica (84)", "Nazarje (83)", "Ljubljana (61)", "Slovenj Gradec (112)", "Murska Sobota (80)", "Maribor (70)", "Celje (11)", "Novo mesto (85)", NA),
  y_2011 = c("Nazarje (83)", "Ljubljana (61)", "Slovenj Gradec (112)", "Murska Sobota (80)", "Maribor (70)", "Novo mesto (85)", "Celje (11)", "Tolmin (128)", "Nova Gorica (84)"),
  y_2012 = c("Celje (11)", "Ljubljana (61)", "Slovenj Gradec (112)", "Novo mesto (85)", "Murska Sobota (80)", "Maribor (70)", "Tolmin (128)", "Nova Gorica (84)", NA),
  y_2013 = c("Celje (11)", "Ljubljana (61)", "Novo mesto (85)", "Slovenj Gradec (112)", "Murska Sobota (80)", "Maribor (70)", "Tolmin (128)", "Nova Gorica (84)", NA),
  y_2014 = c("Celje (11)", "Ljubljana (61)", "Slovenj Gradec (112)", "Murska Sobota (80)", "Novo mesto (85)", "Maribor (70)", "Tolmin (128)", "Nova Gorica (84)", NA),
  y_2015 = c("Celje (11)", "Ljubljana (61)", "Novo mesto (85)", "Slovenj Gradec (112)", "Tolmin (128)", "Murska Sobota (80)", "Maribor (70)", "Nova Gorica (84)", NA),
  y_2016 = c("Celje (11)", "Ljubljana (61)", "Slovenj Gradec (112)", "Novo mesto (85)", "Murska Sobota (80)", "Tolmin (128)", "Maribor (70)", "Nova Gorica (84)", NA),
  y_2017 = c("Celje (11)", "Ljubljana (61)", "Slovenj Gradec (112)", "Novo mesto (85)", "Murska Sobota (80)", "Tolmin (128)", "Maribor (70)", "Nova Gorica (84)", NA),
  y_2018 = c("Celje (11)", "Ljubljana (61)", "Slovenj Gradec (112)", "Novo mesto (85)", "Tolmin (128)", "Murska Sobota (80)", "Maribor (70)", "Nova Gorica (84)", NA),
  y_2019 = c("Celje (11)", "Ljubljana (61)", "Slovenj Gradec (112)", "Novo mesto (85)", "Murska Sobota (80)", "Tolmin (128)", "Maribor (70)", "Nova Gorica (84)", NA),
  y_2020 = c("Maribor (70)", "Big Nova Gorica (84)", "Ljubljana (61)", "Slovenj Gradec (112)", "Murska Sobota (80)",NA,NA,NA,NA)
) %>%
  gather(year, cluster_name, -cluster) %>%
  separate(year, c("foo", "year")) %>%
  select(-foo)

# cluster_map <- tibble(cluster_name = unique(cluster_map$cluster_name),
#        cluster_color = brewer.pal(13, "Set3")) %>%
#   left_join(., cluster_map)

clustered_networks <- clustered_networks %>%
  map(., ~ V(.)) %>%
  map(., ~ data_frame(cluster = .$community)) %>%
  map2(., split(cluster_map, cluster_map$year),
       ~ full_join(.x, .y)) %>%
  map(., ~ .$cluster_name) %>%
  map2(., clustered_networks, 
       ~ set_vertex_attr(.y, "community_name", value = .x))

clustered_networks <- clustered_networks %>%
  map(., ~ V(.)) %>%
  map(., ~ data_frame(cluster = .$community)) %>%
  map2(., split(cluster_map, cluster_map$year),
       ~ full_join(.x, .y)) %>%
  map(., ~ .$cluster_color) %>%
  map2(., clustered_networks, 
       ~ set_vertex_attr(.y, "community_color", value = .x))

clustered_networks %>%
  map(., ~ data_frame(municipality = V(.)$MUNICIPAL,
                      community = V(.)$community_name,
                      Y_C = V(.)$Y_C,
                      X_C = V(.)$X_C)
  ) %>%
  imap_dfr(., ~ mutate(.x, year = .y)) %>%
  mutate(year = as.numeric(year)) %>%
  group_by(community, year) %>%
  summarise(m_X = mean(Y_C), m_Y = mean(X_C), n = n()) %>%
  filter(n > 1) %>%
  ggplot() +
  geom_text(aes(x = m_X, y = m_Y, label = community, # size = n, 
                colour = community)
  , 
  alpha = 1) +
  facet_wrap("year")

# aa <- clustered_networks %>%
#   # map(., ~ igraph::delete.vertices(., degree(., loops = FALSE) == 0)) %>%
#   # map(., ~ igraph::simplify(.)) %>%
#   # E(.)$weight
#   purrr::map(., ~ggnet2(., 
#                         # mode = as.matrix(select(gps_data, Y_C, X_C)),
#                         # mode = "fruchtermanreingold",
#                         mode = c("Y_C", "X_C"),
#                         color = V(.)$community_color,
#                         # label = "MUNICIPAL", 
#                         label = FALSE,
#                         size = 1, arrow.size = 2, arrow.gap = .01, 
#                         # edge.color = "color", 
#                         edge.size = E(.)$weight/1000, edge.alpha = 1,
#                         # edge.color = "weight",
#                         palette = "Set3"
#   )) 
# 
# cowplot::plot_grid(plotlist = aa, labels = 2000:2017)

# op <- par(mfrow = c(5, 4))
# 
# clustered_networks %>%
#   map(., ~ igraph::delete.vertices(., igraph::degree(., loops = TRUE) == 0)) %>%
#   map(simplify) %>%
#   map(., ~ plot(.,
#                 vertex.color = V(.)$community_color,
#                 layout = as.matrix(data.frame(V(.)$Y_C, V(.)$X_C)),
#                 vertex.label = NA,
#                 vertex.size = 3,
#                 edge.width = 1,
#                 edge.arrow.size = .1
#                 )
#       )
# 
# par(op)

p <- clustered_networks %>%
  map(., ~ tibble(x = V(.)$Y_C,
             y = V(.)$X_C,
             cluster_name = V(.)$community_name
             )) %>%
  imap_dfr(., ~ mutate(.x, year = .y)) %>%
  na.omit() %>%
  ggplot() +
    geom_point(aes(x = x, y = y, colour = cluster_name)) +
    facet_wrap("year") +
    scale_x_continuous(labels = NULL, name = "") + scale_y_continuous(labels = NULL, name = "") +
    theme_void() + 
    theme(legend.position = "bottom",
          legend.title = element_blank())

p

`plotly::ggplotly(p)
## Analysis of colapsed networks

# colapsed_nets <- clustered_networks %>%
#   purrr::map(., ~ contract(., V(.)$community))
# 
# simple_colapsed <- colapsed_nets %>%
#   purrr::map(., ~igraph::delete.vertices(., igraph::degree(., loops = TRUE) == 0)) %>%
#   purrr::map(., ~simplify(., remove.loops = TRUE)) 

actual_clusters <- clustered_networks %>%
  map(., ~ V(.)$community) %>%
  map(table) %>%
  map(as.numeric) %>%
  map(., ~ c(1:9)[. > 1])

colapsed_nets <- map2(clustered_networks, actual_clusters,
     ~ igraph::delete.vertices(.x, !(V(.x)$community %in% .y))) %>%
  map(., ~ contract(., V(.)$community))

# colapsed_nets %>%
#   map(simplify) %>%
#   map(., ~ plot(., vertex.label = NA))
  
# aa <- contract(clustered_networks$`2000`, V(clustered_networks$`2000`)$community)
# bb <- contract(clustered_networks$`2008`, V(clustered_networks$`2008`)$community)
# 
# aa %>% V() %>% length()
# bb %>% V() %>% length()
# 
# igraph::degree(aa) %>% unname()
# igraph::degree(bb) %>% unname()
# 
# colapsed_nets %>%
#   map(V) %>%
#   map_dbl(length)
# 
# simple_colapsed %>%
#   map(V) %>%
#   map_dbl(length)
# 
# cluster_map %>% 
#   split(.$year) %>%
#   map(na.omit) %>%
#   map_dbl(nrow)
# 
# aa - bb
################

colapsed_nets <- cluster_map %>% 
  split(.$year) %>%
  map(., ~ arrange(., cluster)) %>%
  map(na.omit) %>%
  map2(., colapsed_nets,
       ~ igraph::set_vertex_attr(.y, "community_name", value = .x$cluster_name)
       )

# colapsed_nets %>%
#   map(simplify) %>%
#   map(., ~ plot(., 
#                 vertex.label = V(.)$community_name,
#                 layout = layout.fruchterman.reingold
#                 ))

op <- par(mfrow = c(3, 7),
          oma = c(3, 3, 0, 0), 
          mar = c(1, 1, 1, 1))
colapsed_nets %>%
  # purrr::map(., ~igraph::delete.vertices(., igraph::degree(., loops = TRUE) == 0)) %>%
  purrr::map(., ~simplify(., remove.loops = TRUE)) %>%
  purrr::map2(., c(2000:2020), ~ plot(., vertex.label = V(.)$community_name, 
       layout = layout.fruchterman.reingold,
       # layout = layout.circle,
       edge.curved = FALSE, edge.arrow.size = .1,
       edge.width = E(.)$weight/1000,
       main = .y
       ))
par(op)

## Net fluxes between clusters (one year)

aa <- colapsed_nets[[18]] %>%
  simplify() %>%
  as_data_frame(., what = "vertices") %>%
  mutate(name = as.character(name))

colapsed_nets[[18]] %>%
  simplify() %>%
  as_data_frame(., what = "edges") %>% 
  mutate(from = as.character(from), to = as.character(to)) %>% 
  full_join(., aa, by = c("from" = "name")) %>%
  rename(community_from = community_name) %>%
  full_join(., aa, by = c("to" = "name")) %>%
  rename(community_to = community_name) %>%
  select(-from, -to) %>%
  ggplot(aes(x = community_from, y = community_to)) +
    geom_tile(aes(fill = log(weight))) +
    geom_text(aes(label = weight))

aa <- colapsed_nets[[18]] %>%
  simplify() %>%
  as_data_frame(., what = "edges") %>% 
  mutate(from = as.character(from), to = as.character(to)) %>% 
  full_join(., aa, by = c("from" = "name")) %>%
  rename(community_from = community_name) %>%
  full_join(., aa, by = c("to" = "name")) %>%
  rename(community_to = community_name) %>%
  select(-from, -to)

aa %>%
  set_names(c("other_way", "community_to", "community_from")) %>%
  full_join(., aa) %>%
  rename(one_way = weight) %>%
  mutate(difference = one_way - other_way) %>%
  ggplot(aes(x = community_to, y = community_from)) +
    geom_tile(aes(fill = difference), colour = "grey") +
    geom_text(aes(label = difference)) +
    scale_fill_gradient2() +
    xlab("Arrives to") + ylab("Leaves") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Subgraphs

aa <- clustered_networks[[1]] %>%
  igraph::delete.vertices(., igraph::degree(., loops = FALSE) == 0)

op <- par(mfrow = c(3, 3),
          oma = c(3, 3, 0, 0), 
          mar = c(1, 1, 1, 1))
1:max(V(aa)$community) %>%
  purrr::map(., ~ induced_subgraph(aa, V(aa)$name[V(aa)$community == .])) %>%
  purrr::map(., ~ igraph::simplify(.)) %>%
  purrr::map(.,
              ~ plot(., vertex.label = NA,
                     vertex.size = 1,
                     edge.arrow.size = .0,
                     layout = layout.fruchterman.reingold,
                     # layout = layout.circle,
                     edge.curved = TRUE,
                     edge.width = E(.)$weight/500,
                     main = V(.)$community_name[1]))

  # purrr::map2(., V(aa)$community_name %>% unique(),
  #             ~ plot(.x, vertex.label = NA,
  #                    vertex.size = 1,
  #                    edge.arrow.size = .0,
  #                    layout = layout.fruchterman.reingold,
  #                    # layout = layout.circle,
  #                    edge.curved = TRUE,
  #                    edge.width = E(.)$weight/500,
  #                    main = .y))

par(op)
# induced_subgraph(aa, V(aa)$name[V(aa)$community == 6]) %>% plot()

get_subnetworks <- function(my_net) {

  subnets <- c(1:max(V(my_net)$community)) %>%
    map(., ~ delete_vertices(my_net, V(my_net)$community != .)
    )

  indexes <- subnets %>%
    map_lgl(., ~ length(V(.)) > 1)

  subnets[indexes]

}

all_subnetworks <- clustered_networks %>%
  map(get_subnetworks)

all_subnetworks %>%
  map(length)


all_subindexes <- lapply(all_subnetworks, function(x) {
  x %>%
    imap_dfr(., ~ data_frame(cluster = .y,
                             # n_edges = length(E(.)),
                             n_vertices = length(V(.)),
                             # assortativity = assortativity_degree(.),
                             # mutuality = mutuality(asNetwork(.)),
                             # diameter = diameter(.),
                             # diameter_nw = diameter(., weights = NA),
                             e_density = edge_density(.),
                             efficiency = efficiency(asNetwork(.)),
                             hierarchy_recip = sna::hierarchy(asNetwork(.), measure = "reciprocity"),
                             # hierarchy_krack = sna::hierarchy(asNetwork(.), measure = "krackhardt"),
                             # lubness = lubness(asNetwork(.)),
                             # m_distance = mean_distance(.),
                             reciprocity = reciprocity(.),
                             transitivity = transitivity(.)
                             # max_out_degree = max(igraph::degree(., mode = "out")),
                             # max_in_degree = max(igraph::degree(., mode = "in")),
                             # connectedness = connectedness(asNetwork(.))
    ))

}) %>%
  imap_dfr(., ~ mutate(.x, year = .y)) %>%
  full_join(cluster_map) #%>%
  # na.omit()

all_subindexes %>%
  mutate(year = as.numeric(year)) %>%
  select(-cluster) %>%
  gather(index, value, -cluster_name, -year)%>%
  filter(is.na(value)) %>%
  group_by(cluster_name, index) %>%
  summarize(n())

index_map <- tibble(index = c("n_vertices", "mutuality", "e_density", "efficiency",
                              "hierarchy_recip", "m_distance", "reciprocity", "transitivity"),
                    index_name = c("Number of vertices", "Mutuality", "Edge density", "Efficiency",
                                   "Hierarchy", "Mean distance", "Reciprocity", "Transitivity")
                    )

all_subindexes %>%
  na.omit() %>%
  mutate(year = as.numeric(year)) %>%
  select(-cluster) %>%
  gather(index, value, -cluster_name, -year) %>%
  left_join(., index_map) %>%
  ggplot(aes(x = year, y = value, colour = cluster_name)) +
    geom_point() + geom_line() +
    facet_wrap("index_name", scales = "free_y") +
    ylab("") + xlab("Year") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title = element_blank())


all_subindexes %>%
  na.omit() %>%
  filter(year == 2019) %>%
  select(-cluster) %>%
  gather(index, value, -cluster_name, -year) %>%
  ggplot() +
    geom_point(aes(x = cluster_name, y = value)) +
    facet_wrap("index", scales = "free_y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Cluster of the indexes

my_hclust <- all_subindexes %>%
  na.omit() %>%
  mutate(cluster_year = paste(cluster_name, year, sep = ": ")) %>%
  column_to_rownames("cluster_year") %>%
  select(-cluster, -year, -cluster_name
         # -connectedness, -hierarchy_krack, -diameter_nw, -lubness
         ) %>%
  scale(., center = TRUE, scale = TRUE) %>%
  dist(., method = "euclidean") %>%
  hclust(., method = "ward.D")

aa <- dendro_data(as.dendrogram(my_hclust))

bb <- aa$labels %>% 
  separate(label, c("region", "year"), sep = ": ", remove = FALSE)

ggplot() +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
               data = aa$segments, colour = "black", size = 1) +
  geom_text(aes(label = label, y = -.5, x = x, colour = region), data = bb,
            angle = 90, hjust = 1, size = 3) +
  geom_hline(yintercept = 30, linetype = 2, colour = "red") +
  # ylim(-1000, 700) +
  # scale_color_gradientn(colours = brewer.pal(5, "Spectral")) +
  # theme_dark() +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(name = "", breaks = NULL) +
  geom_label(aes(x = c(0, 50, 100, 140), y = rep(30, 4), 
                label = c("Ljubljana", "North-east", "Periphery I", "Periphery II")), 
             inherit.aes = FALSE) +
  
  ylab("") +
  ylim(-75, 100) #+
  # scale_y_continuous(name = "Euclidean distance between the indexes",
  #                    limits = c(-800, 700), breaks = c(0, 350, 700)) +
  # coord_flip()

ggplot() +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
               data = aa$segments, colour = "black", size = 1) +
  geom_text(aes(label = label, y = -.1, x = x, colour = region), data = bb,
            angle = 90, hjust = 1, size = 3) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(name = "", breaks = NULL) +
  geom_label(aes(x = c(9.5, 46, 93, 129), y = rep(-60, 4), 
                 label = c("Ljubljana", "North-east", "Periphery I", "Periphery II")), 
             inherit.aes = FALSE, size = 5) +
  geom_rect(aes(xmin = 0, ymin = -70, xmax = 19, ymax = 30), 
            fill = NA, colour = "red", linetype = 2) +
  geom_rect(aes(xmin = 19, ymin = -70, xmax = 73, ymax = 30), 
            fill = NA, colour = "red", linetype = 2) +
  geom_rect(aes(xmin = 73, ymin = -70, xmax = 113, ymax = 30), 
            fill = NA, colour = "red", linetype = 2) +
  geom_rect(aes(xmin = 113, ymin = -70, xmax = 145, ymax = 30), 
            fill = NA, colour = "red", linetype = 2) +
  
  ylab("") +
  ylim(-75, 201)

## PCA of the indexes

my_pca <- all_subindexes %>%
  na.omit() %>%
  mutate(cluster_year = paste(cluster_name, year, sep = ": ")) %>%
  column_to_rownames("cluster_year") %>%
  select(-cluster, -year, -cluster_name
         # -connectedness, -hierarchy_krack, -diameter_nw, -lubness
         ) %>%
  # summary()
  scale(., center = TRUE, scale = TRUE) %>%
  as.matrix() %>%
  prcomp(., scale = TRUE, center = TRUE)

my_pca$x %>%
  as_tibble(rownames = "cluster_year") %>%
  separate(cluster_year, c("cluster", "year"), sep = ": ", remove = FALSE) %>%
  ggplot(aes(x = PC1, y = PC2, colour = cluster)) +
    geom_point() +
    geom_text(aes(label = year))

## Mean indexes of the clusters

aa <- cutree(my_hclust, k = 4) %>% 
  tibble(cluster_year = names(.), cluster_hier = .) 

all_subindexes %>%
  mutate(cluster_year = paste(cluster_name, year, sep = ": ")) %>%
  full_join(., aa) %>% 
  na.omit() %>%
  select(-cluster, -year, -cluster_name, -cluster_year) %>%
  gather(index, value, -cluster_hier) %>%
  left_join(., index_map) %>%
  ggplot() +
    geom_boxplot(aes(x = factor(cluster_hier), y = value)) +
    facet_wrap("index_name", scales = "free") +
    scale_x_discrete(labels = c("Ljubljana", "North-east", "Periphery I", "Periphery II"),
                     name = "") +
    theme_bw() +
    ylab("")

# all_subindexes %>%
#   mutate(cluster_year = paste(cluster_name, year, sep = ": ")) %>%
#   full_join(., aa) %>% 
#   na.omit() %>%
#   select(-cluster, -year, -cluster_name, -cluster_year) %>%
#   gather(index, value, -cluster_hier) %>%
#   group_by(index, cluster_hier) %>%
#   summarize(m_value = median(value)) %>%
#   ggplot() +
#     geom_point(aes(x = cluster_hier, y = m_value)) +
#     facet_wrap("index", scales = "free")


