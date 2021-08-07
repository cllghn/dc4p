# Day 2: Networks

# This tutorial covers the basics of network analysis and visualization in {igraph}
# Install and load a new tool:
# install.packages("igraph")
library(igraph)
# What is igraph? https://igraph.org/r/
# Load out old tools:
library(tidyverse)

# 1. Exploring network graphs --------------------------------------------------
# Before working with a data set, get acquainted with {igraph}
# igraph produces objects class igraph, one way to make one:
?graph_from_literal
(g <- graph_from_literal(a---b, b---c))
# What is the object?
class(g)
typeof(g)
# Read printout:
# A. Igraph id
# B. Types UN vs DN
# C. How many nodes? How many edges?
vcount(g)
# Inspect vertices
?V
V(g)
# How many edges?
ecount(g)
# Inspect edges:
?E
E(g)
# Plot it:
plot(g) # Yours may not look the same
# Spruce it up:
?plot.igraph
plot(g,
     # Tune vertex (node) characteristics
     vertex.color = "gold", vertex.size = 15, vertex.label.color = "black",
     # Tune edge characteristics
     edge.color = "grey", edge.curved = 0.5)
# Exercise 1. Look at list of parameters "Drawing graphs {igraph}", play around with the look of your graph

# Try a directed graph
(g <- graph_from_literal(a--+b, b--+c, c+-+d, b--+d))
# Read printout
plot(g)

# Add attributes to the network, vertices, or edges
V(g)$name # automatically generated when we created the network
?set_vertex_attr
(g <- g %>%
  set_vertex_attr(name = "gender",
                  value = c("male", "male", "female", "female"))) # categorical
(g <- g %>%
    set_vertex_attr(name = "gender",
                    value = c("male", "male", "female", "female")) %>%
    set_vertex_attr(name = "age",
                    value = c(0, 32, 35, 28))) # continious
# Edge attributes
?set_edge_attr
(g <- g %>%
    set_vertex_attr(name = "gender",
                    value = c("male", "male", "female", "female")) %>%
    set_vertex_attr(name = "age",
                    value = c(0, 32, 35, 28)) %>%
    set_edge_attr(name = "type",
                  value = c("email", "call", "call", "call", "email")))
# Check out those attributes
edge_attr(g)
vertex_attr(g)

# Use {igraph}'s algorithms:
degree(g, mode = "total")
degree(g, mode = "in")
degree(g, mode = "out")
betweenness(g)
# Assign them as attributes:
(g <- g %>%
  set_vertex_attr(name = "in_degree", value = degree(., mode = "in")))
# Use these measures in plots:
g %>%
  plot(vertex.size = vertex_attr(., name = "in_degree") * 10) # . 
# Explort data to data.frame:
?get.data.frame
g %>%
  get.data.frame()

g %>%
  get.data.frame(what = "vertices")

# 2. From data.frame to graph --------------------------------------------------
# Each observation is a relationship
(el <- data.frame(from = c("Mike", "Rob", "Mike"), 
                  to   = c("Chris", "Chris", "Rob"),
                  type = "friends"))
(g <- el %>%
  graph_from_data_frame(directed = FALSE))
plot(g)

(nl <- data.frame(name = c("Mike", "Rob", "Chris"),
                  core = c(7, 11, 3)))
g <- el %>%
  graph_from_data_frame(directed = FALSE, vertices = nl)
g %>%
  plot(vertex.size = vertex_attr(., name = "core") * 10)
# Exercise 2. Create an edge list of relationships and a node list for class members...


# 3. Working with real data ----------------------------------------------------
# Edges ========================================================================
# Primary data set: World Bank Group Finances - Major Contract Awards
# https://finances.worldbank.org/Procurement/Major-Contract-Awards/kdui-wcs3

# You may download the data and read it from CSV using read_csv() from {readr}.
awards <- read_csv("data/Major_Contract_Awards.csv") %>%
  janitor::clean_names()

# Explore the data:
dim(awards)
str(awards, nchar = 30)
summary(awards)

# Exercise 3. Using the data set can you identify. Who should be ties to whom and how?
# How many networks?
# How can we make an edgelist with this data set?
# Would this data be directed or undirected?
#   - Who points at whom?

# Country to Country network:
awards %>%
  select(supplier_country, borrower_country)
# Are all nodes "valid"? Are they all countries or do we have groups?
awards %>%
  group_by(supplier_country) %>%
  tally() %>%
  View()
# Remove "World", "Western Africa", "Eastern Africa", "Africa"
# codes <- c("5M", "6R", "7E", "4P", "6O", "7B", "8S", "6C", "7C", "7X", "6L", "3S", "SOthers", "SOthersK")
to_remove <- c("World", "Western Africa", "Eastern Africa", "Africa", "Caribbean",
            "Caucasus", "Central America", "Central Asia", "Europe and Cent", 
            "Latin America", "Middle East and", "OECS Countries", 
            "Pacific Islands", "South Asia", "Southern Africa", "St Maarten",
            "Western Balkans", "MeOthersico")
awards %>% 
  select(supplier_country, borrower_country) %>%
  filter(!supplier_country %in% to_remove | !borrower_country %in% to_remove)
# Since we have 247,141 rows, filter it down to the last five whole years
awards %>% 
  filter(!supplier_country %in% to_remove | !borrower_country %in% to_remove,
         fiscal_year >= 2015 & fiscal_year <=2020)
# Now let's make it into a clean edgelist:
awards %>% 
  filter(!supplier_country %in% to_remove | !borrower_country %in% to_remove,
         fiscal_year >= 2015 & fiscal_year <=2020) %>%
  select(supplier_country, borrower_country)
# Read graph_from_data_frame() documentation for 'd': ?graph_from_data_frame()
# Make it a graph:
(g <- awards %>%
    filter(!supplier_country %in% to_remove & !borrower_country %in% to_remove,
           fiscal_year >= 2015 & fiscal_year <=2020) %>%
  select(supplier_country, borrower_country) %>%
  graph_from_data_frame())
# Now plot it!
plot(g)

# Clean it up:
g %>%
  plot(
    # remove names = less cluter
    vertex.label = NA,
    # makes vertexes smaller
    vertex.size = 5,
    # make the arrow size and width smaller
    edge.arrow.size = 0.25,
    edge.arrow.width = 0.25,
    # pick a layout ?layout_with_fr or ?layout_with_kk
    layout = layout_with_fr)
# What patterns do you see? I see lots of self-loops... remove those.

# Simplify
?simplify
(g <- g %>%
  igraph::simplify(remove.loops = TRUE,
                   # Keep multiple edges
                   remove.multiple = FALSE))
g %>%
  plot(vertex.label = NA,
       vertex.size = 5,
       edge.arrow.size = 0.25,
       layout = layout_with_fr)
# What other patterns do you see? Hubs of lending... But who are they?
?degree
(g <- g %>%
  set_vertex_attr(name = "out_degree", value = degree(., mode = "out")))
# Pull out the dataframe and arrange it
g %>%
  get.data.frame(., what = "vertices") %>%
  # Now arrange it
  arrange(desc(out_degree)) %>%
  head()
# Visualize it
g %>%
  plot(vertex.label = NA,
       vertex.size = scales::rescale(vertex_attr(., name = "out_degree"),
                                     to = c(3, 15)),
       edge.arrow.size = 0.25,
       layout = layout_with_fr) # try multiple layouts

# Who receive the most? In degree
?degree
# Add in-degree to the graph object
(g <- g %>%
    set_vertex_attr(name = "in_degree", value = degree(., mode = "in")))
# Pull out the dataframe and arrange it
g %>%
  get.data.frame(., what = "vertices") %>%
  arrange(desc(in_degree)) %>%
  head()
# Plot it
g %>%
  plot(vertex.label = NA,
       vertex.size =  scales::rescale(vertex_attr(., name = "in_degree"),
                                      to = c(3, 15)),
       edge.arrow.size = 0.25,
       layout = layout_with_fr)
# Who might be the potential brokers? Betweenness
?betweenness
(g <- g %>%
    set_vertex_attr(name = "betweenness",
                    value = betweenness(., normalized = TRUE)))
# Pull out the dataframe and arrange it
g %>%
  get.data.frame(., what = "vertices") %>%
  arrange(desc(betweenness)) %>%
  head()
# Where is India on the graph?
g %>%
  plot(vertex.label = NA,
       vertex.color = if_else(V(g)$name == "India", "red", "orange"),
       vertex.size = scales::rescale(vertex_attr(., name = "betweenness"), to = c(1, 5)),
       edge.arrow.size = 0.25,
       layout = layout_with_fr)
# Ego graph: a graph about a nodes, its connections (alters) and it's alters connections to each other
?make_ego_graph
make_ego_graph(g, order = 1, nodes = V(g)$name == "India")[[1]] %>%
  plot(vertex.label.size = 0.5,
       vertex.label = ifelse(V(.)$name == "India", "India", NA),
       vertex.color = ifelse(V(.)$name == "India", "red", "orange"),
       vertex.size = scales::rescale(vertex_attr(., name = "betweenness"), to = c(1, 5)),
       edge.arrow.size = 0.25,
       layout = layout_with_fr)
# Pull out the dataframe and arrange it
make_ego_graph(g, order = 1, nodes = V(g)$name == "India")[[1]] %>%
  get.data.frame(., what = "vertices") %>%
  # Select variables that remain constant (degree)
  select(name, out_degree, in_degree) 

# Automate this: put is all in one command
awards %>%
  # data.frame
  filter(!supplier_country %in% to_remove & !borrower_country %in% to_remove,
         fiscal_year >= 2015 & fiscal_year <=2020) %>%
  select(supplier_country, borrower_country) %>%
  graph_from_data_frame() %>%
  # igraph
  igraph::simplify(remove.multiple = FALSE, remove.loops = TRUE) %>%
  set_vertex_attr(name = "betweenness", value = betweenness(., normalized = FALSE)) %>%
  set_vertex_attr(name = "in_degree", value = degree(., mode = "in")) %>%
  set_vertex_attr(name = "out_degree", value = degree(., mode = "out")) %>%
  plot(vertex.label = NA,
       vertex.color = if_else(V(g)$name == "Mongolia", "red", "lightblue"),
       vertex.size = scales::rescale(vertex_attr(., name = "in_degree"),
                                     to = c(1, 7)),
       edge.arrow.size = 0.25,
       layout = layout_with_fr)

# Explort it:
awards %>%
  # data.frame
  filter(!supplier_country %in% to_remove & !borrower_country %in% to_remove,
         fiscal_year >= 2015 & fiscal_year <=2020) %>%
  select(supplier_country, borrower_country) %>%
  graph_from_data_frame() %>%
  # igraph
  igraph::simplify(remove.multiple = FALSE, remove.loops = TRUE) %>%
  set_vertex_attr(name = "betweenness", value = betweenness(., normalized = FALSE)) %>%
  set_vertex_attr(name = "in_degree", value = degree(., mode = "in")) %>%
  set_vertex_attr(name = "out_degree", value = degree(., mode = "out")) %>%
  get.data.frame(what = "vertices") %>%
  write_csv(file = "awards_node_score.csv")

# Edge attributes ==============================================================
# Until now, we have focused on the links between nations, but not the values of these (e.g., more money)
awards %>%
  filter(!supplier_country %in% to_remove & !borrower_country %in% to_remove,
         fiscal_year >= 2015 & fiscal_year <=2020) %>%
  select(supplier_country, borrower_country, total_contract_amount_usd) %>%
  group_by(supplier_country, borrower_country) %>% 
  summarize(total = sum(total_contract_amount_usd)) 
# data.frame to igraph
(g <- awards %>%
  filter(!supplier_country %in% to_remove & !borrower_country %in% to_remove,
         fiscal_year >= 2015 & fiscal_year <=2020) %>%
  select(supplier_country, borrower_country, total_contract_amount_usd) %>%
  group_by(supplier_country, borrower_country) %>% 
  summarize(total = sum(total_contract_amount_usd)) %>%
  graph_from_data_frame())

# What dyad has the highest edge betweenness?
g %>%
  igraph::simplify(remove.multiple = FALSE, remove.loops = TRUE) %>%
  set_edge_attr(name = "edge_betweenness",
                value = edge_betweenness(., weights = E(.)$total)) %>%
  get.data.frame(what = "edges") %>% # get a data.frame back
  arrange(desc(edge_betweenness))
# igraph plot
g %>%
  igraph::simplify(remove.multiple = FALSE, remove.loops = TRUE) %>%
  set_edge_attr(name = "edge_betweenness",
                value = edge_betweenness(., weights = E(.)$total)) %>%
  plot(vertex.label = NA,
       vertex.color = "lightblue",
       vertex.size = 2,
       edge.width = scales::rescale(edge_attr(., name = "edge_betweenness"),
                                    to = c(0.5, 4)),
       edge.arrow.size = 0.25,
       edge.arrow.width = 2,
       layout = layout_with_fr)

# What about time? Exercise 1. Filter down data to include awards from 2000 onwards. Rerun edge betweenness how does that change the analysis?
awards %>%
  filter(!supplier_country %in% to_remove & !borrower_country %in% to_remove,
         fiscal_year >= 2000 & fiscal_year <=2000) %>%
  select(supplier_country, borrower_country, total_contract_amount_usd) %>%
  group_by(supplier_country, borrower_country) %>% 
  summarize(total = sum(total_contract_amount_usd)) %>%
  graph_from_data_frame() %>%
  igraph::simplify(remove.multiple = FALSE, remove.loops = TRUE) %>%
  set_edge_attr(name = "edge_betweenness",
                value = edge_betweenness(., weights = E(.)$total)) %>%
  get.data.frame(what = "edges") %>% 
  arrange(desc(edge_betweenness))

# Node attributes ==============================================================
# Like before, let's wrangle together some data to create node attributes
# First read some data:
attributes <- read_csv(file = "data/codelist.csv") %>%
  janitor::clean_names()

attributes
# Remember we have a method for joining by keys, using the iso2 key.

# Look back at your edgelist
awards %>%
  filter(!supplier_country %in% to_remove & !borrower_country %in% to_remove,
         fiscal_year >= 2015 & fiscal_year <=2020) %>%
  select(supplier_country, borrower_country)
# We need to use the country codes:
awards %>%
  filter(!supplier_country %in% to_remove & !borrower_country %in% to_remove,
         fiscal_year >= 2015 & fiscal_year <=2020) %>%
  select(supplier_country_code, borrower_country_code)
# Remember the noise from the prior lecture:
el <- awards %>%
  # Recode
  mutate(supplier_country_code = case_when(supplier_country == "Central Africa" ~ "CF",
                                             supplier_country == "Congo, Democrat" ~ "CR",
                                             supplier_country == "Namibia" ~ "NA",
                                             supplier_country == "Netherlands Ant" ~ "NL",
                                             supplier_country == "Serbia" ~ "RS",
                                             supplier_country == "Timor-Leste" ~ "TL",
                                             supplier_country == "West Bank and G" ~ "PS",
                                             supplier_country == "Yemen, Republic" ~ "YE",
                                             TRUE ~ supplier_country_code),
         borrower_country_code = case_when(borrower_country == "Central Africa" ~ "CF",
                                           borrower_country == "Congo, Democrat" ~ "CR",
                                           borrower_country == "Namibia" ~ "NA",
                                           borrower_country == "Netherlands Ant" ~ "NL",
                                           borrower_country == "Serbia" ~ "RS",
                                           borrower_country == "Timor-Leste" ~ "TL",
                                           borrower_country == "West Bank and G" ~ "PS",
                                           borrower_country == "Yemen, Republic" ~ "YE",
                                           TRUE ~ borrower_country_code)) %>%
  # add our new processing code
  filter(!supplier_country %in% to_remove & !borrower_country %in% to_remove,
         fiscal_year >= 2015 & fiscal_year <=2020) %>%
  select(supplier_country_code, borrower_country_code)
el
# Now let's move on to an node list, which should have one observation per node:
# Create a vector of names
c(el$supplier_country_code, el$borrower_country_code)
# Now only keep unique codes
unique(c(el$supplier_country_code, el$borrower_country_code))
data.frame(id = unique(c(el$supplier_country_code, el$borrower_country_code)))
# Now join on key
nl <- data.frame(id = unique(c(el$supplier_country_code, el$borrower_country_code))) %>%
  left_join(attributes, by = c("id" = "iso2c"))
nl
# Maybe select
nl <- data.frame(id = unique(c(el$supplier_country_code, el$borrower_country_code))) %>%
  left_join(attributes, by = c("id" = "iso2c")) %>%
  select(id, iso_name_en, continent, region)
# Let's make a graph
g <- graph_from_data_frame(d = el, directed = TRUE, vertices = nl)
# Apply the treatment 
g %>%
  igraph::simplify(remove.loops = TRUE, remove.multiple = FALSE)
# Plot it:
g %>%
  igraph::simplify(remove.loops = TRUE, remove.multiple = FALSE) %>%
  plot(vertex.label = NA,
       vertex.color = "lightblue",
       vertex.size = 2,
       edge.width = scales::rescale(edge_attr(., name = "edge_betweenness"),
                                    to = c(0.5, 4)),
       edge.arrow.size = 0.25,
       edge.arrow.width = 2,
       layout = layout_with_fr)
# Add color, how many unique values?
table(vertex_attr(g, name = "continent"))
?as.factor
color_continents[as.factor(vertex_attr(g, name = "continent"))]
# Plot it with colors
g %>%
  igraph::simplify(remove.loops = TRUE, remove.multiple = FALSE) %>%
  plot(vertex.label = NA,
       vertex.color = color_continents[as.factor(vertex_attr(g, name = "continent"))],
       vertex.size = 2,
       edge.width = scales::rescale(edge_attr(., name = "edge_betweenness"),
                                    to = c(0.5, 4)),
       edge.arrow.size = 0.25,
       edge.arrow.width = 2,
       layout = layout_with_fr)

# Network of those who are EAP (east asia pacific)
table(nl$region)
# Which nodes are East Asia and Pacific
want <- nl %>%
  filter(region == "East Asia & Pacific")
el_eap <- el %>%
  filter(supplier_country_code %in% want$id | borrower_country_code %in% want$id)
nl_eap <- nl %>%
  filter(id %in% unique(c(el_eap$supplier_country_code, el_eap$borrower_country_code)))
g_eap <- graph_from_data_frame(d = el_eap, directed = TRUE, vertices = nl_eap)

g_eap %>%
  igraph::simplify(remove.loops = TRUE, remove.multiple = FALSE) %>%
  plot(vertex.label = NA,
       vertex.color = color_continents[as.factor(vertex_attr(g, name = "continent"))],
       vertex.size = 2,
       edge.width = scales::rescale(edge_attr(., name = "edge_betweenness"),
                                    to = c(0.5, 4)),
       edge.arrow.size = 0.25,
       edge.arrow.width = 2,
       layout = layout_with_fr)

# Who is most active in this network:
g_eap %>%
  set_vertex_attr(name = "out_degree", value = degree(., mode = "out")) %>%
  get.data.frame(what = "vertices") %>%
  arrange(desc(out_degree)) %>%
  head()
