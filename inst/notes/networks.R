# Day 2: Networks

# This tutorial covers the basics of network analysis and visualization in {igraph}
# Install and load a new tool:
install.packages("igraph")
library(igraph)
# What is igraph? https://igraph.org/r/
# Load out old tools:
library(tidyverse)

# 1. Exploring network graphs --------------------------------------------------
# Before working with a data set, get aquaninted with {igraph}
(g <- graph_from_literal(a---b, b---c))
# What is the object?
class(g)
typeof(g)
# Read printout:
# A. Igraph id
# B. Types UN vs DN
# C. How many nodes? How many edges?
vcount(g)
V(g)
ecount(g)
E(g)
# Plot it:
plot(g) # Yours may not look the same
# Spruce it up:
plot(g,
     # Tune vertex (node) characteristics
     vertex.color = "gold", vertex.size = 15, vertex.label.color = "black",
     # Tune edge characteristics
     edge.color = "grey", edge.curved = 0.5)

# Try a directed graph
(g <- graph_from_literal(a--+b, b--+c, c+-+d, b--+d))
# Read printout
plot(g)

# Add attributes to the network, vertices, or edges
V(g)$name # automatically generated when we created the network
V(g)$gender <- c("male", "male", "female", "female") # categorical
V(g)$age <- c(30, 32, 35, 28) # continious
E(g)$type <- c("email", "call", "call", "call", "email")
# Check out those attribures
edge_attr(g)
vertex_attr(g)
g

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
g %>%
  get.data.frame()

g %>%
  get.data.frame(what = "vertices")

# 2. From data.frame to graph --------------------------------------------------
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

# 3. Working with real data ----------------------------------------------------
# Edges ========================================================================
# Primary data set: World Bank Group Finances - Major Contract Awards
# https://finances.worldbank.org/Procurement/Major-Contract-Awards/kdui-wcs3

# You may download the data and read it from CSV using read_csv() from {readr}.
awards <- readr::read_csv("data/Major_Contract_Awards.csv")

# Explore the data:
dim(awards)
str(awards, nchar = 30)
summary(awards)

# Exercise 1. Using the data set can you identify:
# How many networks?
# How can we make an edgelist with this data set?
# Would this data be directed or undirected?
#   - Who points at whom?

# Country to Country network:
awards %>%
  select(`Supplier Country`, `Borrower Country`)
# Are all nodes "valid"?
table(awards$`Supplier Country`)
# Remove "World"
awards %>% 
  select(`Supplier Country`, `Borrower Country`) %>%
  filter(!`Supplier Country` %in% c("World", "Western Africa", "Eastern Africa", "Africa") | !`Borrower Country` %in% c("World", "Western Africa", "Eastern Africa", "Africa"))
# Since we have 247,279 rows, filter it down to the last five whole years
awards %>% 
  filter(!`Supplier Country` %in% c("World", "Western Africa", "Eastern Africa", "Africa") | !`Borrower Country` %in% c("World", "Western Africa", "Eastern Africa", "Africa"),
         `Fiscal Year` >= 2015 & `Fiscal Year` <=2020) %>%
  select(`Supplier Country`, `Borrower Country`)
  
# Read graph_from_data_frame() documentation for 'd': ?graph_from_data_frame()
# Make it a graph:
(g <- awards %>%
    filter(!`Supplier Country` %in% c("World", "Western Africa", "Eastern Africa", "Africa") & !`Borrower Country` %in% c("World", "Western Africa", "Eastern Africa", "Africa"),
           `Fiscal Year` >= 2015 & `Fiscal Year` <=2020) %>%
  select(`Supplier Country`, `Borrower Country`) %>%
  graph_from_data_frame())
plot(g)

# Clean it up:
g %>%
  plot(vertex.label = NA,
       vertex.size = 5,
       edge.arrow.size = 0.25,
       layout = layout_with_fr)

# What patterns do you see? I see lots of self-loops...
# Simplify
(g <- g %>%
  igraph::simplify(remove.loops = TRUE, remove.multiple = FALSE))
g %>%
  plot(vertex.label = NA,
       vertex.size = 5,
       edge.arrow.size = 0.25,
       layout = layout_with_fr)
# What other patterns do you see? Hubs of lending... But who are they?
(g <- g %>%
  set_vertex_attr(name = "out_degree", value = degree(., mode = "out")))
g %>%
  get.data.frame(., what = "vertices") %>%
  arrange(desc(out_degree)) %>%
  head()
g %>%
  plot(vertex.label = NA,
       vertex.size = scales::rescale(vertex_attr(., name = "out_degree"),
                                     to = c(3, 15)),
       edge.arrow.size = 0.25,
       layout = layout_with_fr)

# Who receive the most? In degree
(g <- g %>%
    set_vertex_attr(name = "in_degree", value = degree(., mode = "in")))
g %>%
  get.data.frame(., what = "vertices") %>%
  arrange(desc(in_degree)) %>%
  head()
g %>%
  plot(vertex.label = NA,
       vertex.size =  scales::rescale(vertex_attr(., name = "in_degree"),
                                      to = c(3, 15)),
       edge.arrow.size = 0.25,
       layout = layout_with_fr)
# Who might be the potential brokers? Betweenness
(g <- g %>%
    set_vertex_attr(name = "betweenness",
                    value = betweenness(., normalized = TRUE)))
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
make_ego_graph(g, order = 1, nodes = V(g)$name == "India")[[1]] %>%
  plot(vertex.label.size = 0.5,
       vertex.label = ifelse(V(.)$name == "India", "India", NA),
       vertex.color = ifelse(V(.)$name == "India", "red", "orange"),
       vertex.size = scales::rescale(vertex_attr(., name = "betweenness"), to = c(1, 5)),
       edge.arrow.size = 0.25,
       layout = layout_with_fr)
# Automate this
awards %>%
  # data.frame
  filter(!`Supplier Country` %in% c("World", "Western Africa", "Eastern Africa", "Africa") & !`Borrower Country` %in% c("World", "Western Africa", "Eastern Africa", "Africa"),
         `Fiscal Year` >= 2015 & `Fiscal Year` <=2020) %>%
  select(`Supplier Country`, `Borrower Country`) %>%
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

awards %>%
  # data.frame
  filter(!`Supplier Country` %in% c("World", "Western Africa", "Eastern Africa", "Africa") & !`Borrower Country` %in% c("World", "Western Africa", "Eastern Africa", "Africa"),
         `Fiscal Year` >= 2015 & `Fiscal Year` <=2020) %>%
  select(`Supplier Country`, `Borrower Country`) %>%
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
  filter(!`Supplier Country` %in% c("World", "Western Africa", "Eastern Africa", "Africa") & !`Borrower Country` %in% c("World", "Western Africa", "Eastern Africa", "Africa"),
         `Fiscal Year` >= 2015 & `Fiscal Year` <=2020) %>%
  select(`Supplier Country`, `Borrower Country`, `Total Contract Amount (USD)`) %>%
  group_by(`Supplier Country`, `Borrower Country`) %>% 
  summarize(total = sum(`Total Contract Amount (USD)`)) 
# data.frame to igraph
(g <- awards %>%
  filter(!`Supplier Country` %in% c("World", "Western Africa", "Eastern Africa", "Africa") & !`Borrower Country` %in% c("World", "Western Africa", "Eastern Africa", "Africa"),
         `Fiscal Year` >= 2015 & `Fiscal Year` <=2020) %>%
  select(`Supplier Country`, `Borrower Country`, `Total Contract Amount (USD)`) %>%
  group_by(`Supplier Country`, `Borrower Country`) %>% 
  summarize(total = sum(`Total Contract Amount (USD)`)) %>%
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
  filter(!`Supplier Country` %in% c("World", "Western Africa", "Eastern Africa", "Africa") & !`Borrower Country` %in% c("World", "Western Africa", "Eastern Africa", "Africa"),
         `Fiscal Year` >= 2000 & `Fiscal Year` <=2000) %>%
  select(`Supplier Country`, `Borrower Country`, `Total Contract Amount (USD)`) %>%
  group_by(`Supplier Country`, `Borrower Country`) %>% 
  summarize(total = sum(`Total Contract Amount (USD)`)) %>%
  graph_from_data_frame() %>%
  igraph::simplify(remove.multiple = FALSE, remove.loops = TRUE) %>%
  set_edge_attr(name = "edge_betweenness",
                value = edge_betweenness(., weights = E(.)$total)) %>%
  get.data.frame(what = "edges") %>% 
  arrange(desc(edge_betweenness))
 
