#THIS SCRIPT IS LARGELY BASED ON JULIA SILGE'S & DAVID ROBINSON'S "TEXT MINING WITH R"


library(readtext)
library(tidytext)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(igraph)
library(ggraph)
library(widyr)


#REPUBLICANS
sotu.r <- readtext("*Republican.txt") %>%
  mutate(text = str_replace_all(text, pattern = "\\[.*?\\]", replacement =  "")) %>%
  mutate(text = str_replace_all(text, pattern = "\\(.*?\\)", replacement =  ""))

sotu.r_bigrams <- sotu.r %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(bigram, sort = T)

bigrams.r_separated <- sotu.r_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams.r_filtered <- bigrams.r_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram.r_count <- bigrams.r_filtered %>%
  count(word1, word2, sort = T)

#network of bigrams
bigram.r_graph <- bigram.r_count %>%
  filter(n > 10) %>%
  graph_from_data_frame()

set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(4, "mm"))

nodes.r <- ggraph(bigram.r_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = F, arrow = a, end_cap = circle(1.5, "mm")) +
  geom_node_point(color = "#FF0000", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#DEMOCRATS
sotu.d <- readtext("*Democrat.txt") %>%
  mutate(text = str_replace_all(text, pattern = "\\[.*?\\]", replacement =  "")) %>%
  mutate(text = str_replace_all(text, pattern = "\\(.*?\\)", replacement =  ""))

sotu.d_bigrams <- sotu.d %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

sotu.d_bigrams %>%
  count(bigram, sort = T)

bigrams.d_separated <- sotu.d_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams.d_filtered <- bigrams.d_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram.d_count <- bigrams.d_filtered %>%
  count(word1, word2, sort = T)

#network of bigrams
bigram.d_graph <- bigram.d_count %>%
  filter(n > 10) %>%
  graph_from_data_frame()

set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(4, "mm"))

nodes.d <- ggraph(bigram.d_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = F, arrow = a, end_cap = circle(1.5, "mm")) +
  geom_node_point(color = "#0009FF", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
